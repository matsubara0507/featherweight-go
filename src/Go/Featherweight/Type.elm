module Go.Featherweight.Type exposing
    ( TypeError(..)
    , check
    , checkDeclWith
    , displayError
    , distinct
    , mdecls
    , tdecls
    , typeInferWith
    )

import Dict exposing (Dict)
import Go.Featherweight.Syntax as FG exposing (..)
import Result.Extra as Result


type alias Env =
    ( Gamma, DeclMap )


type alias Gamma =
    Dict VarName TypeName



-- |
-- DeclMap :
-- if defined method but undefined type then item is Nothing


type alias DeclMap =
    Dict TypeName ( TypeLiteral, List MethodSpecific )


mkDeclMap : List Declaration -> DeclMap
mkDeclMap decls =
    let
        update decl ( tlit, mss ) =
            ( tlit, { name = decl.name, sign = decl.sign } :: mss )
    in
    List.foldl
        (\d ->
            case d of
                TDecl _ ->
                    identity

                MDecl decl ->
                    Dict.update (Tuple.second decl.recv) (Maybe.map <| update decl)
        )
        (Dict.fromList <|
            List.filterMap
                (\t ->
                    case t of
                        TDecl decl ->
                            Just ( decl.name, ( decl.literal, [] ) )

                        MDecl _ ->
                            Nothing
                )
                decls
        )
        decls


newEnv : List Declaration -> Env
newEnv decls =
    ( Dict.empty, mkDeclMap decls )


type TypeError
    = DuplicatedDefinition String String
    | Undefined String String
    | ExpectStructType TypeName
    | NotSubtype TypeName TypeName
    | ErrorOn String TypeError


displayError : TypeError -> String
displayError err =
    case err of
        DuplicatedDefinition key val ->
            "duplicated " ++ key ++ " '" ++ val ++ "'"

        Undefined key val ->
            "undefined " ++ key ++ " '" ++ val ++ "'"

        ExpectStructType t ->
            "type '" ++ t ++ "' is interface, but expected to structure"

        NotSubtype t u ->
            "type '" ++ t ++ "' is not subtype of '" ++ u ++ "'"

        ErrorOn val e ->
            displayError e ++ " on '" ++ val ++ "'"


check : FG.Program -> Result TypeError ()
check p =
    let
        env =
            newEnv p.decls

        dmap =
            Tuple.second env
    in
    combine_
        [ distinct (tdecls p.decls)
            |> Result.mapError (DuplicatedDefinition "type")
        , distinct (mdecls p.decls)
            |> Result.mapError (\( x, y ) -> DuplicatedDefinition "method" (x ++ "." ++ y))
        , combine_ (List.map (checkDeclWith dmap) p.decls)
        , typeInferWith (newEnv p.decls) p.exp
            |> Result.map (always ())
        ]


checkDeclWith : DeclMap -> Declaration -> Result TypeError ()
checkDeclWith dmap d =
    case d of
        TDecl decl ->
            checkTypeLitWith dmap decl.literal
                |> Result.mapError (ErrorOn decl.name)

        MDecl decl ->
            combine_
                [ decl.sign.args
                    |> List.map Tuple.first
                    |> (::) (Tuple.first decl.recv)
                    |> distinct
                    |> Result.mapError (DuplicatedDefinition "variable")
                , checkTypeNameWith dmap (Tuple.second decl.recv)
                , decl.sign.args
                    |> List.map Tuple.second
                    |> List.map (checkTypeNameWith dmap)
                    |> combine_
                , checkTypeNameWith dmap decl.sign.rett
                ]


checkTypeLitWith : DeclMap -> TypeLiteral -> Result TypeError ()
checkTypeLitWith dmap tlit =
    case tlit of
        Structure fs ->
            Result.map2 (\_ _ -> ())
                (List.map Tuple.first fs
                    |> distinct
                    |> Result.mapError (DuplicatedDefinition "field")
                )
                (List.map Tuple.second fs
                    |> List.map (checkTypeNameWith dmap)
                    |> combine_
                )

        Interface mss ->
            Result.map2 (\_ _ -> ())
                (List.map uniqMethodSpec mss
                    |> distinct
                    |> Result.mapError (\( m, _, _ ) -> DuplicatedDefinition "method" m)
                )
                (List.map (checkMethodSpecWith dmap) mss
                    |> combine_
                )


checkMethodSpecWith : DeclMap -> MethodSpecific -> Result TypeError ()
checkMethodSpecWith dmap s =
    Result.map3 (\_ _ _ -> ())
        (List.map Tuple.first s.sign.args
            |> distinct
            |> Result.mapError (ErrorOn s.name << DuplicatedDefinition "variable")
        )
        (List.map Tuple.second s.sign.args
            |> List.map (checkTypeNameWith dmap)
            |> combine_
        )
        (checkTypeNameWith dmap s.sign.rett)


typeInferWith : Env -> Expression -> Result TypeError TypeName
typeInferWith env exp =
    let
        ( gamma, dmap ) =
            env
    in
    case exp of
        Var name ->
            Dict.get name gamma
                |> Maybe.map Ok
                |> Maybe.withDefault (Err <| Undefined "variable" name)

        MethodCall mcall ->
            typeInferWith env mcall.exp
                |> Result.andThen (\t -> findMethodSpecific ( t, mcall.method ) dmap)
                |> Result.andThen
                    (\s ->
                        Result.map2
                            (\ts -> combine_ << List.map2 (subtypeWith dmap) ts)
                            (Result.combineMap (typeInferWith env) mcall.args)
                            (Ok <| List.map Tuple.second s.sign.args)
                            |> Result.join
                            |> Result.map (\_ -> s.sign.rett)
                    )

        StructLiteral slit ->
            findTypeLiteral slit.struct dmap
                |> Result.andThen
                    (Result.map (List.map Tuple.second) << fields slit.struct)
                |> Result.map3
                    (\_ ts -> combine_ << List.map2 (subtypeWith dmap) ts)
                    (checkTypeNameWith dmap slit.struct)
                    (Result.combineMap (typeInferWith env) slit.args)
                |> Result.map (\_ -> slit.struct)

        SelectField sel ->
            typeInferWith env sel.exp
                |> Result.andThen
                    (\t ->
                        findTypeLiteral t dmap
                            |> Result.andThen (\lit -> findFieldTypeOn ( t, lit ) sel.field)
                    )

        TypeAssertion ta ->
            Result.map2 (\_ -> subtypeWith dmap ta.ty)
                (checkTypeNameWith dmap ta.ty)
                (typeInferWith env ta.exp)
                |> Result.join
                |> Result.map (\_ -> ta.ty)


findTypeLiteral : TypeName -> DeclMap -> Result TypeError TypeLiteral
findTypeLiteral t dmap =
    Dict.get t dmap
        |> Maybe.map (Ok << Tuple.first)
        |> Maybe.withDefault (Err <| Undefined "type" t)


findMethodSpecific : ( TypeName, MethodName ) -> DeclMap -> Result TypeError MethodSpecific
findMethodSpecific ( t, m ) dmap =
    Dict.get t dmap
        |> Maybe.map Tuple.second
        |> Maybe.map (List.filter <| \s -> s.name == m)
        |> Maybe.andThen List.head
        |> Maybe.map Ok
        |> Maybe.withDefault (Err <| Undefined "method" (t ++ "." ++ m))


findFieldTypeOn : ( TypeName, TypeLiteral ) -> FieldName -> Result TypeError TypeName
findFieldTypeOn ( t, tlit ) name =
    fields t tlit
        |> Result.andThen
            (\fs ->
                case List.head (List.filter (\( f, _ ) -> f == name) fs) of
                    Just ( _, ty ) ->
                        Ok ty

                    Nothing ->
                        Err (ErrorOn t <| Undefined "field" name)
            )


fields : TypeName -> TypeLiteral -> Result TypeError (List ( FieldName, TypeName ))
fields t tlit =
    case tlit of
        Structure fs ->
            Ok fs

        Interface _ ->
            Err (ExpectStructType t)


checkTypeNameWith : DeclMap -> TypeName -> Result TypeError ()
checkTypeNameWith dmap t =
    findTypeLiteral t dmap |> Result.map (always ())


tdecls : List Declaration -> List TypeName
tdecls =
    List.filterMap <|
        \x ->
            case x of
                TDecl decl ->
                    Just decl.name

                _ ->
                    Nothing


mdecls : List Declaration -> List ( TypeName, MethodName )
mdecls =
    List.filterMap <|
        \x ->
            case x of
                MDecl decl ->
                    Just ( Tuple.second decl.recv, decl.name )

                _ ->
                    Nothing


distinct : List comparable -> Result comparable ()
distinct xs =
    findDupItem Dict.empty xs
        |> Maybe.map Err
        |> Maybe.withDefault (Ok ())


findDupItem : Dict comparable () -> List comparable -> Maybe comparable
findDupItem cache xs =
    case xs of
        [] ->
            Nothing

        y :: ys ->
            if Dict.member y cache then
                Just y

            else
                findDupItem (Dict.insert y () cache) ys


subtypeWith : DeclMap -> TypeName -> TypeName -> Result TypeError ()
subtypeWith dmap t u =
    let
        err =
            Err (NotSubtype t u)
    in
    findTypeLiteral u dmap
        |> Result.andThen
            (\tlit ->
                case tlit of
                    Structure _ ->
                        if t == u then
                            Ok ()

                        else
                            err

                    Interface _ ->
                        Maybe.map2
                            (\ms ns ->
                                if List.all (\m -> List.member m ms) ns then
                                    Ok ()

                                else
                                    err
                            )
                            (Dict.get t dmap
                                |> Maybe.map Tuple.second
                                |> Maybe.map (List.map uniqMethodSpec)
                            )
                            (Dict.get u dmap
                                |> Maybe.map Tuple.second
                                |> Maybe.map (List.map uniqMethodSpec)
                            )
                            |> Maybe.withDefault err
            )


uniqMethodSpec : MethodSpecific -> ( MethodName, List TypeName, TypeName )
uniqMethodSpec s =
    ( s.name, List.map Tuple.second s.sign.args, s.sign.rett )


combine_ : List (Result e a) -> Result e ()
combine_ =
    Result.map (always ()) << Result.combine
