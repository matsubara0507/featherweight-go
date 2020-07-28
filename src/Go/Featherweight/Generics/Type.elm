module Go.Featherweight.Generics.Type exposing
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
import Go.Featherweight.Generics.Syntax as FGG exposing (..)
import Result.Extra as Result


type alias Env =
    { gamma : Gamma
    , delta : Delta
    , dmap : DeclMap
    }


type alias Gamma =
    Dict VarName Type


type alias Delta =
    Dict TypeParam Type


type alias Eta =
    Dict TypeParam Type



-- |
-- DeclMap :
-- if defined method but undefined type then item is Nothing


type alias DeclMap =
    Dict TypeName
        { formal : TypeFormal
        , literal : TypeLiteral
        , methods : List MethodSpecific
        }


mkDeclMap : List Declaration -> DeclMap
mkDeclMap decls =
    let
        update decl dmap =
            { dmap | methods = { name = decl.name, sign = decl.sign } :: dmap.methods }
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
                            Just <|
                                case decl.literal of
                                    Structure _ ->
                                        ( decl.name
                                        , { formal = decl.formal
                                          , literal = decl.literal
                                          , methods = []
                                          }
                                        )

                                    Interface methods ->
                                        ( decl.name
                                        , { formal = decl.formal
                                          , literal = decl.literal
                                          , methods = methods
                                          }
                                        )

                        MDecl _ ->
                            Nothing
                )
                decls
        )
        decls


newEnv : List Declaration -> Env
newEnv decls =
    Env Dict.empty Dict.empty (mkDeclMap decls)


type TypeError
    = DuplicatedDefinition String String
    | Undefined String String
    | ExpectStructType TypeName
    | NotSubtype TypeName TypeName
    | UnmatchTypeParams (List TypeName) (List TypeName)
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

        UnmatchTypeParams _ _ ->
            "unmatch type params size"

        ErrorOn val e ->
            displayError e ++ " on '" ++ val ++ "'"


displayExp : FGG.Expression -> String
displayExp exp =
    case exp of
        Var name ->
            name

        MethodCall mcall ->
            displayExp mcall.exp
                ++ "."
                ++ mcall.method
                ++ "("
                ++ (List.map displayExp mcall.args |> String.join ", ")
                ++ ")"

        StructLiteral slit ->
            toTypeName slit.struct
                ++ "{"
                ++ (List.map displayExp slit.args |> String.join ", ")
                ++ "}"

        SelectField sel ->
            displayExp sel.exp ++ "." ++ sel.field

        TypeAssertion ta ->
            displayExp ta.exp ++ ".(" ++ toTypeName ta.ty ++ ")"


check : FGG.Program -> Result TypeError ()
check p =
    let
        env =
            newEnv p.decls
    in
    combine_
        [ distinct (tdecls p.decls)
            |> Result.mapError (DuplicatedDefinition "type")
        , distinct (mdecls p.decls)
            |> Result.mapError (\( x, y ) -> DuplicatedDefinition "method" (x ++ "." ++ y))
        , combine_ (List.map (checkDeclWith env) p.decls)
        , typeInferWith env p.exp
            |> Result.map (always ())
        ]


checkDeclWith : Env -> Declaration -> Result TypeError ()
checkDeclWith env d =
    case d of
        TDecl decl ->
            combine_
                [ checkTypeFormalWith env decl.formal
                , mergePhi env decl.formal
                    |> Result.andThen (\env2 -> checkTypeLitWith env2 decl.literal)
                ]
                |> Result.mapError (ErrorOn decl.name)

        MDecl decl ->
            mergePhi env decl.formal
                |> Result.andThen (\env2 -> mergePhi env2 decl.sign.formal)
                |> Result.andThen
                    (\env2 ->
                        combine_
                            [ decl.sign.args
                                |> List.map Tuple.first
                                |> (::) (Tuple.first decl.recv)
                                |> distinct
                                |> Result.mapError (DuplicatedDefinition "variable")
                            , Dict.get (Tuple.second decl.recv) env2.dmap
                                |> Result.fromMaybe (Undefined "type" <| Tuple.second decl.recv)
                                |> Result.map .formal
                                |> Result.andThen (subformalWith env2 decl.formal)
                                |> Result.mapError (ErrorOn "receive")
                            , checkTypeNameWith env2.dmap (Tuple.second decl.recv)
                            , decl.sign.args
                                |> List.map Tuple.second
                                |> List.map (checkTypeWith env2)
                                |> combine_
                                |> Result.mapError (ErrorOn "args")
                            , checkTypeWith env2 decl.sign.rett
                            , Tuple.mapSecond (mkRecvType decl.formal) decl.recv
                                :: decl.sign.args
                                |> Dict.fromList
                                |> (\g -> typeInferWith { env2 | gamma = g } decl.retv)
                                |> Result.andThen (\t -> subtypeWith env2 t decl.sign.rett)
                            ]
                    )
                |> Result.mapError (ErrorOn <| Tuple.second decl.recv ++ "." ++ decl.name)


mergePhi : Env -> TypeFormal -> Result TypeError Env
mergePhi env formal =
    Ok <|
        { env
            | delta =
                Dict.union env.delta (Dict.fromList formal)
        }


mkRecvType : TypeFormal -> TypeName -> Type
mkRecvType formal name =
    List.map Tuple.first formal
        |> List.map (\a -> Ty ( a, [] ))
        |> Tuple.pair name
        |> Ty


checkTypeFormalWith : Env -> TypeFormal -> Result TypeError ()
checkTypeFormalWith env formal =
    let
        env2 =
            Dict.fromList formal
                |> (\delta -> { env | delta = Dict.union env.delta delta })
    in
    combine_
        [ List.map Tuple.first formal
            |> List.append (Dict.keys env.delta)
            |> distinct
            |> Result.mapError (DuplicatedDefinition "type param")
        , List.map Tuple.second formal
            |> List.map (checkTypeWith env2)
            |> combine_
        ]


checkTypeWith : Env -> Type -> Result TypeError ()
checkTypeWith env (Ty ( name, tyParams )) =
    case ( Dict.get name env.dmap, Dict.member name env.delta, tyParams ) of
        ( Just decl, _, _ ) ->
            combine_
                [ List.map (checkTypeWith env) tyParams
                    |> combine_
                , checkBoundsWith env decl.formal tyParams
                    |> Result.map (\_ -> ())
                ]

        ( _, True, [] ) ->
            Ok ()

        _ ->
            Err (Undefined "type" name)


buildEta : TypeFormal -> List Type -> Result TypeError Eta
buildEta formal actual =
    case ( formal, actual ) of
        ( [], [] ) ->
            Ok Dict.empty

        ( ( a, _ ) :: fs, t :: ts ) ->
            buildEta fs ts
                |> Result.map (Dict.insert a t)

        _ ->
            Err <|
                UnmatchTypeParams
                    (List.map Tuple.first formal)
                    (List.map toTypeName actual)


checkBoundsWith : Env -> TypeFormal -> List Type -> Result TypeError Eta
checkBoundsWith env formal actual =
    buildEta formal actual
        |> Result.andThen
            (\eta ->
                List.map (Tuple.mapBoth (substTypeName eta) (substType eta)) formal
                    |> Result.combineMap (\( a, u ) -> subtypeWith env (Ty ( a, [] )) u)
                    |> Result.map (\_ -> eta)
            )
        |> Result.mapError (ErrorOn "check bounds")


toTypeName : Type -> TypeName
toTypeName (Ty ( name, _ )) =
    name


checkTypeLitWith : Env -> TypeLiteral -> Result TypeError ()
checkTypeLitWith env tlit =
    case tlit of
        Structure fs ->
            Result.map2 (\_ _ -> ())
                (List.map Tuple.first fs
                    |> distinct
                    |> Result.mapError (DuplicatedDefinition "field")
                )
                (combine_ <|
                    List.map
                        (\( name, ty ) ->
                            checkTypeWith env ty
                                |> Result.mapError (ErrorOn name)
                        )
                        fs
                )
                |> Result.mapError (ErrorOn "check struct literal")

        Interface mss ->
            Result.map2 (\_ _ -> ())
                (List.map uniqMethodSpec mss
                    |> distinct
                    |> Result.mapError (\( m, _, _ ) -> DuplicatedDefinition "method" m)
                )
                (List.map (checkMethodSpecWith env) mss
                    |> combine_
                )
                |> Result.mapError (ErrorOn "check interface literal")


checkMethodSpecWith : Env -> MethodSpecific -> Result TypeError ()
checkMethodSpecWith env s =
    distinct (List.map Tuple.first s.sign.args)
        |> Result.mapError (DuplicatedDefinition "variable")
        |> Result.andThen (\_ -> mergePhi env s.sign.formal)
        |> Result.andThen
            (\env2 ->
                s.sign.rett
                    :: List.map Tuple.second s.sign.args
                    |> List.map (checkTypeWith env2)
                    |> combine_
            )
        |> Result.mapError (ErrorOn s.name)


typeInferWith : Env -> Expression -> Result TypeError Type
typeInferWith env exp =
    case exp of
        Var name ->
            Dict.get name env.gamma
                |> Result.fromMaybe (Undefined "variable" name)

        MethodCall mcall ->
            typeInferWith env mcall.exp
                |> Result.andThen (\t -> findMethodSpecific ( t, mcall.method ) env)
                |> Result.andThen
                    (\s ->
                        Result.map3
                            (\eta ts us ->
                                combine_ <|
                                    List.map2 (subtypeWith env)
                                        (List.map (substType eta) ts)
                                        (List.map (substType eta) us)
                            )
                            (checkBoundsWith env s.sign.formal mcall.types)
                            (Result.combineMap (typeInferWith env) mcall.args)
                            (Ok <| List.map Tuple.second s.sign.args)
                            |> Result.join
                            |> Result.map (\_ -> s.sign.rett)
                    )
                |> Result.mapError (ErrorOn <| displayExp mcall.exp)

        StructLiteral slit ->
            fieldsWith env slit.struct
                |> Result.map (List.map Tuple.second)
                |> Result.map3
                    (\_ ts us ->
                        List.map2 (subtypeWith env) ts us
                            |> combine_
                    )
                    (checkTypeWith env slit.struct)
                    (Result.combineMap
                        (typeInferWith env)
                        slit.args
                    )
                |> Result.join
                |> Result.map (\_ -> slit.struct)
                |> Result.mapError (ErrorOn "struct literal")

        SelectField sel ->
            typeInferWith env sel.exp
                |> Result.andThen (\t -> findFieldType ( t, sel.field ) env)
                |> Result.mapError (ErrorOn "select field")

        TypeAssertion ta ->
            Result.map2
                (\_ -> subtypeWith env ta.ty)
                (checkTypeWith env ta.ty)
                (typeInferWith env ta.exp)
                |> Result.join
                |> Result.map (\_ -> ta.ty)


findTypeLiteral : TypeName -> DeclMap -> Result TypeError TypeLiteral
findTypeLiteral t dmap =
    Dict.get t dmap
        |> Maybe.map .literal
        |> Result.fromMaybe (Undefined "type literal" t)


findMethodSpecific : ( Type, MethodName ) -> Env -> Result TypeError MethodSpecific
findMethodSpecific ( t, m ) env =
    methodsWith env t
        |> Result.map (List.head << List.filter (\s -> s.name == m))
        |> Result.map (Result.fromMaybe <| Undefined "method" (toTypeName t ++ "." ++ m))
        |> Result.join
        |> Result.mapError (ErrorOn "find method")


methodsWith : Env -> Type -> Result TypeError (List MethodSpecific)
methodsWith env (Ty ( t, tyParams )) =
    case ( Dict.get t env.delta, Dict.get t env.dmap ) of
        ( Just bound, _ ) ->
            methodsWith env bound

        ( _, Just decl ) ->
            Result.map
                (\eta ->
                    List.map (\s -> { s | sign = substMethodSign eta s.sign }) decl.methods
                )
                (case decl.literal of
                    Structure _ ->
                        checkBoundsWith env decl.formal tyParams

                    Interface _ ->
                        buildEta decl.formal tyParams
                )

        _ ->
            Err (Undefined "type" t)


findFieldType : ( Type, FieldName ) -> Env -> Result TypeError Type
findFieldType ( t, name ) env =
    fieldsWith env t
        |> Result.andThen
            (\fs ->
                case List.head (List.filter (\( f, _ ) -> f == name) fs) of
                    Just ( _, ty ) ->
                        Ok ty

                    Nothing ->
                        Err (ErrorOn (toTypeName t) <| Undefined "field" name)
            )


fieldsWith : Env -> Type -> Result TypeError (List ( FieldName, Type ))
fieldsWith env (Ty ( t, tyParams )) =
    Dict.get t env.dmap
        |> Result.fromMaybe (Undefined "type" t)
        |> Result.andThen
            (\decl ->
                case decl.literal of
                    Structure fs ->
                        buildEta decl.formal tyParams
                            |> Result.map
                                (\eta ->
                                    List.map (Tuple.mapSecond <| substType eta) fs
                                )

                    Interface _ ->
                        Err (ExpectStructType t)
            )


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


subformalWith : Env -> TypeFormal -> TypeFormal -> Result TypeError ()
subformalWith env formal1 formal2 =
    case ( formal1, formal2 ) of
        ( [], [] ) ->
            Ok ()

        ( ( _, t ) :: fs, ( _, u ) :: hs ) ->
            combine_
                [ subtypeWith env t u
                , subformalWith env fs hs
                ]

        _ ->
            Err <|
                UnmatchTypeParams
                    (List.map Tuple.first formal1)
                    (List.map Tuple.first formal2)


subtypeWith : Env -> Type -> Type -> Result TypeError ()
subtypeWith env t u =
    let
        ( tname, uname ) =
            ( toTypeName t, toTypeName u )

        err =
            Err (NotSubtype tname uname)
    in
    case ( Dict.member uname env.delta, findTypeLiteral uname env.dmap ) of
        ( True, _ ) ->
            if tname == uname then
                Ok ()

            else
                err

        ( _, Ok (Structure _) ) ->
            if tname == uname then
                Ok ()

            else
                err

        ( _, Ok (Interface _) ) ->
            Result.map2
                (\tms ums ->
                    if List.all (\m -> List.member m tms) ums then
                        Ok ()

                    else
                        err
                )
                (Result.map (List.map uniqMethodSpec) <| methodsWith env t)
                (Result.map (List.map uniqMethodSpec) <| methodsWith env u)
                |> Result.join

        ( _, Err err2 ) ->
            Err err2


substTypeName : Eta -> TypeName -> TypeName
substTypeName eta t =
    Dict.get t eta
        |> Maybe.map toTypeName
        |> Maybe.withDefault t


substType : Eta -> Type -> Type
substType eta (Ty ( t, tyParams )) =
    Ty ( substTypeName eta t, List.map (substType eta) tyParams )


substMethodSign : Eta -> MethodSignature -> MethodSignature
substMethodSign eta s =
    { formal = List.map (Tuple.mapBoth (substTypeName eta) (substType eta)) s.formal
    , args = List.map (Tuple.mapSecond (substType eta)) s.args
    , rett = substType eta s.rett
    }


uniqMethodSpec : MethodSpecific -> ( MethodName, List TypeName, TypeName )
uniqMethodSpec s =
    ( s.name
    , List.map (toTypeName << Tuple.second) s.sign.args
    , toTypeName s.sign.rett
    )


combine_ : List (Result e a) -> Result e ()
combine_ =
    Result.map (always ()) << Result.combine
