module Go.Featherweight.Generics.Syntax exposing
    ( Declaration(..)
    , Expression(..)
    , FieldName
    , MethodName
    , MethodSignature
    , MethodSpecific
    , Program
    , Type(..)
    , TypeFormal
    , TypeLiteral(..)
    , TypeName
    , TypeParam
    , VarName
    , declParser
    , expParser
    , parser
    )

import Go.Parser.Helper exposing (..)
import Parser exposing (..)
import Set exposing (Set)


type alias Program =
    { decls : List Declaration, exp : Expression }


type Declaration
    = TDecl
        { name : TypeName
        , formal : TypeFormal
        , literal : TypeLiteral
        }
    | MDecl
        { recv : ( VarName, TypeName )
        , formal : TypeFormal
        , name : MethodName
        , sign : MethodSignature
        , retv : Expression
        }


type Type
    = Ty ( TypeName, List Type )


type alias TypeFormal =
    List ( TypeParam, Type )


type alias TypeParam =
    String


type alias TypeName =
    String


type alias VarName =
    String


type alias MethodName =
    String


type alias FieldName =
    String


type TypeLiteral
    = Structure (List ( FieldName, Type ))
    | Interface (List MethodSpecific)


type alias MethodSpecific =
    { name : MethodName, sign : MethodSignature }


type alias MethodSignature =
    { formal : TypeFormal
    , args : List ( VarName, Type )
    , rett : Type
    }


type Expression
    = Var VarName
    | MethodCall
        { exp : Expression
        , method : MethodName
        , types : List Type
        , args : List Expression
        }
    | StructLiteral
        { struct : Type
        , args : List Expression
        }
    | SelectField
        { exp : Expression
        , field : FieldName
        }
    | TypeAssertion
        { exp : Expression
        , ty : Type
        }


parser : Parser Program
parser =
    let
        parseMainPackage =
            succeed ()
                |. keyword "package"
                |. whitespaces
                |. keyword "main"

        parseMainFunc =
            succeed ()
                |. backtrackable (keyword "func")
                |. backtrackable whitespaces
                |. keyword "main()"

        parseMainExp =
            succeed identity
                |. symbol "_"
                |. whitespaces
                |. symbol "="
                |. whitespaces
                |= expParser
    in
    succeed Program
        |. parseMainPackage
        |. newlines
        |= newlineSequence parseMainFunc declParser
        |. spaces
        |. symbol "{"
        |. spaces
        |= parseMainExp
        |. spaces
        |. symbol "}"
        |. spaces
        |. Parser.end


declParser : Parser Declaration
declParser =
    oneOf
        [ succeed (\name fm lit -> TDecl { name = name, formal = fm, literal = lit })
            |. keyword "type"
            |. whitespaces
            |= nameParser
            |. spaces
            |= formalParser
            |. spaces
            |= typeLitParser
        , succeed (\r f n s v -> MDecl { recv = r, formal = f, name = n, sign = s, retv = v })
            |. keyword "func"
            |. spaces
            |. symbol "("
            |= nameAndTypeNameParser
            |. spaces
            |= formalParser
            |. symbol ")"
            |. spaces
            |= nameParser
            |= methodSignParser
            |. spaces
            |. symbol "{"
            |. spaces
            |. keyword "return"
            |. whitespaces
            |= expParser
            |. spaces
            |. symbol "}"
        ]


formalParser : Parser TypeFormal
formalParser =
    let
        p =
            succeed Tuple.pair
                |= nameParser
                |. whitespaces
                |= typeParser
    in
    oneOf
        [ succeed identity
            |. backtrackable (symbol "(")
            |. spaces
            |= blockWith ( "type", ")" ) p
        , succeed []
        ]


typeParser : Parser Type
typeParser =
    nameParser
        |> Parser.andThen
            (\name ->
                oneOf
                    [ succeed (Tuple.pair name)
                        |= blockWith ( "(", ")" ) typeParser
                    , succeed ( name, [] )
                    ]
            )
        |> Parser.map Ty


typeLitParser : Parser TypeLiteral
typeLitParser =
    oneOf
        [ succeed Structure
            |. keyword "struct"
            |. spaces
            |. symbol "{"
            |. spaces
            |= newlineSequence (symbol "}") nameAndTypeParser
        , succeed Interface
            |. keyword "interface"
            |. spaces
            |. symbol "{"
            |. spaces
            |= newlineSequence (symbol "}") methodSpecificParser
        ]


nameAndTypeParser : Parser ( String, Type )
nameAndTypeParser =
    succeed (\n t -> ( n, t ))
        |= nameParser
        |. whitespaces
        |= typeParser


nameAndTypeNameParser : Parser ( String, TypeName )
nameAndTypeNameParser =
    succeed (\n t -> ( n, t ))
        |= nameParser
        |. whitespaces
        |= nameParser


methodSpecificParser : Parser MethodSpecific
methodSpecificParser =
    succeed MethodSpecific
        |= nameParser
        |= methodSignParser


methodSignParser : Parser MethodSignature
methodSignParser =
    succeed MethodSignature
        |= formalParser
        |. spaces
        |= blockWith ( "(", ")" ) nameAndTypeParser
        |. whitespaces
        |= typeParser


expParser : Parser Expression
expParser =
    nameParser
        |> Parser.andThen expParserWithName
        |> Parser.andThen (\exp -> loop exp stepExpParserWithExp)


expParserWithName : String -> Parser Expression
expParserWithName name =
    oneOf
        [ succeed (Ty << Tuple.pair name)
            |= blockWith ( "(", ")" ) typeParser
            |> Parser.andThen structLiteralParser
        , structLiteralParser (Ty ( name, [] ))
        , succeed (Var name)
        ]


structLiteralParser : Type -> Parser Expression
structLiteralParser ty =
    succeed (\args -> StructLiteral { struct = ty, args = args })
        |= blockWith ( "{", "}" ) (lazy <| \_ -> expParser)


stepExpParserWithExp : Expression -> Parser (Step Expression Expression)
stepExpParserWithExp exp =
    oneOf
        [ succeed Loop
            |. symbol "."
            |= expParserWithExp exp
        , succeed (Done exp)
        ]


expParserWithExp : Expression -> Parser Expression
expParserWithExp exp =
    oneOf
        [ succeed (\ty -> TypeAssertion { exp = exp, ty = ty })
            |. symbol "("
            |= typeParser
            |. symbol ")"
        , nameParser
            |> Parser.andThen (expParserWithExpAndName exp)
        ]


expParserWithExpAndName : Expression -> String -> Parser Expression
expParserWithExpAndName exp name =
    oneOf
        [ succeed (\( ts, xs ) -> MethodCall { exp = exp, method = name, types = ts, args = xs })
            |= oneOf
                [ succeed Tuple.pair
                    |= backtrackable (blockWith ( "(", ")" ) typeParser)
                    |= blockWith ( "(", ")" ) (lazy <| \_ -> expParser)
                , succeed (Tuple.pair [])
                    |= blockWith ( "(", ")" ) (lazy <| \_ -> expParser)
                ]
        , succeed (SelectField { exp = exp, field = name })
        ]


nameParser : Parser String
nameParser =
    variable
        { start = Char.isAlphaNum
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = keywords
        }


keywords : Set String
keywords =
    Set.fromList
        [ "package"
        , "main"
        , "func"
        , "struct"
        , "interface"
        , "type"
        , "return"
        ]
