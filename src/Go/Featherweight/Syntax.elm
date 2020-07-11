module Go.Featherweight.Syntax exposing
    ( Declaration(..)
    , Expression(..)
    , FieldName
    , MethodName
    , MethodSignature
    , MethodSpecific
    , Program
    , TypeLiteral(..)
    , TypeName
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
        , literal : TypeLiteral
        }
    | MDecl
        { recv : ( VarName, TypeName )
        , name : MethodName
        , sign : MethodSignature
        , retv : Expression
        }


type alias TypeName =
    String


type alias VarName =
    String


type alias MethodName =
    String


type alias FieldName =
    String


type TypeLiteral
    = Structure (List ( FieldName, TypeName ))
    | Interface (List MethodSpecific)


type alias MethodSpecific =
    { name : MethodName, sign : MethodSignature }


type alias MethodSignature =
    { args : List ( VarName, TypeName ), rett : TypeName }


type Expression
    = Var VarName
    | MethodCall
        { exp : Expression
        , method : MethodName
        , args : List Expression
        }
    | StructLiteral
        { struct : TypeName
        , args : List Expression
        }
    | SelectField
        { exp : Expression
        , field : FieldName
        }
    | TypeAssertion
        { exp : Expression
        , ty : TypeName
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
        [ succeed (\name lit -> TDecl { name = name, literal = lit })
            |. keyword "type"
            |. whitespaces
            |= nameParser
            |. whitespaces
            |= typeLitParser
        , succeed (\r n s v -> MDecl { recv = r, name = n, sign = s, retv = v })
            |. keyword "func"
            |. spaces
            |. symbol "("
            |= nameAndTypeParser
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


nameAndTypeParser : Parser ( String, TypeName )
nameAndTypeParser =
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
        |= blockWith ( "(", ")" ) nameAndTypeParser
        |. whitespaces
        |= nameParser


expParser : Parser Expression
expParser =
    nameParser
        |> Parser.andThen expParserWithName
        |> Parser.andThen (\exp -> loop exp stepExpParserWithExp)


expParserWithName : String -> Parser Expression
expParserWithName name =
    oneOf
        [ succeed (\args -> StructLiteral { struct = name, args = args })
            |= blockWith ( "{", "}" ) (lazy <| \_ -> expParser)
        , succeed (Var name)
        ]


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
            |= nameParser
            |. symbol ")"
        , nameParser
            |> Parser.andThen (expParserWithExpAndName exp)
        ]


expParserWithExpAndName : Expression -> String -> Parser Expression
expParserWithExpAndName exp name =
    oneOf
        [ succeed (\args -> MethodCall { exp = exp, method = name, args = args })
            |= blockWith ( "(", ")" ) (lazy <| \_ -> expParser)
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
