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
    , parse
    , parseDecl
    , parseExp
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


parse : Parser Program
parse =
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
                |= parseExp
    in
    succeed Program
        |. parseMainPackage
        |. newlines
        |= newlineSequence parseMainFunc parseDecl
        |. spaces
        |. symbol "{"
        |. spaces
        |= parseMainExp
        |. spaces
        |. symbol "}"
        |. Parser.end


parseDecl : Parser Declaration
parseDecl =
    oneOf
        [ succeed (\name lit -> TDecl { name = name, literal = lit })
            |. keyword "type"
            |. whitespaces
            |= parseName
            |. whitespaces
            |= parseTypeLit
        , succeed (\r n s v -> MDecl { recv = r, name = n, sign = s, retv = v })
            |. keyword "func"
            |. spaces
            |. symbol "("
            |= parseNameAndType
            |. symbol ")"
            |. spaces
            |= parseName
            |= parseMethodSign
            |. spaces
            |. symbol "{"
            |. spaces
            |. keyword "return"
            |. whitespaces
            |= parseExp
            |. spaces
            |. symbol "}"
        ]


parseTypeLit : Parser TypeLiteral
parseTypeLit =
    oneOf
        [ succeed Structure
            |. keyword "struct"
            |. spaces
            |. symbol "{"
            |. spaces
            |= newlineSequence (symbol "}") parseNameAndType
        , succeed Interface
            |. keyword "interface"
            |. spaces
            |. symbol "{"
            |. spaces
            |= newlineSequence (symbol "}") parseMethodSpecific
        ]


parseNameAndType : Parser ( String, TypeName )
parseNameAndType =
    succeed (\n t -> ( n, t ))
        |= parseName
        |. whitespaces
        |= parseName


parseMethodSpecific : Parser MethodSpecific
parseMethodSpecific =
    succeed MethodSpecific
        |= parseName
        |= parseMethodSign


parseMethodSign : Parser MethodSignature
parseMethodSign =
    succeed MethodSignature
        |= blockWith ( "(", ")" ) parseNameAndType
        |. whitespaces
        |= parseName


parseExp : Parser Expression
parseExp =
    parseName
        |> Parser.andThen parseExpWithName
        |> Parser.andThen (\exp -> loop exp stepParseExpWithExp)


parseExpWithName : String -> Parser Expression
parseExpWithName name =
    oneOf
        [ succeed (\args -> StructLiteral { struct = name, args = args })
            |= blockWith ( "{", "}" ) (lazy <| \_ -> parseExp)
        , succeed (Var name)
        ]


stepParseExpWithExp : Expression -> Parser (Step Expression Expression)
stepParseExpWithExp exp =
    oneOf
        [ succeed Loop
            |. symbol "."
            |= parseExpWithExp exp
        , succeed (Done exp)
        ]


parseExpWithExp : Expression -> Parser Expression
parseExpWithExp exp =
    oneOf
        [ succeed (\ty -> TypeAssertion { exp = exp, ty = ty })
            |. symbol "("
            |= parseName
            |. symbol ")"
        , parseName
            |> Parser.andThen (parseExpWithExpAndName exp)
        ]


parseExpWithExpAndName : Expression -> String -> Parser Expression
parseExpWithExpAndName exp name =
    oneOf
        [ succeed (\args -> MethodCall { exp = exp, method = name, args = args })
            |= blockWith ( "(", ")" ) (lazy <| \_ -> parseExp)
        , succeed (SelectField { exp = exp, field = name })
        ]


parseName : Parser String
parseName =
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
