module Go.Featherweight.Generics exposing
    ( Error(..)
    , Program
    , displayError
    , parse
    )

import Go.Featherweight.Generics.Syntax as FGG
import Go.Parser.Helper as Parser
import Parser exposing (DeadEnd)


type alias Program =
    FGG.Program


type Error
    = ParseError (List DeadEnd)


displayError : Error -> String
displayError err =
    case err of
        ParseError txt ->
            Parser.displayError txt


parse : String -> Result Error Program
parse =
    Result.mapError ParseError << Parser.run FGG.parser
