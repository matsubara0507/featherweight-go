module Go.Featherweight.Generics exposing
    ( Error(..)
    , Program
    , check
    , displayError
    , parse
    )

import Go.Featherweight.Generics.Syntax as FGG
import Go.Featherweight.Generics.Type as FGG
import Go.Parser.Helper as Parser
import Parser exposing (DeadEnd)


type alias Program =
    FGG.Program


type Error
    = ParseError (List DeadEnd)
    | TypeError FGG.TypeError


displayError : Error -> String
displayError err =
    case err of
        ParseError txt ->
            Parser.displayError txt

        TypeError e ->
            FGG.displayError e


parse : String -> Result Error Program
parse =
    Result.mapError ParseError << Parser.run FGG.parser


check : Program -> Result Error ()
check =
    Result.mapError TypeError << FGG.check
