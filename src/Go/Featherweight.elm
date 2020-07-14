module Go.Featherweight exposing
    ( Error(..)
    , Program
    , check
    , displayError
    , parse
    )

import Go.Featherweight.Syntax as FG
import Go.Featherweight.Type as FG
import Go.Parser.Helper as Parser
import Parser exposing (DeadEnd)


type alias Program =
    FG.Program


type Error
    = ParseError (List DeadEnd)
    | TypeError FG.TypeError


displayError : Error -> String
displayError err =
    case err of
        ParseError txt ->
            Parser.displayError txt

        TypeError e ->
            FG.displayError e


parse : String -> Result Error Program
parse =
    Result.mapError ParseError << Parser.run FG.parser


check : Program -> Result Error ()
check =
    Result.mapError TypeError << FG.check
