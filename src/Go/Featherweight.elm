module Go.Featherweight exposing
    ( Error(..)
    , Program
    , check
    , parse
    )

import Go.Featherweight.Syntax as FG
import Go.Featherweight.Type as FG
import Parser exposing (DeadEnd)


type alias Program =
    FG.Program


type Error
    = ParseError (List DeadEnd)
    | TypeError FG.TypeError


parse : String -> Result Error Program
parse =
    Result.mapError ParseError << Parser.run FG.parser


check : Program -> Result Error ()
check =
    Result.mapError TypeError << FG.check
