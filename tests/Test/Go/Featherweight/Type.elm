module Test.Go.Featherweight.Type exposing (suite)

import Expect exposing (Expectation)
import Go.Featherweight as FG
import Go.Featherweight.Type exposing (TypeError(..))
import Test exposing (..)


suite : Test
suite =
    describe "module Go.Featherweight.Type"
        [ describe "check"
            [ test "success type check simple code" <|
                \_ ->
                    FG.parse code1
                        |> Result.andThen FG.check
                        |> Expect.equal (Ok ())
            , test "success type check simple code with interface" <|
                \_ ->
                    FG.parse code2
                        |> Result.andThen FG.check
                        |> Expect.equal (Ok ())
            , test "success type check simple code with method declaration" <|
                \_ ->
                    FG.parse code3
                        |> Result.andThen FG.check
                        |> Expect.equal (Ok ())
            , test "fail type check simple code with undefined type" <|
                \_ ->
                    FG.parse failCode1
                        |> Result.andThen FG.check
                        |> Expect.equal
                            (Err <| FG.TypeError <| Undefined "type" "Fuga")
            ]
        ]


code1 : String
code1 =
    String.dropLeft 1
        """
package main

type Hoge struct {}

func main() {
  _ = Hoge{}
}
"""


code2 : String
code2 =
    String.dropLeft 1
        """
package main

type Any interface {}

type Hoge struct { hoge Any }

type Fuga struct {}

func main() {
  _ = Hoge{Fuga{}}.hoge.(Fuga)
}
"""


code3 : String
code3 =
    String.dropLeft 1
        """
package main

type Hoge struct {}

func (self Hoge) hoge(x Hoge, y Hoge) Hoge {
    return x
}

func main() {
  _ = Hoge{}.hoge(Hoge{}, Hoge{}.hoge(Hoge{}, Hoge{}))
}
"""


failCode1 : String
failCode1 =
    String.dropLeft 1
        """
package main

type Hoge struct {}

func (self Hoge) hoge(x Fuga) Hoge {
    return x
}

func main() {
  _ = Hoge{}.hoge(Hoge{})
}
"""
