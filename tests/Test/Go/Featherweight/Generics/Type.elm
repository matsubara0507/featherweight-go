module Test.Go.Featherweight.Generics.Type exposing (suite)

import Expect exposing (Expectation)
import Go.Featherweight.Generics as FGG
import Go.Featherweight.Generics.Type exposing (TypeError(..))
import Test exposing (..)


suite : Test
suite =
    describe "module Go.Featherweight.Generics.Type"
        [ describe "check"
            [ test "success type check simple FG code" <|
                \_ ->
                    FGG.parse codeFG1
                        |> Result.andThen FGG.check
                        |> Expect.equal (Ok ())
            , test "success type check simple FG code with interface" <|
                \_ ->
                    FGG.parse codeFG2
                        |> Result.andThen FGG.check
                        |> Expect.equal (Ok ())
            , test "success type check simple FG code with method declaration" <|
                \_ ->
                    FGG.parse codeFG3
                        |> Result.andThen FGG.check
                        |> Expect.equal (Ok ())
            , test "success type check simple FG code with interface method" <|
                \_ ->
                    FGG.parse codeFG4
                        |> Result.andThen FGG.check
                        |> Expect.equal (Ok ())
            , test "fail type check simple FG code with undefined type" <|
                \_ ->
                    FGG.parse failCodeFG1
                        |> Result.andThen FGG.check
                        |> Expect.equal
                            (Err <|
                                FGG.TypeError <|
                                    ErrorOn "Hoge.hoge" <|
                                        ErrorOn "args" <|
                                            Undefined "type" "Fuga"
                            )
            , test "fail type check simple FG code with undefined interface method" <|
                \_ ->
                    FGG.parse failCodeFG2
                        |> Result.andThen FGG.check
                        |> Expect.equal
                            (Err <|
                                FGG.TypeError <|
                                    ErrorOn "Fuga{Hoge{}}.piyo" <|
                                        ErrorOn "select field" <|
                                            ErrorOn "struct literal" <|
                                                NotSubtype "Hoge" "Piyo"
                            )
            , test "success type check simple FGG code with interface" <|
                \_ ->
                    FGG.parse codeFGG1
                        |> Result.andThen FGG.check
                        |> Expect.equal (Ok ())
            , test "success type check simple FGG code with compose interface" <|
                \_ ->
                    FGG.parse codeFGG2
                        |> Result.andThen FGG.check
                        |> Expect.equal (Ok ())
            , test "success type check simple FGG code with Maybe" <|
                \_ ->
                    FGG.parse codeFGG3
                        |> Result.andThen FGG.check
                        |> Expect.equal (Ok ())
            , test "success type check simple FGG code with Eq" <|
                \_ ->
                    FGG.parse codeFGG4
                        |> Result.andThen FGG.check
                        |> Expect.equal (Ok ())
            , test "success type check simple FGG code with Number" <|
                \_ ->
                    FGG.parse codeFGG5
                        |> Result.andThen FGG.check
                        |> Expect.equal (Ok ())
            , test "fail type check simple FGG code with Eq" <|
                \_ ->
                    FGG.parse failCodeFGG1
                        |> Result.andThen FGG.check
                        |> Expect.equal
                            (Err <|
                                FGG.TypeError <|
                                    ErrorOn "struct literal" <|
                                        ErrorOn "check bounds" <|
                                            NotSubtype "Hoge" "Eq"
                            )
            ]
        ]


codeFG1 : String
codeFG1 =
    String.dropLeft 1
        """
package main

type Hoge struct {}

func main() {
  _ = Hoge{}
}
"""


codeFG2 : String
codeFG2 =
    String.dropLeft 1
        """
package main

type Any interface {}

type Hoge struct { hoge Any }

type Fuga struct {}

func (self Hoge) hogege(other Any) Any {
    return self.hoge
}

func main() {
  _ = Hoge{Fuga{}}.hogege(Fuga{}).(Fuga)
}
"""


codeFG3 : String
codeFG3 =
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


codeFG4 : String
codeFG4 =
    String.dropLeft 1
        """
package main

type Any interface {}

type Piyo interface {
    Piyoyo(x Any) Any
}

type Hoge struct {}

func (self Hoge) Piyoyo(x Any) Any {
    return x
}

type Fuga struct {
    piyo Piyo
}

func main() {
  _ = Fuga{Hoge{}}.piyo.Piyoyo(Hoge{})
}
"""


failCodeFG1 : String
failCodeFG1 =
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


failCodeFG2 : String
failCodeFG2 =
    String.dropLeft 1
        """
package main

type Any interface {}

type Piyo interface {
    Piyoyo(x Any) Any
}

type Hoge struct {}

type Fuga struct {
    piyo Piyo
}

func main() {
  _ = Fuga{Hoge{}}.piyo.Piyoyo(Hoge{})
}
"""


codeFGG1 : String
codeFGG1 =
    String.dropLeft 1
        """
package main

type Any interface {}

type Hoge(type a Any) struct { val a }

type Fuga struct {}

func main() {
  _ = Hoge(Fuga){Fuga{}}.val
}
"""


codeFGG2 : String
codeFGG2 =
    String.dropLeft 1
        """
package main

type Any interface {}

type Function(type a Any, b Any) interface {
    Apply(x a) b
}

type compose(type a Any, b Any, c Any) struct {
    f Function(a, b)
    g Function(b, c)
}
func (this compose(type a Any, b Any, c Any)) Apply(x a) c {
    return this.g.Apply(this.f.Apply(x))
}

type Const(type a Any, b Any) struct { always a }
func (self Const(type a Any, b Any)) Apply(val b) a {
    return self.always
}

type Hoge struct {}
type Fuga struct {}

func main() {
  _ = compose(Hoge, Fuga, Hoge){Const(Fuga, Hoge){Fuga{}}, Const(Hoge, Fuga){Hoge{}}}.Apply(Hoge{})
}
"""


codeFGG3 : String
codeFGG3 =
    String.dropLeft 1
        """
package main

type Any interface {}

type Function(type a Any, b Any) interface {
    Apply(x a) b
}

type Maybe(type a Any) interface {
    Map(type b Any)(f Function(a, b)) Maybe(b)
}

type Nothing(type a Any) struct {}
type Just(type a Any) struct { value a }

func (this Nothing(type a Any)) Map(type b Any)(f Function(a, b)) Maybe(b) {
    return Nothing(b){}
}
func (this Just(type a Any)) Map(type b Any)(f Function(a, b)) Maybe(b) {
    return Just(b){f.Apply(this.value)}
}

type Hoge struct {}
type Fuga struct {}

type Const(type a Any, b Any) struct { always a }
func (self Const(type a Any, b Any)) Apply(val b) a {
    return self.always
}

func main() {
    _ = Just(Hoge){Hoge{}}.Map(Fuga)(Const(Fuga, Hoge){Fuga{}})
}
"""


codeFGG4 : String
codeFGG4 =
    String.dropLeft 1
        """
package main

type Any interface {}

type Eq(type a Eq(a)) interface {
    Equal(that a) Bool
}

type Bool interface {
    Not() Bool
    Equal(that Bool) Bool
}

type TT struct {}
type FF struct {}

func (this TT) Not() Bool { return FF{} }
func (this FF) Not() Bool { return TT{} }

func (this TT) Equal(that Bool) Bool { return that }
func (this FF) Equal(that Bool) Bool { return that.Not() }

type Hoge struct {}
type Fuga struct {}
type Piyo(type a Eq(a)) struct { piyo a }

func (this Hoge) Equal(that Hoge) Bool { return TT{} }

func main() {
    _ = Piyo(Hoge){Hoge{}}
}
"""


codeFGG5 : String
codeFGG5 =
    String.dropLeft 1
        """
package main

type Any interface {}

type Eq(type a Eq(a)) interface {
    Equal(that a) Bool
}

type Bool interface {
    Not() Bool
    And(that Bool) Bool
    Equal(that Bool) Bool
}

type TT struct {}
type FF struct {}

func (this TT) Not() Bool { return FF{} }
func (this FF) Not() Bool { return TT{} }

func (this TT) Equal(that Bool) Bool { return that }
func (this FF) Equal(that Bool) Bool { return that.Not() }

func (this TT) And(that Bool) Bool { return that }
func (this FF) And(that Bool) Bool { return this }

type Number interface {
    IsZero() Bool
    Decr() Number
    Plus(that Number) Number
    Equal(that Number) Bool
}

type Zero struct {}
type Succ struct { Pred Number }

func (this Zero) IsZero() Bool { return TT{} }
func (this Succ) IsZero() Bool { return FF{} }

func (this Zero) Decr() Number {
    return this
}
func (this Succ) Decr() Number {
    return this.Pred
}

func (this Zero) Plus(that Number) Number {
    return that
}
func (this Succ) Plus(that Number) Number {
    return Succ{this.Pred.Plus(that)}
}

func (this Zero) Equal(that Number) Bool {
    return that.IsZero()
}
func (this Succ) Equal(that Number) Bool {
    return that.IsZero().Not().And(this.Pred.Equal(that.Decr()))
}

func main() {
    _ = Succ{Succ{Zero{}}}.Plus(Succ{Zero{}}).Equal(Succ{Zero{}}).Not()
}
"""


failCodeFGG1 : String
failCodeFGG1 =
    String.dropLeft 1
        """
package main

type Any interface {}

type Eq(type a Eq(a)) interface {
    Equal(that a) Bool
}

type Bool interface {
    Not() Bool
    Equal(that Bool) Bool
}

type TT struct {}
type FF struct {}

func (this TT) Not() Bool { return FF{} }
func (this FF) Not() Bool { return TT{} }

func (this TT) Equal(that Bool) Bool { return that }
func (this FF) Equal(that Bool) Bool { return that.Not() }

type Hoge struct {}
type Fuga struct {}
type Piyo(type a Eq(a)) struct { piyo a }

func (this Hoge) Equal(that Bool) Bool { return TT{} }

func main() {
    _ = Piyo(Hoge){Hoge{}}
}
"""
