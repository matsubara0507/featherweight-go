module Test.Go.Featherweight.Generics.Syntax exposing (suite)

import Expect exposing (Expectation)
import Go.Featherweight.Generics.Syntax exposing (..)
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "module Go.Featherweight.Generics.Syntax"
        [ describe "parser"
            [ test "parse sample FGG code" <|
                \_ ->
                    Parser.run parser sampleFGG
                        |> Expect.equal
                            (Ok
                                { decls =
                                    [ TDecl
                                        { formal =
                                            [ ( "a", Ty ( "Any", [] ) ), ( "b", Ty ( "Any", [] ) ) ]
                                        , literal =
                                            Interface
                                                [ { name = "Apply"
                                                  , sign =
                                                        { args = [ ( "x", Ty ( "a", [] ) ) ]
                                                        , formal = []
                                                        , rett = Ty ( "b", [] )
                                                        }
                                                  }
                                                ]
                                        , name = "Function"
                                        }
                                    , TDecl
                                        { formal = []
                                        , literal = Structure [ ( "n", Ty ( "int", [] ) ) ]
                                        , name = "incr"
                                        }
                                    , MDecl
                                        { formal = []
                                        , name = "Apply"
                                        , recv = ( "this", "incr" )
                                        , retv =
                                            MethodCall
                                                { args = [ Var "x" ]
                                                , exp = SelectField { exp = Var "this", field = "n" }
                                                , method = "add"
                                                , types = []
                                                }
                                        , sign =
                                            { args = [ ( "x", Ty ( "int", [] ) ) ]
                                            , formal = []
                                            , rett = Ty ( "int", [] )
                                            }
                                        }
                                    , TDecl
                                        { formal = []
                                        , literal = Structure []
                                        , name = "pos"
                                        }
                                    , MDecl
                                        { formal = []
                                        , name = "Apply"
                                        , recv = ( "this", "pos" )
                                        , retv =
                                            MethodCall
                                                { args = [ Var "zero" ]
                                                , exp = Var "x"
                                                , method = "lt"
                                                , types = []
                                                }
                                        , sign =
                                            { args = [ ( "x", Ty ( "int", [] ) ) ]
                                            , formal = []
                                            , rett = Ty ( "bool", [] )
                                            }
                                        }
                                    , TDecl
                                        { formal =
                                            [ ( "a", Ty ( "Any", [] ) )
                                            , ( "b", Ty ( "Any", [] ) )
                                            , ( "c", Ty ( "Any", [] ) )
                                            ]
                                        , literal =
                                            Structure
                                                [ ( "f", Ty ( "Function", [ Ty ( "a", [] ), Ty ( "b", [] ) ] ) )
                                                , ( "g", Ty ( "Function", [ Ty ( "b", [] ), Ty ( "c", [] ) ] ) )
                                                ]
                                        , name = "compose"
                                        }
                                    , MDecl
                                        { formal =
                                            [ ( "a", Ty ( "Any", [] ) )
                                            , ( "b", Ty ( "Any", [] ) )
                                            , ( "c", Ty ( "Any", [] ) )
                                            ]
                                        , name = "Apply"
                                        , recv = ( "this", "compose" )
                                        , retv =
                                            MethodCall
                                                { args =
                                                    [ MethodCall
                                                        { args = [ Var "x" ]
                                                        , exp = SelectField { exp = Var "this", field = "f" }
                                                        , method = "Apply"
                                                        , types = []
                                                        }
                                                    ]
                                                , exp = SelectField { exp = Var "this", field = "g" }
                                                , method = "Apply"
                                                , types = []
                                                }
                                        , sign =
                                            { args = [ ( "x", Ty ( "a", [] ) ) ]
                                            , formal = []
                                            , rett = Ty ( "c", [] )
                                            }
                                        }
                                    ]
                                , exp =
                                    MethodCall
                                        { args = [ Var "y" ]
                                        , exp =
                                            StructLiteral
                                                { args =
                                                    [ StructLiteral { args = [ Var "x" ], struct = Ty ( "incr", [] ) }
                                                    , StructLiteral { args = [], struct = Ty ( "pos", [] ) }
                                                    ]
                                                , struct =
                                                    Ty
                                                        ( "compose"
                                                        , [ Ty ( "int", [] )
                                                          , Ty ( "int", [] )
                                                          , Ty ( "bool", [] )
                                                          ]
                                                        )
                                                }
                                        , method = "Apply"
                                        , types = []
                                        }
                                }
                            )
            , test "parse sample FG code" <|
                \_ ->
                    Parser.run parser sampleFG
                        |> Expect.equal
                            (Ok
                                { decls =
                                    [ TDecl
                                        { formal = []
                                        , literal = Interface []
                                        , name = "Any"
                                        }
                                    , TDecl
                                        { formal = []
                                        , literal =
                                            Interface
                                                [ { name = "Apply"
                                                  , sign =
                                                        { args = [ ( "x", Ty ( "Any", [] ) ) ]
                                                        , formal = []
                                                        , rett = Ty ( "Any", [] )
                                                        }
                                                  }
                                                ]
                                        , name = "Function"
                                        }
                                    , TDecl
                                        { formal = []
                                        , literal = Structure [ ( "n", Ty ( "int", [] ) ) ]
                                        , name = "incr"
                                        }
                                    , MDecl
                                        { formal = []
                                        , name = "Apply"
                                        , recv = ( "this", "incr" )
                                        , retv =
                                            MethodCall
                                                { args = [ TypeAssertion { exp = Var "x", ty = Ty ( "int", [] ) } ]
                                                , exp = SelectField { exp = Var "this", field = "n" }
                                                , method = "add"
                                                , types = []
                                                }
                                        , sign =
                                            { args = [ ( "x", Ty ( "Any", [] ) ) ]
                                            , formal = []
                                            , rett = Ty ( "Any", [] )
                                            }
                                        }
                                    , TDecl { formal = [], literal = Structure [], name = "pos" }
                                    , MDecl
                                        { formal = []
                                        , name = "Apply"
                                        , recv = ( "this", "pos" )
                                        , retv =
                                            MethodCall
                                                { args = [ Var "zero" ]
                                                , exp = TypeAssertion { exp = Var "x", ty = Ty ( "int", [] ) }
                                                , method = "lt"
                                                , types = []
                                                }
                                        , sign =
                                            { args = [ ( "x", Ty ( "Any", [] ) ) ]
                                            , formal = []
                                            , rett = Ty ( "Any", [] )
                                            }
                                        }
                                    , TDecl
                                        { formal = []
                                        , literal =
                                            Structure
                                                [ ( "f", Ty ( "Function", [] ) )
                                                , ( "g", Ty ( "Function", [] ) )
                                                ]
                                        , name = "compose"
                                        }
                                    , MDecl
                                        { formal = []
                                        , name = "Apply"
                                        , recv = ( "this", "compose" )
                                        , retv =
                                            MethodCall
                                                { args =
                                                    [ MethodCall
                                                        { args = [ Var "x" ]
                                                        , exp = SelectField { exp = Var "this", field = "f" }
                                                        , method = "Apply"
                                                        , types = []
                                                        }
                                                    ]
                                                , exp = SelectField { exp = Var "this", field = "g" }
                                                , method = "Apply"
                                                , types = []
                                                }
                                        , sign =
                                            { args = [ ( "x", Ty ( "Any", [] ) ) ]
                                            , formal = []
                                            , rett = Ty ( "Any", [] )
                                            }
                                        }
                                    ]
                                , exp =
                                    TypeAssertion
                                        { exp =
                                            MethodCall
                                                { args = [ Var "y" ]
                                                , exp =
                                                    StructLiteral
                                                        { args =
                                                            [ StructLiteral
                                                                { args = [ Var "x" ]
                                                                , struct = Ty ( "incr", [] )
                                                                }
                                                            , StructLiteral
                                                                { args = []
                                                                , struct = Ty ( "pos", [] )
                                                                }
                                                            ]
                                                        , struct = Ty ( "compose", [] )
                                                        }
                                                , method = "Apply"
                                                , types = []
                                                }
                                        , ty = Ty ( "bool", [] )
                                        }
                                }
                            )
            ]
        ]


sampleFGG : String
sampleFGG =
    String.dropLeft 1
        """
package main

type Function(type a Any, b Any) interface {
    Apply(x a) b
}

type incr struct { n int }
func (this incr) Apply(x int) int {
    return this.n.add(x)
}

type pos struct {}
func (this pos) Apply(x int) bool {
    return x.lt(zero)
}

type compose(type a Any, b Any, c Any) struct {
    f Function(a, b)
    g Function(b, c)
}
func (this compose(type a Any, b Any, c Any)) Apply(x a) c {
    return this.g.Apply(this.f.Apply(x))
}

func main(){
    _ = compose(int, int, bool){incr{x}, pos{}}.Apply(y)
}
"""


sampleFG : String
sampleFG =
    String.dropLeft 1
        """
package main

type Any interface {}
type Function interface {
    Apply(x Any) Any
}

type incr struct { n int }
func (this incr) Apply(x Any) Any {
    return this.n.add(x.(int))
}

type pos struct {}
func (this pos) Apply(x Any) Any {
    return x.(int).lt(zero)
}

type compose struct {
    f Function
    g Function
}
func (this compose) Apply(x Any) Any {
    return this.g.Apply(this.f.Apply(x))
}

func main(){
    _ = compose{incr{x}, pos{}}.Apply(y).(bool)
}
"""
