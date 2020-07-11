module Test.Go.Featherweight.Syntax exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Go.Featherweight.Syntax exposing (..)
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "module Go.Featherweight.Syntax"
        [ describe "parse"
            [ test "parse sample FG code" <|
                \_ ->
                    Parser.run parse sample
                        |> Expect.equal
                            (Ok <|
                                { decls =
                                    [ TDecl { literal = Interface [], name = "Any" }
                                    , TDecl
                                        { literal =
                                            Interface
                                                [ { name = "Apply", sign = { args = [ ( "x", "Any" ) ], rett = "Any" } }
                                                ]
                                        , name = "Function"
                                        }
                                    , TDecl { literal = Structure [ ( "n", "int" ) ], name = "incr" }
                                    , MDecl
                                        { name = "Apply"
                                        , recv = ( "this", "incr" )
                                        , retv =
                                            MethodCall
                                                { args = [ TypeAssertion { exp = Var "x", ty = "int" } ]
                                                , exp = SelectField { exp = Var "this", field = "n" }
                                                , method = "add"
                                                }
                                        , sign = { args = [ ( "x", "Any" ) ], rett = "Any" }
                                        }
                                    , TDecl { literal = Structure [], name = "pos" }
                                    , MDecl
                                        { name = "Apply"
                                        , recv = ( "this", "pos" )
                                        , retv =
                                            MethodCall
                                                { args = [ Var "zero" ]
                                                , exp = TypeAssertion { exp = Var "x", ty = "int" }
                                                , method = "lt"
                                                }
                                        , sign = { args = [ ( "x", "Any" ) ], rett = "Any" }
                                        }
                                    , TDecl
                                        { literal =
                                            Structure [ ( "f", "Function" ), ( "g", "Function" ) ]
                                        , name = "compose"
                                        }
                                    , MDecl
                                        { name = "Apply"
                                        , recv = ( "this", "compose" )
                                        , retv =
                                            MethodCall
                                                { args =
                                                    [ MethodCall
                                                        { args = [ Var "x" ]
                                                        , exp = SelectField { exp = Var "this", field = "f" }
                                                        , method = "Apply"
                                                        }
                                                    ]
                                                , exp = SelectField { exp = Var "this", field = "g" }
                                                , method = "Apply"
                                                }
                                        , sign = { args = [ ( "x", "Any" ) ], rett = "Any" }
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
                                                            [ StructLiteral { args = [ Var "x" ], struct = "incr" }
                                                            , StructLiteral { args = [], struct = "pos" }
                                                            ]
                                                        , struct = "compose"
                                                        }
                                                , method = "Apply"
                                                }
                                        , ty = "bool"
                                        }
                                }
                            )
            ]
        , describe "parseDecl"
            [ test "parse structure type declaration" <|
                \_ ->
                    Parser.run parseDecl
                        "type hoge struct {\n  hoge1 int\n  hoge2 bool\n}"
                        |> Expect.equal
                            (Ok <|
                                TDecl
                                    { name = "hoge"
                                    , literal = Structure [ ( "hoge1", "int" ), ( "hoge2", "bool" ) ]
                                    }
                            )
            , test "parse interface type declaration" <|
                \_ ->
                    Parser.run parseDecl
                        "type hoge interface {\n  hoge1(x int) int\n  hoge2(x int, y bool) bool\n}"
                        |> Expect.equal
                            (Ok <|
                                TDecl
                                    { name = "hoge"
                                    , literal =
                                        Interface
                                            [ { name = "hoge1"
                                              , sign =
                                                    { args = [ ( "x", "int" ) ]
                                                    , rett = "int"
                                                    }
                                              }
                                            , { name = "hoge2"
                                              , sign =
                                                    { args = [ ( "x", "int" ), ( "y", "bool" ) ]
                                                    , rett = "bool"
                                                    }
                                              }
                                            ]
                                    }
                            )
            , test "parse method declaration" <|
                \_ ->
                    Parser.run parseDecl
                        "func (self int) hoge(x int, y bool) int {return self}"
                        |> Expect.equal
                            (Ok <|
                                MDecl
                                    { name = "hoge"
                                    , recv = ( "self", "int" )
                                    , sign =
                                        { args = [ ( "x", "int" ), ( "y", "bool" ) ]
                                        , rett = "int"
                                        }
                                    , retv = Var "self"
                                    }
                            )
            ]
        , describe "parseExp"
            [ test "parse expression" <|
                \_ ->
                    Parser.run parseExp
                        "A{b}.hoge(x, y).Hoge.(int).Fuga"
                        |> Expect.equal
                            (Ok <|
                                SelectField
                                    { exp =
                                        TypeAssertion
                                            { exp =
                                                SelectField
                                                    { exp =
                                                        MethodCall
                                                            { exp =
                                                                StructLiteral
                                                                    { struct = "A"
                                                                    , args = [ Var "b" ]
                                                                    }
                                                            , method = "hoge"
                                                            , args = [ Var "x", Var "y" ]
                                                            }
                                                    , field = "Hoge"
                                                    }
                                            , ty = "int"
                                            }
                                    , field = "Fuga"
                                    }
                            )
            ]
        ]


sample : String
sample =
    String.join "\n"
        [ "package main"
        , ""
        , "type Any interface {}"
        , "type Function interface {"
        , "    Apply(x Any) Any"
        , "}"
        , ""
        , "type incr struct { n int }"
        , "func (this incr) Apply(x Any) Any {"
        , "    return this.n.add(x.(int))"
        , "}"
        , ""
        , "type pos struct {}"
        , "func (this pos) Apply(x Any) Any {"
        , "    return x.(int).lt(zero)"
        , "}"
        , ""
        , "type compose struct {"
        , "    f Function"
        , "    g Function"
        , "}"
        , "func (this compose) Apply(x Any) Any {"
        , "    return this.g.Apply(this.f.Apply(x))"
        , "}"
        , ""
        , "func main(){"
        , "    _ = compose{incr{x}, pos{}}.Apply(y).(bool)"
        , "}"
        ]
