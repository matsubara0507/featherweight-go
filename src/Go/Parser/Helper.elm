module Go.Parser.Helper exposing
    ( blockWith
    , newlineSequence
    , newlines
    , whitespaces
    )

import Parser exposing (..)


newlineSequence : Parser () -> Parser a -> Parser (List a)
newlineSequence end p =
    let
        go s =
            succeed identity
                |. chompWhile (\c -> c == ' ')
                |= oneOf
                    [ succeed (Done s)
                        |. backtrackable spaces
                        |. end
                    , succeed (\d -> Loop <| d :: s)
                        |. newlines
                        |= p
                    ]
    in
    succeed identity
        |. spaces
        |= oneOf
            [ succeed []
                |. end
            , p
                |> Parser.andThen (\x -> loop [ x ] go)
                |> Parser.map List.reverse
            ]


blockWith : ( String, String ) -> Parser a -> Parser (List a)
blockWith ( start, end ) p =
    Parser.sequence
        { start = start
        , separator = ","
        , end = end
        , spaces = spaces
        , item = p
        , trailing = Optional
        }


whitespaces : Parser ()
whitespaces =
    succeed ()
        |. symbol " "
        |. chompWhile (\c -> c == ' ')


newlines : Parser ()
newlines =
    succeed ()
        |. oneOf [ symbol "\n", symbol "\u{000D}" ]
        |. spaces
