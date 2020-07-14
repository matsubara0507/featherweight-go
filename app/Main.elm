module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Go.Featherweight as FG
import Html as Html exposing (..)
import Html.Attributes exposing (attribute, class, id, style)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { input : String
    , error : String
    }


isEmpty : Model -> Bool
isEmpty model =
    model.input == ""


isError : Model -> Bool
isError model =
    model.error /= ""


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "", Cmd.none )


type Msg
    = InputText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputText txt ->
            ( checkFG { model | input = txt }, Cmd.none )


checkFG : Model -> Model
checkFG model =
    case Result.andThen FG.check <| FG.parse model.input of
        Ok _ ->
            { model | error = "" }

        Err err ->
            { model | error = FG.displayError err }


view : Model -> Html Msg
view model =
    div
        [ class "my-3 mx-auto container-md" ]
        [ div [ class "Header" ]
            [ div [ class "Header-item Header-item--full" ]
                [ h1 [ class "Header-link" ] [ text "Featherweight Go" ]
                ]
            ]
        , div [] [ viewFormFG model ]
        ]


viewFormFG : Model -> Html Msg
viewFormFG model =
    div
        [ class "form-group"
        , if isEmpty model then
            class ""

          else if isError model then
            class "errored"

          else
            class "successed"
        ]
        [ div [ class "form-group-header" ] [ text "FG code" ]
        , div [ class "form-group-body" ]
            [ textarea
                [ onInput InputText
                , class "form-control"
                , attribute "aria-describedby" "fg-code-validation"
                , attribute "wrap" "off"
                ]
                [ text model.input ]
            ]
        , viewFormValidateFG model
        ]


viewFormValidateFG : Model -> Html msg
viewFormValidateFG model =
    if isEmpty model then
        p [] []

    else if isError model then
        p
            [ class "note error", id "fg-code-validation" ]
            [ text model.error ]

    else
        p
            [ class "note success", id "fg-code-validation" ]
            [ text "OK" ]
