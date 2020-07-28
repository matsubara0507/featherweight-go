module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Go.Featherweight as FG
import Go.Featherweight.Generics as FGG
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
    { inputFGG : String
    , inputFG : String
    , errorFGG : String
    , errorFG : String
    }


isEmptyFGG : Model -> Bool
isEmptyFGG model =
    model.inputFGG == ""


isErrorFGG : Model -> Bool
isErrorFGG model =
    model.errorFGG /= ""


isEmptyFG : Model -> Bool
isEmptyFG model =
    model.inputFG == ""


isErrorFG : Model -> Bool
isErrorFG model =
    model.errorFG /= ""


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" "" "", Cmd.none )


type Msg
    = InputFGG String
    | InputFG String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputFGG txt ->
            ( checkFGG { model | inputFGG = txt }, Cmd.none )

        InputFG txt ->
            ( checkFG { model | inputFG = txt }, Cmd.none )


checkFGG : Model -> Model
checkFGG model =
    case Result.andThen FGG.check <| FGG.parse model.inputFGG of
        Ok _ ->
            { model | errorFGG = "" }

        Err err ->
            { model | errorFGG = FGG.displayError err }


checkFG : Model -> Model
checkFG model =
    case Result.andThen FG.check <| FG.parse model.inputFG of
        Ok _ ->
            { model | errorFG = "" }

        Err err ->
            { model | errorFG = FG.displayError err }


view : Model -> Html Msg
view model =
    div
        [ class "my-3 mx-auto container-md" ]
        [ div [ class "Header" ]
            [ div [ class "Header-item Header-item--full" ]
                [ h1 [ class "Header-link" ] [ text "Featherweight Go" ]
                ]
            ]
        , div [ class "pb-2" ] [ viewFormFG model ]
        , div [] [ viewFormFGG model ]
        ]


viewFormFGG : Model -> Html Msg
viewFormFGG model =
    div
        [ class "form-group"
        , if isEmptyFGG model then
            class ""

          else if isErrorFGG model then
            class "errored"

          else
            class "successed"
        ]
        [ div [ class "form-group-header" ] [ text "FGG code" ]
        , div [ class "form-group-body" ]
            [ textarea
                [ onInput InputFGG
                , class "form-control"
                , attribute "aria-describedby" "fg-code-validation"
                , attribute "wrap" "off"
                ]
                [ text model.inputFGG ]
            ]
        , viewFormValidateFGG model
        ]


viewFormFG : Model -> Html Msg
viewFormFG model =
    div
        [ class "form-group"
        , if isEmptyFG model then
            class ""

          else if isErrorFG model then
            class "errored"

          else
            class "successed"
        ]
        [ div [ class "form-group-header" ] [ text "FG code" ]
        , div [ class "form-group-body" ]
            [ textarea
                [ onInput InputFG
                , class "form-control"
                , attribute "aria-describedby" "fg-code-validation"
                , attribute "wrap" "off"
                ]
                [ text model.inputFG ]
            ]
        , viewFormValidateFG model
        ]


viewFormValidateFGG : Model -> Html msg
viewFormValidateFGG model =
    if isEmptyFGG model then
        p [] []

    else if isErrorFGG model then
        p
            [ class "note error", id "fg-code-validation" ]
            [ text model.errorFGG ]

    else
        p
            [ class "note success", id "fg-code-validation" ]
            [ text "OK" ]


viewFormValidateFG : Model -> Html msg
viewFormValidateFG model =
    if isEmptyFG model then
        p [] []

    else if isErrorFG model then
        p
            [ class "note error", id "fg-code-validation" ]
            [ text model.errorFG ]

    else
        p
            [ class "note success", id "fg-code-validation" ]
            [ text "OK" ]
