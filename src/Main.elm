module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "c-nav-bar__container" ]
            [ header [ class "c-nav-bar" ]
                [ h1 [ class "c-nav-bar__logo" ] [ text "SOC" ]
                ]
            ]
        , div []
            [ button [ onClick Decrement ] [ text "-" ]
            , div [] [ text (String.fromInt model) ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        ]
