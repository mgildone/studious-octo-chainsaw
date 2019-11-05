module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, bool, decodeString, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Race =
    { name : String
    , pct : Int
    , id : String
    }


type alias Races =
    List Race


type alias Model =
    { races : Maybe Races
    , race : Race
    , errors : Maybe Http.Error
    }


raceDecoder : Decoder Race
raceDecoder =
    succeed Race
        |> required "name" string
        |> required "pct" int
        |> required "id" string


baseUrl : String
baseUrl =
    "/"


initialModel : Model
initialModel =
    { races = Nothing
    , race =
        { name = "Human"
        , pct = 90
        , id = "human"
        }
    , errors = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, fetchRaces )


fetchRaces : Cmd Msg
fetchRaces =
    Http.get
        { url = baseUrl ++ "data/races.json"
        , expect = Http.expectJson LoadRaces (list raceDecoder)
        }



-- UPDATE


type Msg
    = LoadRaces (Result Http.Error Races)
    | ToggleRace Race


updateRaceById : (Race -> Race) -> String -> Races -> Races
updateRaceById updateRace id races =
    List.map
        (\race ->
            if race.id == id then
                updateRace race

            else
                race
        )
        races


updateRaces : (Race -> Race) -> String -> Maybe Races -> Maybe Races
updateRaces updateRace id maybeRaces =
    Maybe.map (updateRaceById updateRace id) maybeRaces


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadRaces (Ok races) ->
            ( { model | races = Just races }
            , Cmd.none
            )

        LoadRaces (Err error) ->
            ( { model | errors = Just error }, Cmd.none )

        ToggleRace race ->
            ( { model | race = race }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewRace : Race -> Race -> Html Msg
viewRace selectedRace race =
    let
        buttonClass =
            if selectedRace.id == race.id then
                "c-button--unelevated"

            else
                "c-button--raised"
    in
    button [ class buttonClass, onClick (ToggleRace race) ] [ text race.name ]


viewRaces : Maybe Races -> Race -> Html Msg
viewRaces maybeRaces mybeRace =
    case maybeRaces of
        Just races ->
            div []
                [ h3 [] [ text "Select your race" ]
                , div [] (List.map (viewRace mybeRace) races)
                ]

        Nothing ->
            div [ class "loading-feed" ]
                [ text "Loading Races..." ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "c-nav-bar__container" ]
            [ header [ class "c-nav-bar" ]
                [ h1 [ class "c-nav-bar__logo" ] [ text "SOC" ]
                ]
            ]
        , viewRaces model.races model.race
        ]
