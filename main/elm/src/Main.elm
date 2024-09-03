module Main exposing (main)

import Browser
import Html exposing (Html, a, br, button, div, h1, i, input, label, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseDown)
import Html.Events.Extra.Touch exposing (onStart)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Time



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { items : List String, apiKey : String }



-- INIT


init : String -> ( Model, Cmd Msg )
init flags =
    ( Model [ "Milch" ] flags, getItems flags )



-- UPDATE


type Msg
    = FetchItems
    | ItemsReceived (Result Http.Error String)


itemsUrl : String -> String
itemsUrl apiKey =
    "/items/?k=" ++ apiKey


getItems : String -> Cmd Msg
getItems apiKey =
    Http.get { url = itemsUrl apiKey, expect = Http.expectString ItemsReceived }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchItems ->
            ( model, getItems model.apiKey )

        ItemsReceived payload ->
            case payload of
                Ok items ->
                    ( { model | items = String.split "," items }, Cmd.none )

                Err httpError ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text (String.join ", " model.items) ]
