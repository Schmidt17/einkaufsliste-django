module Main exposing (main)

import Browser
import Html exposing (Html, a, br, button, div, h1, header, i, input, label, main_, nav, p, span, text)
import Html.Attributes exposing (..)
import Html.Attributes.Aria as Aria
import Html.Events exposing (onClick, onInput, onMouseDown, preventDefaultOn)
import Html.Events.Extra.Mouse exposing (onWithOptions)
import Html.Events.Extra.Touch exposing (onStart)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Set
import Time



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { items : List ItemData
    , filterTags : List FilterTag
    , apiKey : String
    }


type alias FilterTag =
    { tag : String
    , isActive : Bool
    }


type alias ItemData =
    { id : String
    , title : String
    , tags : List String
    , done : Int
    , orderIndex : Int
    }



-- INIT


init : String -> ( Model, Cmd Msg )
init flags =
    ( Model [] [] flags, getItems flags )



-- UPDATE


type Msg
    = FetchItems
    | ItemsReceived (Result Http.Error String)
    | EditCard
    | FilterClicked String
    | CardClicked String


itemsUrl : String -> String
itemsUrl apiKey =
    "/items/?k=" ++ apiKey


getItems : String -> Cmd Msg
getItems apiKey =
    Http.get { url = itemsUrl apiKey, expect = Http.expectString ItemsReceived }


initTags : List String -> List FilterTag
initTags tagNames =
    List.map (\tag -> FilterTag tag False) tagNames


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchItems ->
            ( model, getItems model.apiKey )

        ItemsReceived payload ->
            case payload of
                Ok rawString ->
                    let
                        items =
                            List.indexedMap receivedToItem (parseItems rawString)
                    in
                    ( { model | items = items, filterTags = initTags ([ "No tags" ] ++ Set.toList (uniqueTags items)) }, Cmd.none )

                Err httpError ->
                    ( model, Cmd.none )

        FilterClicked tag ->
            ( { model | filterTags = toggleTag tag model.filterTags }, Cmd.none )

        CardClicked itemId ->
            ( { model | items = toggleDone itemId model.items }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ headerView model
        , main_ [ Aria.ariaLabel "Listenbereich" ]
            [ div [ class "container" ] (List.map itemCard (itemsToShow model))
            ]
        ]


itemsToShow : Model -> List ItemData
itemsToShow model =
    sortItems (List.filter (isVisible model) model.items)


headerView : Model -> Html Msg
headerView model =
    header [ Aria.ariaLabel "Filterbereich" ]
        [ nav [ Aria.ariaLabel "Header", style "height" "auto" ]
            [ div [ class "nav-wrapper", style "display" "flex" ]
                [ div [ class "chips-wrapper filter-chips", Aria.ariaLabel "Filterbereich", Aria.role "navigation" ]
                    (List.map filterTagChip model.filterTags)
                ]
            ]
        ]


itemCard : ItemData -> Html Msg
itemCard itemData =
    div
        [ class
            ("card s12 item-card"
                ++ (if itemData.done == 1 then
                        " grey lighten-2 grey-text"

                    else
                        ""
                   )
            )
        , onClick (CardClicked itemData.id)
        ]
        [ div [ class "card-content" ]
            [ editButton
            , span
                [ class
                    ("card-title"
                        ++ (if itemData.done == 1 then
                                " line-through"

                            else
                                ""
                           )
                    )
                ]
                [ text itemData.title ]
            , div [ class "chips-wrapper" ] (List.map displayTagChip itemData.tags)
            ]
        ]


toggleDoneCond : String -> ItemData -> ItemData
toggleDoneCond idToMatch item =
    if item.id == idToMatch then
        { item
            | done =
                if item.done == 0 then
                    1

                else
                    0
        }

    else
        item


toggleDone : String -> List ItemData -> List ItemData
toggleDone itemId items =
    List.map (toggleDoneCond itemId) items


editButton : Html Msg
editButton =
    a
        [ href ""
        , onWithOptions "click" { stopPropagation = True, preventDefault = True } (\event -> EditCard)
        , class "edit-btn right"
        , Aria.role "button"
        , Aria.ariaLabel "Bearbeiten"
        ]
        [ i [ class "material-icons grey-text right-align" ] [ text "edit" ] ]


filterTagChip : FilterTag -> Html Msg
filterTagChip filterTag =
    div
        [ class
            ("chip green-text green lighten-5"
                ++ (case filterTag.isActive of
                        True ->
                            " darken-1 white-text"

                        False ->
                            ""
                   )
            )
        , onClick (FilterClicked filterTag.tag)
        ]
        [ text filterTag.tag ]


displayTagChip : String -> Html Msg
displayTagChip tag =
    div
        [ class "chip green-text green lighten-5" ]
        [ text tag ]


isVisible : Model -> ItemData -> Bool
isVisible model item =
    let
        filters =
            activeFilters model.filterTags

        filteringActive =
            List.length filters > 0
    in
    if not filteringActive then
        True

    else
        List.foldl (||) False (List.map (\x -> List.member x filters) item.tags)


sortItems : List ItemData -> List ItemData
sortItems items =
    List.sortBy .orderIndex items



-- UTILITIES


type alias ItemDataReceived =
    { id : String
    , title : String
    , tags : List String
    , done : Int
    }


jsonParseItemData : Decode.Decoder ItemDataReceived
jsonParseItemData =
    Decode.map4 ItemDataReceived
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "tags" (Decode.list Decode.string))
        (Decode.field "done" Decode.int)


jsonParseItemList : Decode.Decoder (List ItemDataReceived)
jsonParseItemList =
    Decode.list jsonParseItemData


parseItems : String -> List ItemDataReceived
parseItems rawString =
    case Decode.decodeString jsonParseItemList rawString of
        Ok itemsList ->
            itemsList

        Err _ ->
            []


receivedToItem : Int -> ItemDataReceived -> ItemData
receivedToItem index itemReceived =
    { id = itemReceived.id, title = itemReceived.title, tags = itemReceived.tags, done = itemReceived.done, orderIndex = index }


itemToReceived : ItemData -> ItemDataReceived
itemToReceived item =
    { id = item.id, title = item.title, tags = item.tags, done = item.done }


uniqueTags : List ItemData -> Set.Set String
uniqueTags items =
    Set.fromList (allTags items)


allTags : List ItemData -> List String
allTags =
    List.concatMap .tags


toggleTag : String -> List FilterTag -> List FilterTag
toggleTag tag tagList =
    let
        toggle tagToMatch filterTag =
            if tagToMatch == filterTag.tag then
                { filterTag | isActive = not filterTag.isActive }

            else
                filterTag
    in
    List.map (toggle tag) tagList


maybeActiveTag : FilterTag -> Maybe String
maybeActiveTag filterTag =
    if filterTag.isActive then
        Just filterTag.tag

    else
        Nothing


activeFilters : List FilterTag -> List String
activeFilters filterTags =
    List.filterMap maybeActiveTag filterTags
