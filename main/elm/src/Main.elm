module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, br, button, div, h1, header, i, input, label, main_, nav, p, span, text)
import Html.Attributes exposing (..)
import Html.Attributes.Aria as Aria
import Html.Events exposing (onClick, onInput, onMouseDown, preventDefaultOn)
import Html.Events.Extra.Mouse exposing (onWithOptions)
import Html.Events.Extra.Touch exposing (onStart)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Set
import Time
import UUID



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { items : Dict String ItemData
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
    , editing : Bool
    }



-- INIT


init : String -> ( Model, Cmd Msg )
init flags =
    ( Model Dict.empty [] flags, getItems flags )



-- UPDATE


type Msg
    = FetchItems
    | ItemsReceived (Result Http.Error String)
    | EditCardClicked String
    | FilterClicked String
    | CardClicked String
    | CancelEditing
    | AddNewCardClicked
    | AddNewCard UUID.UUID
    | TitleChanged String String


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
                            itemListToDict (List.indexedMap receivedToItem (parseItems rawString))
                    in
                    ( { model | items = items, filterTags = initTags ([ "No tags" ] ++ Set.toList (uniqueTags (Dict.values items))) }, Cmd.none )

                Err httpError ->
                    ( model, Cmd.none )

        FilterClicked tag ->
            ( { model | filterTags = toggleTag tag model.filterTags }, Cmd.none )

        CardClicked itemId ->
            ( { model | items = Dict.update itemId toggleDone model.items }, Cmd.none )

        EditCardClicked itemId ->
            ( { model | items = Dict.update itemId toggleEdit model.items }, Cmd.none )

        AddNewCardClicked ->
            ( model, Random.generate AddNewCard UUID.generator )

        AddNewCard newUUID ->
            let
                newId =
                    UUID.toString newUUID
            in
            ( { model | items = addNewItem newId model.items }, Cmd.none )

        TitleChanged itemId newTitle ->
            ( { model | items = Dict.update itemId (updateTitle newTitle) model.items }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateTitle : String -> Maybe ItemData -> Maybe ItemData
updateTitle newTitle maybeItem =
    case maybeItem of
        Just item ->
            Just { item | title = newTitle }

        Nothing ->
            Nothing


toggleEdit : Maybe ItemData -> Maybe ItemData
toggleEdit maybeItem =
    case maybeItem of
        Just item ->
            Just { item | editing = not item.editing }

        Nothing ->
            Nothing


toggleDone : Maybe ItemData -> Maybe ItemData
toggleDone maybeItem =
    case maybeItem of
        Just item ->
            Just
                { item
                    | done =
                        if item.done == 0 then
                            1

                        else
                            0
                }

        Nothing ->
            Nothing


addNewItem : String -> Dict String ItemData -> Dict String ItemData
addNewItem newId dict =
    let
        newIndex =
            case maxOrderIndex (Dict.values dict) of
                Just maxIndex ->
                    maxIndex + 1

                Nothing ->
                    0
    in
    Dict.insert newId { id = newId, title = "", tags = [], done = 0, orderIndex = newIndex, editing = True } dict



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
            [ itemCardsView model
            , addCardButton model
            ]
        ]


itemsToShow : Model -> List ItemData
itemsToShow model =
    sortItems (List.filter (isVisible model) (Dict.values model.items))


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


itemCardsView : Model -> Html Msg
itemCardsView model =
    div [ class "container" ] (List.map cardView (itemsToShow model))


cardView : ItemData -> Html Msg
cardView itemData =
    if itemData.editing then
        editCard itemData

    else
        itemCard itemData


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
            [ editButton itemData
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


editButton : ItemData -> Html Msg
editButton item =
    a
        [ href ""
        , onWithOptions "click" { stopPropagation = True, preventDefault = True } (\event -> EditCardClicked item.id)
        , class "edit-btn right"
        , Aria.role "button"
        , Aria.ariaLabel "Bearbeiten"
        ]
        [ i [ class "material-icons grey-text right-align" ] [ text "edit" ] ]


editCard : ItemData -> Html Msg
editCard item =
    div
        [ class
            "card s12 item-edit"
        ]
        [ div [ class "card-content" ]
            [ div [ class "input-field card-title" ] [ input [ placeholder "Neuer Eintrag", type_ "text", value item.title, onInput (TitleChanged item.id) ] [] ]
            , div [ class "chips chips-autocomplete chips-placeholder", placeholder "Tags" ] []
            , div [ class "card-action valign-wrapper justify-right" ]
                [ cancelButton
                , a [ class "green btn finish-edit", Aria.role "button", Aria.ariaLabel "Bestätigen" ] [ i [ class "material-icons" ] [ text "check" ] ]
                ]
            ]
        ]


cancelButton : Html Msg
cancelButton =
    a
        [ href ""
        , onWithOptions "click" { preventDefault = True, stopPropagation = True } (\event -> CancelEditing)
        , class "grey-text cancel-edit"
        , Aria.role "button"
        ]
        [ text "Abbrechen" ]


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
    if not filteringActive || item.editing then
        True

    else
        List.foldl (||) False (List.map (\x -> List.member x filters) item.tags)


sortItems : List ItemData -> List ItemData
sortItems items =
    List.sortBy .orderIndex items
        |> List.reverse


addCardButton : Model -> Html Msg
addCardButton model =
    div [ class "fixed-action-btn center-horizontally" ]
        [ a
            [ class "btn-floating btn-large waves-effect red"
            , Aria.role "button"
            , Aria.ariaLabel "Neuer Eintrag"
            , onClick AddNewCardClicked
            ]
            [ i [ class "large material-icons" ] [ text "add" ]
            ]
        ]



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
    { id = itemReceived.id, title = itemReceived.title, tags = itemReceived.tags, done = itemReceived.done, orderIndex = index, editing = False }


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


itemListToDict : List ItemData -> Dict String ItemData
itemListToDict items =
    Dict.fromList (itemListToAssoc items)


itemListToAssoc : List ItemData -> List ( String, ItemData )
itemListToAssoc items =
    List.map (\item -> ( item.id, item )) items


maxOrderIndex : List ItemData -> Maybe Int
maxOrderIndex items =
    List.maximum (List.map .orderIndex items)
