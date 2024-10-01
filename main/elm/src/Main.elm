port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, br, button, div, h1, h4, header, i, input, label, li, main_, nav, node, p, span, text, ul)
import Html.Attributes exposing (..)
import Html.Attributes.Aria as Aria
import Html.Events exposing (on, onClick, onInput, onMouseDown, preventDefaultOn)
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
    , overrideOrdering : Bool
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
    , draftTitle : String
    , draftTags : List String
    , draftTagsInput : String
    , done : Int
    , orderIndexDefault : Int
    , orderIndexOverride : Int
    , editing : Bool
    , synced : Bool
    , new : Bool
    }


type alias MqttMessageDoneStatus =
    { id : String
    , status : Bool
    }



-- INIT


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        apiKey =
            case apiKeyFromFlags flags of
                Just decodedApiKey ->
                    decodedApiKey

                Nothing ->
                    ""
    in
    ( Model Dict.empty False [] apiKey, getItems apiKey )


apiKeyFromFlags : Decode.Value -> Maybe String
apiKeyFromFlags flags =
    case Decode.decodeValue (Decode.field "apiKey" Decode.string) flags of
        Ok apiKey ->
            Just apiKey

        Err _ ->
            Nothing


localStoreModelFromFlags : Decode.Value -> Maybe String
localStoreModelFromFlags flags =
    case Decode.decodeValue (Decode.field "localStore" (Decode.nullable Decode.string)) flags of
        Ok data ->
            data

        Err _ ->
            Nothing



-- UPDATE


type Msg
    = FetchItems
    | ItemsReceived (Result Http.Error String)
    | EditCardClicked String
    | FilterClicked String
    | CardClicked String
    | CancelEditing String
    | FinishEditing String
    | AddNewCardClicked
    | AddNewCard UUID.UUID
    | DraftTitleChanged String String
    | SortButtonClicked
    | SortResponseReceived (List String) (Result Http.Error (List Int))
    | DoneResponseReceived (Result Http.Error Bool)
    | UpdateResponseReceived String (Result Http.Error Bool)
    | DeleteResponseReceived String (Result Http.Error Bool)
    | ReceivedMQTTMessageDoneStatus String
    | ReceivedMQTTMessageNewItem String
    | ItemPosted String (Result Http.Error PostResponse)
    | DraftTagsChanged String (List String)
    | DraftTagsInputChanged String String
    | DeleteCard String
    | DeleteAllDone


itemsUrl : String -> String
itemsUrl apiKey =
    "https://picluster.a-h.wtf/einkaufsliste/api/v1/items?k=" ++ apiKey


itemUrl : String -> String -> String
itemUrl apiKey itemId =
    "https://picluster.a-h.wtf/einkaufsliste/api/v1/items/" ++ itemId ++ "?k=" ++ apiKey


updateDoneUrl : String -> String -> String
updateDoneUrl apiKey itemId =
    "https://picluster.a-h.wtf/einkaufsliste/api/v1/items/" ++ itemId ++ "/done?k=" ++ apiKey


sortUrl : String
sortUrl =
    "https://picluster.a-h.wtf/einkaufs_api/sort/"


getItems : String -> Cmd Msg
getItems apiKey =
    Http.get { url = itemsUrl apiKey, expect = Http.expectString ItemsReceived }


type alias PostResponse =
    { success : Bool
    , newId : String
    }


postItem : String -> ItemData -> Cmd Msg
postItem apiKey item =
    Http.post
        { url = itemsUrl apiKey
        , body = Http.jsonBody (Encode.object [ ( "itemData", Encode.object [ ( "title", Encode.string item.title ), ( "tags", Encode.list Encode.string item.tags ) ] ) ])
        , expect = Http.expectJson (ItemPosted item.id) (Decode.map2 PostResponse (Decode.field "success" Decode.bool) (Decode.field "newId" Decode.string))
        }


deleteItem : String -> String -> Cmd Msg
deleteItem apiKey itemId =
    httpDelete
        { url = itemUrl apiKey itemId
        , body = Http.emptyBody
        , expect = Http.expectJson (DeleteResponseReceived itemId) (Decode.field "success" Decode.bool)
        }


updateItem : String -> ItemData -> Cmd Msg
updateItem apiKey item =
    httpUpdate
        { url = itemUrl apiKey item.id
        , body = Http.jsonBody (Encode.object [ ( "itemData", Encode.object [ ( "title", Encode.string item.title ), ( "tags", Encode.list Encode.string item.tags ) ] ) ])
        , expect = Http.expectJson (UpdateResponseReceived item.id) (Decode.field "success" Decode.bool)
        }


httpUpdate : { url : String, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
httpUpdate options =
    Http.request
        { method = "UPDATE"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = options.url
        , body = options.body
        , expect = options.expect
        , timeout = Nothing
        , tracker = Nothing
        }


httpDelete : { url : String, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
httpDelete options =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = options.url
        , body = options.body
        , expect = options.expect
        , timeout = Nothing
        , tracker = Nothing
        }


updateDoneBackend : String -> String -> Int -> Cmd Msg
updateDoneBackend apiKey itemId doneStatus =
    httpUpdate
        { url = updateDoneUrl apiKey itemId
        , body = Http.jsonBody (Encode.object [ ( "done", Encode.int doneStatus ) ])
        , expect = Http.expectJson DoneResponseReceived (Decode.field "success" Decode.bool)
        }


callSortAPI : List ItemData -> Cmd Msg
callSortAPI items =
    Http.post
        { url = sortUrl
        , body = Http.jsonBody (Encode.object [ ( "input_list", Encode.list Encode.string (List.map .title items) ) ])
        , expect = Http.expectJson (SortResponseReceived (List.map .id items)) sortAPIResponseDecoder
        }


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

                        newModel =
                            { model | items = items, filterTags = initTags (getFilterTags items) }
                    in
                    ( newModel, writeToLocalStorage (encodeModel newModel) )

                Err httpError ->
                    ( model, Cmd.none )

        FilterClicked tag ->
            let
                newModel =
                    { model | filterTags = toggleTag tag model.filterTags }
            in
            ( newModel, writeToLocalStorage (encodeModel newModel) )

        CardClicked itemId ->
            let
                newItems =
                    Dict.update itemId toggleDone model.items

                maybeItem =
                    Dict.get itemId newItems
            in
            ( { model | items = newItems }
            , case maybeItem of
                Just item ->
                    updateDoneBackend model.apiKey itemId item.done

                Nothing ->
                    Cmd.none
            )

        EditCardClicked itemId ->
            ( { model | items = Dict.update itemId toggleEdit model.items }, Cmd.none )

        CancelEditing itemId ->
            let
                maybeItem =
                    Dict.get itemId model.items
            in
            case maybeItem of
                Just item ->
                    if item.new then
                        ( { model | items = Dict.remove itemId model.items }, Cmd.none )

                    else
                        ( { model | items = Dict.update itemId toggleEdit model.items }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FinishEditing itemId ->
            case Dict.get itemId model.items of
                Just item ->
                    if item.synced then
                        ( { model | items = Dict.update itemId toggleEdit model.items }, Cmd.none )

                    else
                        let
                            remainingText =
                                String.trim item.draftTagsInput

                            updatedItem =
                                { item
                                    | title = String.trim item.draftTitle
                                    , tags =
                                        item.draftTags
                                            ++ (if remainingText == "" then
                                                    []

                                                else
                                                    [ remainingText ]
                                               )
                                    , draftTagsInput = ""
                                }

                            newItems =
                                Dict.update itemId
                                    (\i ->
                                        case i of
                                            Just it ->
                                                Just updatedItem

                                            Nothing ->
                                                Nothing
                                    )
                                    model.items

                            newFilters =
                                getFilterTags newItems

                            oldFilters =
                                List.map .tag model.filterTags

                            filterTagsDecimated =
                                List.filter (\x -> List.member x.tag newFilters) model.filterTags

                            additionalFilters =
                                List.filter (\x -> not (List.member x oldFilters)) newFilters

                            filterTagsAdded =
                                filterTagsDecimated ++ List.map (\x -> { tag = x, isActive = False }) additionalFilters
                        in
                        ( { model
                            | items = newItems
                            , filterTags = filterTagsAdded
                          }
                        , if item.new then
                            postItem model.apiKey updatedItem

                          else
                            updateItem model.apiKey updatedItem
                        )

                Nothing ->
                    ( model, Cmd.none )

        AddNewCardClicked ->
            ( model, Random.generate AddNewCard UUID.generator )

        AddNewCard newUUID ->
            let
                newId =
                    "local-" ++ UUID.toString newUUID
            in
            ( { model | items = addNewItem newId model.items }, Cmd.none )

        DraftTitleChanged itemId newTitle ->
            ( { model | items = Dict.update itemId (updateDraftTitle newTitle) model.items }, Cmd.none )

        SortButtonClicked ->
            if model.overrideOrdering then
                ( { model | overrideOrdering = False }, Cmd.none )

            else
                ( model, callSortAPI (Dict.values model.items) )

        SortResponseReceived requestedIds payload ->
            case payload of
                Ok sortIndices ->
                    let
                        newIndices =
                            argsort (List.reverse sortIndices)

                        idToIndexDict =
                            Dict.fromList (List.map2 Tuple.pair requestedIds newIndices)
                    in
                    ( { model | items = Dict.map (updateOverrideOrderIndex idToIndexDict) model.items, overrideOrdering = True }, Cmd.none )

                Err httpError ->
                    ( model, Cmd.none )

        DoneResponseReceived success ->
            ( model, Cmd.none )

        UpdateResponseReceived itemId successPayload ->
            case successPayload of
                Ok success ->
                    let
                        newItemDict =
                            Dict.update itemId
                                (\maybeItem ->
                                    case maybeItem of
                                        Just item ->
                                            Just { item | synced = True, editing = False }

                                        Nothing ->
                                            Nothing
                                )
                                model.items
                    in
                    ( { model | items = newItemDict }
                    , if model.overrideOrdering then
                        callSortAPI (Dict.values newItemDict)

                      else
                        Cmd.none
                    )

                Err httpError ->
                    ( model, Cmd.none )

        DeleteAllDone ->
            let
                doneIds =
                    List.map .id (List.filter (\item -> item.done == 1) (Dict.values model.items))
            in
            ( model, Cmd.batch (List.map (deleteItem model.apiKey) doneIds) )

        DeleteResponseReceived itemId successPayload ->
            case successPayload of
                Ok success ->
                    if success then
                        ( { model | items = Dict.remove itemId model.items }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Err httpError ->
                    ( model, Cmd.none )

        ReceivedMQTTMessageDoneStatus mqttMsg ->
            let
                maybeMqttData =
                    parseMQTTMessageDoneStatus mqttMsg
            in
            case maybeMqttData of
                Just mqttData ->
                    ( { model
                        | items =
                            Dict.update mqttData.id
                                (setDone
                                    (if mqttData.status then
                                        1

                                     else
                                        0
                                    )
                                )
                                model.items
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ReceivedMQTTMessageNewItem mqttMsg ->
            let
                maybeReceivedItem =
                    parseMQTTMessageNewItem mqttMsg

                newItems =
                    case maybeReceivedItem of
                        Just receivedItem ->
                            addReceivedItem receivedItem model.items

                        Nothing ->
                            model.items
            in
            ( { model | items = newItems }
            , if model.overrideOrdering then
                callSortAPI (Dict.values newItems)

              else
                Cmd.none
            )

        ItemPosted oldId postResponsePayload ->
            case postResponsePayload of
                Ok postResponse ->
                    let
                        maybeOldItem =
                            Dict.get oldId model.items

                        newItemDict =
                            case maybeOldItem of
                                Just oldItem ->
                                    Dict.remove oldId model.items
                                        |> Dict.insert postResponse.newId { oldItem | id = postResponse.newId, synced = True, new = False, editing = False }

                                Nothing ->
                                    model.items
                    in
                    ( { model | items = newItemDict }
                    , if model.overrideOrdering then
                        callSortAPI (Dict.values newItemDict)

                      else
                        Cmd.none
                    )

                Err httpError ->
                    ( model, Cmd.none )

        DraftTagsChanged itemId newTagList ->
            ( { model | items = Dict.update itemId (updateDraftTags newTagList) model.items }, Cmd.none )

        DraftTagsInputChanged itemId remainingText ->
            ( { model | items = Dict.update itemId (updateDraftTagsInput remainingText) model.items }, Cmd.none )

        DeleteCard itemId ->
            ( model, deleteItem model.apiKey itemId )


updateOverrideOrderIndex : Dict String Int -> String -> ItemData -> ItemData
updateOverrideOrderIndex idToIndexDict itemId item =
    { item
        | orderIndexOverride =
            case Dict.get itemId idToIndexDict of
                Just newIndex ->
                    newIndex

                Nothing ->
                    item.orderIndexOverride
    }


updateDraftTitle : String -> Maybe ItemData -> Maybe ItemData
updateDraftTitle newTitle maybeItem =
    case maybeItem of
        Just item ->
            Just
                { item
                    | draftTitle = newTitle
                    , synced =
                        if newTitle /= item.title then
                            False

                        else
                            item.synced
                }

        Nothing ->
            Nothing


updateDraftTags : List String -> Maybe ItemData -> Maybe ItemData
updateDraftTags newDraftTags maybeItem =
    case maybeItem of
        Just item ->
            Just
                { item
                    | draftTags = newDraftTags
                    , synced =
                        if listEqual newDraftTags item.tags then
                            item.synced

                        else
                            False
                }

        Nothing ->
            Nothing


updateDraftTagsInput : String -> Maybe ItemData -> Maybe ItemData
updateDraftTagsInput newText maybeItem =
    case maybeItem of
        Just item ->
            Just
                { item
                    | draftTagsInput = newText
                    , synced =
                        if String.trim newText == "" then
                            item.synced

                        else
                            False
                }

        Nothing ->
            Nothing


listEqual : List comparable -> List comparable -> Bool
listEqual listA listB =
    let
        sortedA =
            List.sort listA

        sortedB =
            List.sort listB
    in
    List.map2 Tuple.pair sortedA sortedB
        |> List.map (\x -> Tuple.first x == Tuple.second x)
        |> List.foldr (&&) (List.length listA == List.length listB)


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
            Just { item | editing = not item.editing, draftTitle = item.title, draftTags = item.tags, draftTagsInput = "" }

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


setDone : Int -> Maybe ItemData -> Maybe ItemData
setDone newStatus maybeItem =
    case maybeItem of
        Just item ->
            Just { item | done = newStatus }

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
    Dict.insert newId
        { id = newId
        , title = ""
        , tags = []
        , draftTitle = ""
        , draftTags = []
        , draftTagsInput = ""
        , done = 0
        , orderIndexDefault = newIndex
        , orderIndexOverride = newIndex
        , editing = True
        , synced = False
        , new = True
        }
        dict


addReceivedItem : ItemDataReceived -> Dict String ItemData -> Dict String ItemData
addReceivedItem itemDataReceived dict =
    let
        newIndex =
            case maxOrderIndex (Dict.values dict) of
                Just maxIndex ->
                    maxIndex + 1

                Nothing ->
                    0

        itemData =
            receivedToItem newIndex itemDataReceived
    in
    Dict.insert itemData.id itemData dict



-- SUBSCRIPTIONS


type alias ChipsInitArgs =
    { parentSelector : String
    , tags : List String
    }


port receiveMQTTMessageDoneStatus : (String -> msg) -> Sub msg


port receiveMQTTMessageNewItem : (String -> msg) -> Sub msg


port writeToLocalStorage : Encode.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveMQTTMessageDoneStatus ReceivedMQTTMessageDoneStatus
        , receiveMQTTMessageNewItem ReceivedMQTTMessageNewItem
        ]


parseMQTTMessageDoneStatus : String -> Maybe MqttMessageDoneStatus
parseMQTTMessageDoneStatus rawString =
    case Decode.decodeString (Decode.map2 MqttMessageDoneStatus (Decode.field "id" Decode.string) (Decode.field "status" Decode.bool)) rawString of
        Ok mqttData ->
            Just mqttData

        Err _ ->
            Nothing


parseMQTTMessageNewItem : String -> Maybe ItemDataReceived
parseMQTTMessageNewItem rawString =
    case Decode.decodeString jsonParseItemData rawString of
        Ok itemDataReceived ->
            Just itemDataReceived

        Err _ ->
            Nothing



{- subscriptions : Model -> Sub Msg
   subscriptions model =
       Sub.batch
           []
-}
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ headerView model
        , main_ [ Aria.ariaLabel "Listenbereich" ]
            [ itemCardsView model
            , addCardButton
            ]
        ]


itemsToShow : Model -> List ItemData
itemsToShow model =
    sortItems model.overrideOrdering (List.filter (isVisible model) (Dict.values model.items))


headerView : Model -> Html Msg
headerView model =
    header [ Aria.ariaLabel "Filterbereich" ]
        [ nav [ Aria.ariaLabel "Header", style "height" "auto" ]
            [ div [ class "nav-wrapper", style "display" "flex" ]
                [ div [ class "chips-wrapper filter-chips", Aria.ariaLabel "Filterbereich", Aria.role "navigation" ]
                    (List.map filterTagChip (sortFilterTags model.filterTags))
                , ul [ id "nav-mobile" ]
                    [ li [] [ sortButton model.overrideOrdering ]
                    , li []
                        [ a [ href "#delConfirmModal", class "modal-trigger", Aria.role "button", Aria.ariaLabel "Abgehakte löschen" ]
                            [ i [ class "material-icons white-text" ] [ text "delete" ]
                            ]
                        ]
                    ]
                ]
            ]
        , delAllModal
        ]


sortButton : Bool -> Html Msg
sortButton isActive =
    a
        [ href ""
        , onWithOptions "click" { preventDefault = True, stopPropagation = True } (\event -> SortButtonClicked)
        , Aria.role "button"
        , Aria.ariaLabel "Sortieren"
        ]
        [ i
            [ class
                ("material-icons"
                    ++ (if isActive then
                            " yellow-text"

                        else
                            " white-text"
                       )
                )
            ]
            [ text "sort" ]
        ]


delAllModal : Html Msg
delAllModal =
    node "custom-modal"
        []
        [ div [ id "delConfirmModal", class "modal" ]
            [ div [ class "modal-content" ]
                [ h4 [] [ text "Sicher?" ]
                , p [] [ text "Alle abgehakten löschen?" ]
                ]
            , div [ class "modal-footer" ]
                [ a [ href "#!", class "modal-close waves-effect waves-green btn-flat" ] [ text "Abbrechen" ]
                , a [ href "#!", class "modal-close waves-effect waves-green btn-flat", onClick DeleteAllDone ] [ text "Löschen" ]
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
            ((if item.new then
                []

              else
                [ deleteCardButton item.id ]
             )
                ++ [ div [ class "input-field card-title" ] [ input [ placeholder "Neuer Eintrag", type_ "text", value item.draftTitle, onInput (DraftTitleChanged item.id) ] [] ]
                   , editChipsView item
                   , div [ class "card-action valign-wrapper justify-right" ]
                        [ cancelButton item.id
                        , a [ class "green btn finish-edit", Aria.role "button", Aria.ariaLabel "Bestätigen", onClick (FinishEditing item.id) ] [ i [ class "material-icons" ] [ text "check" ] ]
                        ]
                   ]
            )
        ]


editChipsView : ItemData -> Html Msg
editChipsView item =
    node "custom-chips"
        [ class "chips chips-autocomplete chips-placeholder"
        , placeholder "Tags"
        , on "tagsChanged" <|
            Decode.map (DraftTagsChanged item.id) <|
                Decode.at [ "detail", "tags" ] <|
                    Decode.list Decode.string
        , on "inputChanged" <|
            Decode.map (DraftTagsInputChanged item.id) <|
                Decode.at [ "detail", "remainingText" ] <|
                    Decode.string
        ]
        (List.map tagElement item.draftTags)


tagElement : String -> Html Msg
tagElement tagName =
    node "chips-tag" [ value tagName ] []


cancelButton : String -> Html Msg
cancelButton itemId =
    a
        [ href ""
        , onWithOptions "click" { preventDefault = True, stopPropagation = True } (\event -> CancelEditing itemId)
        , class "grey-text cancel-edit"
        , Aria.role "button"
        ]
        [ text "Abbrechen" ]


deleteCardButton : String -> Html Msg
deleteCardButton itemId =
    a
        [ href ""
        , onWithOptions "click" { preventDefault = True, stopPropagation = True } (\event -> DeleteCard itemId)
        , class "delete-btn right"
        , Aria.role "button"
        , Aria.ariaLabel "Löschen"
        ]
        [ i [ class "material-icons grey-text right-align" ] [ text "delete" ] ]


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
    if not filteringActive || item.editing || (List.member "No tags" filters && (List.length item.tags == 0)) then
        True

    else
        List.foldl (||) False (List.map (\x -> List.member x filters) item.tags)


sortItems : Bool -> List ItemData -> List ItemData
sortItems useOverrideIndex items =
    if useOverrideIndex then
        List.sortBy .orderIndexOverride items
            |> List.reverse

    else
        List.sortBy .orderIndexDefault items
            |> List.reverse


addCardButton : Html Msg
addCardButton =
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
    { id = itemReceived.id
    , title = itemReceived.title
    , tags = itemReceived.tags
    , draftTitle = itemReceived.title
    , draftTags = itemReceived.tags
    , draftTagsInput = ""
    , done = itemReceived.done
    , orderIndexDefault = index
    , orderIndexOverride = index
    , editing = False
    , synced = True
    , new = False
    }


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


getFilterTags : Dict String ItemData -> List String
getFilterTags items =
    [ "No tags" ] ++ Set.toList (uniqueTags (Dict.values items))


sortFilterTags : List FilterTag -> List FilterTag
sortFilterTags filterTags =
    case filterTags of
        noTags :: rest ->
            noTags :: List.sortBy .tag rest

        [] ->
            []


itemListToDict : List ItemData -> Dict String ItemData
itemListToDict items =
    Dict.fromList (itemListToAssoc items)


itemListToAssoc : List ItemData -> List ( String, ItemData )
itemListToAssoc items =
    List.map (\item -> ( item.id, item )) items


maxOrderIndex : List ItemData -> Maybe Int
maxOrderIndex items =
    List.maximum (List.map .orderIndexDefault items)


sortAPIResponseDecoder : Decode.Decoder (List Int)
sortAPIResponseDecoder =
    Decode.field "sort_indices" (Decode.list Decode.int)


argsort : List comparable -> List Int
argsort l =
    List.indexedMap Tuple.pair l
        |> List.sortBy Tuple.second
        |> List.map Tuple.first


encodeItemData : ItemData -> Encode.Value
encodeItemData { id, title, tags, draftTitle, draftTags, draftTagsInput, done, orderIndexDefault, orderIndexOverride, editing, synced, new } =
    Encode.object
        [ ( "id", Encode.string id )
        , ( "title", Encode.string title )
        , ( "tags", Encode.list Encode.string tags )
        , ( "draftTitle", Encode.string draftTitle )
        , ( "draftTags", Encode.list Encode.string draftTags )
        , ( "draftTagsInput", Encode.string draftTagsInput )
        , ( "done", Encode.int done )
        , ( "orderIndexDefault", Encode.int orderIndexDefault )
        , ( "orderIndexOverride", Encode.int orderIndexOverride )
        , ( "editing", Encode.bool editing )
        , ( "synced", Encode.bool synced )
        , ( "new", Encode.bool new )
        ]


encodeFilterTag : FilterTag -> Encode.Value
encodeFilterTag { tag, isActive } =
    Encode.object
        [ ( "tag", Encode.string tag )
        , ( "isActive", Encode.bool isActive )
        ]


encodeModel : Model -> Encode.Value
encodeModel { items, overrideOrdering, filterTags, apiKey } =
    Encode.object
        [ ( "items", Encode.dict identity encodeItemData items )
        , ( "overrideOrdering", Encode.bool overrideOrdering )
        , ( "filterTags", Encode.list encodeFilterTag filterTags )
        , ( "apiKey", Encode.string "" ) --- we don't store the apiKey
        ]
