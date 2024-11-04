port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import FilterTag exposing (FilterTag)
import Html exposing (Html, a, br, button, div, h1, h4, header, i, input, label, li, main_, nav, node, p, span, text, ul)
import Html.Attributes exposing (..)
import Html.Attributes.Aria as Aria
import Html.Events exposing (on, onClick, onFocus, onInput, onMouseDown, preventDefaultOn)
import Html.Events.Extra.Mouse exposing (onWithOptions)
import Html.Events.Extra.Touch exposing (onStart)
import Http
import ItemData exposing (ItemData, ItemDataReceived)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Set
import Task
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
    , noTagsFilterActive : Bool
    , apiKey : String
    , geolocation : Maybe Geolocation
    , userAgent : String
    , clientId : String
    }


encodeModel : Model -> Encode.Value
encodeModel { items, overrideOrdering, filterTags, noTagsFilterActive, apiKey } =
    Encode.object
        [ ( "items", Encode.dict identity ItemData.encode items )
        , ( "overrideOrdering", Encode.bool overrideOrdering )
        , ( "filterTags", Encode.list encodeFilterTag filterTags )
        , ( "noTagsFilterActive", Encode.bool noTagsFilterActive )
        ]


type alias MqttMessageDoneStatus =
    { id : String
    , status : Int
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

        clientId =
            case clientIdFromFlags flags of
                Just decodedClientId ->
                    decodedClientId

                Nothing ->
                    ""

        userAgent =
            case userAgentFromFlags flags of
                Just decodedUserAgent ->
                    decodedUserAgent

                Nothing ->
                    ""

        items =
            case itemsFromLocalStorage flags of
                Just itemDict ->
                    itemDict

                Nothing ->
                    Dict.empty

        filterTags =
            case filterTagsFromLocalStorage flags of
                Just filterTagList ->
                    filterTagList

                Nothing ->
                    []

        overrideOrdering =
            case overrideOrderingFromLocalStorage flags of
                Just overrideOrderingFlag ->
                    overrideOrderingFlag

                Nothing ->
                    False

        noTagsFilterActive =
            case noTagsFilterFromLocalStorage flags of
                Just filterActive ->
                    filterActive

                Nothing ->
                    False
    in
    ( { items = items
      , overrideOrdering = overrideOrdering
      , filterTags = filterTags
      , noTagsFilterActive = noTagsFilterActive
      , apiKey = apiKey
      , geolocation = Nothing
      , userAgent = userAgent
      , clientId = clientId
      }
      -- , getItems apiKey
    , syncItems apiKey clientId (List.sortBy .orderIndexDefault (Dict.values items))
      --, Cmd.none
    )


apiKeyFromFlags : Decode.Value -> Maybe String
apiKeyFromFlags flags =
    case Decode.decodeValue (Decode.field "apiKey" Decode.string) flags of
        Ok apiKey ->
            Just apiKey

        Err _ ->
            Nothing


clientIdFromFlags : Decode.Value -> Maybe String
clientIdFromFlags flags =
    case Decode.decodeValue (Decode.field "clientId" Decode.string) flags of
        Ok clientId ->
            Just clientId

        Err _ ->
            Nothing


userAgentFromFlags : Decode.Value -> Maybe String
userAgentFromFlags flags =
    case Decode.decodeValue (Decode.field "userAgent" Decode.string) flags of
        Ok userAgent ->
            Just userAgent

        Err _ ->
            Nothing


itemsFromLocalStorage : Decode.Value -> Maybe (Dict String ItemData)
itemsFromLocalStorage flags =
    case Decode.decodeValue (Decode.at [ "localStore", "items" ] (Decode.dict itemDataDecoder)) flags of
        Ok data ->
            Just data

        Err _ ->
            Nothing


itemDataDecoder : Decode.Decoder ItemData
itemDataDecoder =
    Decode.succeed ItemData
        |> required "id" Decode.string
        |> required "title" Decode.string
        |> required "tags" (Decode.list Decode.string)
        |> required "draftTitle" Decode.string
        |> required "draftTags" (Decode.list Decode.string)
        |> required "draftTagsInput" Decode.string
        |> required "draftChanged" Decode.bool
        |> required "done" Decode.int
        |> required "orderIndexDefault" Decode.int
        |> required "orderIndexOverride" Decode.int
        |> required "editing" Decode.bool
        |> required "synced" Decode.bool
        |> required "new" Decode.bool
        |> required "lastSyncedRevision" Decode.int
        |> required "oldId" (Decode.oneOf [ Decode.string, Decode.null "" ])


filterTagsFromLocalStorage : Decode.Value -> Maybe (List FilterTag)
filterTagsFromLocalStorage flags =
    case Decode.decodeValue (Decode.at [ "localStore", "filterTags" ] (Decode.list filterTagDecoder)) flags of
        Ok data ->
            Just data

        Err _ ->
            Nothing


filterTagDecoder : Decode.Decoder FilterTag
filterTagDecoder =
    Decode.map2 FilterTag (Decode.field "tag" Decode.string) (Decode.field "isActive" Decode.bool)


overrideOrderingFromLocalStorage : Decode.Value -> Maybe Bool
overrideOrderingFromLocalStorage flags =
    case Decode.decodeValue (Decode.at [ "localStore", "overrideOrdering" ] Decode.bool) flags of
        Ok data ->
            Just data

        Err _ ->
            Nothing


noTagsFilterFromLocalStorage : Decode.Value -> Maybe Bool
noTagsFilterFromLocalStorage flags =
    case Decode.decodeValue (Decode.at [ "localStore", "noTagsFilterActive" ] Decode.bool) flags of
        Ok data ->
            Just data

        Err _ ->
            Nothing


decodeApply =
    Decode.map2 (|>)


required : String -> Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
required fieldName itemDecoder functionDecoder =
    decodeApply (Decode.field fieldName itemDecoder) functionDecoder



-- UPDATE


type Msg
    = ItemsReceived (Result Http.Error String)
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
    | DoneResponseReceived String (Result Http.Error Bool)
    | UpdateResponseReceived String (Result Http.Error UpdateResponse)
    | DeleteResponseReceived String (Result Http.Error Bool)
    | ReceivedMQTTMessageDoneStatus String
    | ReceivedMQTTMessageNewItem String
    | ReceivedMQTTMessageDeletedItem String
    | ReceivedMQTTMessageUpdatedItem String
    | ItemPosted String (Result Http.Error PostResponse)
    | DraftTagsChanged String (List String)
    | DraftTagsInputChanged String String
    | DeleteItem String
    | DeleteAllDone
    | ReceivedGeolocation Decode.Value
    | CollectResponseReceived (Result Http.Error ())
    | GotFocus String
    | NoTagsFilterClicked
    | NoOp


backendBaseUrl : String
backendBaseUrl =
    "https://picluster.a-h.wtf/einkaufsliste-multiuser/api/v1"


itemsUrl : String -> String -> String
itemsUrl apiKey clientId =
    backendBaseUrl ++ "/items?k=" ++ apiKey ++ "&c=" ++ clientId


itemUrl : String -> String -> String
itemUrl apiKey itemId =
    backendBaseUrl ++ "/items/" ++ itemId ++ "?k=" ++ apiKey


updateDoneUrl : String -> String -> String
updateDoneUrl apiKey itemId =
    backendBaseUrl ++ "/items/" ++ itemId ++ "/done?k=" ++ apiKey


itemsSyncUrl : String -> String -> String
itemsSyncUrl apiKey clientId =
    backendBaseUrl ++ "/items/sync?k=" ++ apiKey ++ "&c=" ++ clientId


dataBaseUrl : String
dataBaseUrl =
    "https://picluster.a-h.wtf/einkaufs_api"


sortUrl : String
sortUrl =
    dataBaseUrl ++ "/sort/"


collectUrl : String
collectUrl =
    dataBaseUrl ++ "/collect/"


syncItems : String -> String -> List ItemData -> Cmd Msg
syncItems apiKey clientId items =
    Http.post
        { url = itemsSyncUrl apiKey clientId
        , body = Http.jsonBody (Encode.object [ ( "clientItems", Encode.list ItemData.encode items ) ])
        , expect = Http.expectString ItemsReceived
        }


type alias PostResponse =
    { success : Bool
    , newId : String
    , revision : Int
    }


type alias UpdateResponse =
    { success : Bool
    , revision : Int
    }


postItem : String -> String -> ItemData -> Cmd Msg
postItem apiKey clientId item =
    Http.post
        { url = itemsUrl apiKey clientId
        , body = Http.jsonBody (Encode.object [ ( "itemData", Encode.object [ ( "title", Encode.string item.title ), ( "tags", Encode.list Encode.string item.tags ) ] ) ])
        , expect =
            Http.expectJson (ItemPosted item.id)
                (Decode.map3 PostResponse
                    (Decode.field "success" Decode.bool)
                    (Decode.field "newId" Decode.string)
                    (Decode.field "revision" Decode.int)
                )
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
        , expect = Http.expectJson (UpdateResponseReceived item.id) (Decode.map2 UpdateResponse (Decode.field "success" Decode.bool) (Decode.field "revision" Decode.int))
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
        , expect = Http.expectJson (DoneResponseReceived itemId) (Decode.field "success" Decode.bool)
        }


callSortAPI : List ItemData -> Cmd Msg
callSortAPI items =
    Http.post
        { url = sortUrl
        , body = Http.jsonBody (Encode.object [ ( "input_list", Encode.list Encode.string (List.map .title items) ) ])
        , expect = Http.expectJson (SortResponseReceived (List.map .id items)) sortAPIResponseDecoder
        }


postCollectEvent : Model -> ItemData -> Cmd Msg
postCollectEvent model item =
    let
        actionType =
            case item.done of
                0 ->
                    "UNCROSSED"

                _ ->
                    "CROSSED"

        latitudeValue =
            case model.geolocation of
                Just geolocation ->
                    Encode.float geolocation.latitude

                Nothing ->
                    Encode.null

        longitudeValue =
            case model.geolocation of
                Just geolocation ->
                    Encode.float geolocation.longitude

                Nothing ->
                    Encode.null
    in
    Http.post
        { url = collectUrl
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "action_type", Encode.string actionType )
                    , ( "name", Encode.string item.title )
                    , ( "item_id", Encode.string item.id )
                    , ( "latitude", latitudeValue )
                    , ( "longitude", longitudeValue )
                    , ( "user_agent", Encode.string model.userAgent )
                    , ( "user_key", Encode.string model.apiKey )
                    ]
                )
        , expect = Http.expectWhatever CollectResponseReceived
        }


filterTagsFromNames : List String -> List FilterTag
filterTagsFromNames tagNames =
    List.map (\tag -> FilterTag tag False) tagNames


mergeFilterTags : List FilterTag -> List FilterTag -> List FilterTag
mergeFilterTags oldTags newTags =
    let
        oldNames =
            List.map .tag oldTags

        newNames =
            List.map .tag newTags

        additionalTags =
            List.filter (\tag -> not (List.member tag.tag oldNames)) newTags

        tagsToKeep =
            List.filter (\tag -> List.member tag.tag newNames) oldTags
    in
    tagsToKeep ++ additionalTags


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ItemsReceived payload ->
            case payload of
                Ok rawString ->
                    let
                        serverItems =
                            ItemData.itemListToDict (List.indexedMap ItemData.receivedToItem (ItemData.parseReceivedItems rawString))

                        items =
                            mergeIntoItemDict serverItems model.items

                        newFilterTags =
                            mergeFilterTags model.filterTags (filterTagsFromNames (filterTagNames items))

                        newModel =
                            { model | items = items, filterTags = newFilterTags }
                    in
                    ( newModel
                    , Cmd.batch
                        ((if model.overrideOrdering then
                            [ callSortAPI (Dict.values items) ]

                          else
                            []
                         )
                            ++ [ writeToLocalStorage (encodeModel newModel) ]
                        )
                    )

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
                    Dict.update itemId (\item -> toggleDone item |> setSynced False) model.items

                maybeItem =
                    Dict.get itemId newItems

                newModel =
                    { model | items = newItems }
            in
            ( newModel
            , Cmd.batch
                ((case maybeItem of
                    Just item ->
                        [ updateDoneBackend model.apiKey itemId item.done
                        , postCollectEvent model item
                        ]

                    Nothing ->
                        []
                 )
                    ++ [ writeToLocalStorage (encodeModel newModel) ]
                )
            )

        EditCardClicked itemId ->
            let
                newModel =
                    { model | items = Dict.update itemId toggleEdit model.items }
            in
            ( newModel, writeToLocalStorage (encodeModel newModel) )

        CancelEditing itemId ->
            let
                maybeItem =
                    Dict.get itemId model.items
            in
            case maybeItem of
                Just item ->
                    let
                        newModel =
                            if item.new then
                                { model | items = Dict.remove itemId model.items }

                            else
                                { model | items = Dict.update itemId toggleEdit model.items }
                    in
                    ( newModel, writeToLocalStorage (encodeModel newModel) )

                Nothing ->
                    ( model, Cmd.none )

        FinishEditing itemId ->
            case Dict.get itemId model.items of
                Just item ->
                    if not item.draftChanged then
                        let
                            newModel =
                                { model | items = Dict.update itemId toggleEdit model.items }
                        in
                        ( newModel, writeToLocalStorage (encodeModel newModel) )

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
                                    , editing = False
                                    , new = False
                                    , synced = False
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
                                mergeFilterTags model.filterTags (filterTagsFromNames (filterTagNames newItems))

                            newModel =
                                { model
                                    | items = newItems
                                    , filterTags = newFilters
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            ((if item.new then
                                postItem model.apiKey model.clientId updatedItem

                              else
                                updateItem model.apiKey updatedItem
                             )
                                :: ((if model.overrideOrdering then
                                        callSortAPI (Dict.values newItems)

                                     else
                                        Cmd.none
                                    )
                                        :: [ writeToLocalStorage (encodeModel newModel) ]
                                   )
                            )
                        )

                Nothing ->
                    ( model, Cmd.none )

        AddNewCardClicked ->
            ( model, Random.generate AddNewCard UUID.generator )

        AddNewCard newUUID ->
            let
                newId =
                    "local-" ++ UUID.toString newUUID

                newModel =
                    { model | items = addNewItem newId model.items }
            in
            ( newModel
            , Cmd.batch
                [ resetViewport
                , writeToLocalStorage (encodeModel newModel)
                ]
            )

        DraftTitleChanged itemId newTitle ->
            let
                newModel =
                    { model | items = Dict.update itemId (updateDraftTitle newTitle) model.items }
            in
            ( newModel, writeToLocalStorage (encodeModel newModel) )

        SortButtonClicked ->
            if model.overrideOrdering then
                let
                    newModel =
                        { model | overrideOrdering = False }
                in
                ( newModel, writeToLocalStorage (encodeModel newModel) )

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

                        newModel =
                            { model | items = Dict.map (updateOverrideOrderIndex idToIndexDict) model.items, overrideOrdering = True }
                    in
                    ( newModel, writeToLocalStorage (encodeModel newModel) )

                Err httpError ->
                    ( model, Cmd.none )

        DoneResponseReceived itemId success ->
            let
                newModel =
                    case success of
                        Ok successValue ->
                            if successValue then
                                { model | items = Dict.update itemId (setSynced True) model.items }

                            else
                                model

                        Err httpError ->
                            model
            in
            ( newModel, writeToLocalStorage (encodeModel newModel) )

        UpdateResponseReceived itemId updatePayload ->
            case updatePayload of
                Ok updateResponse ->
                    let
                        newItemDict =
                            Dict.update itemId
                                (\maybeItem ->
                                    case maybeItem of
                                        Just item ->
                                            Just { item | synced = True, lastSyncedRevision = updateResponse.revision }

                                        Nothing ->
                                            Nothing
                                )
                                model.items

                        newModel =
                            { model | items = newItemDict }
                    in
                    ( newModel
                    , writeToLocalStorage (encodeModel newModel)
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
                        let
                            newItems =
                                Dict.remove itemId model.items

                            newFilters =
                                mergeFilterTags model.filterTags (filterTagsFromNames (filterTagNames newItems))

                            newModel =
                                { model | items = newItems, filterTags = newFilters }
                        in
                        ( newModel, writeToLocalStorage (encodeModel newModel) )

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
                    let
                        newModel =
                            { model
                                | items = Dict.update mqttData.id (setDone mqttData.status) model.items
                            }
                    in
                    ( newModel
                    , writeToLocalStorage (encodeModel newModel)
                    )

                Nothing ->
                    ( model, Cmd.none )

        ReceivedMQTTMessageNewItem mqttMsg ->
            let
                maybeReceivedItem =
                    parseMQTTMessageNewItem mqttMsg

                maybeClientId =
                    parseMQTTClientId mqttMsg

                processMessage =
                    case maybeClientId of
                        Just clientId ->
                            if clientId == model.clientId then
                                False

                            else
                                True

                        Nothing ->
                            True

                newItems =
                    if processMessage then
                        case maybeReceivedItem of
                            Just receivedItem ->
                                addReceivedItem receivedItem model.items

                            Nothing ->
                                model.items

                    else
                        model.items

                newModel =
                    { model | items = newItems }
            in
            ( newModel
            , Cmd.batch
                ((if model.overrideOrdering then
                    callSortAPI (Dict.values newItems)

                  else
                    Cmd.none
                 )
                    :: [ writeToLocalStorage (encodeModel newModel) ]
                )
            )

        ItemPosted oldId postResponsePayload ->
            case postResponsePayload of
                Ok postResponse ->
                    let
                        maybeOldItem =
                            Dict.get oldId model.items

                        newItems =
                            case maybeOldItem of
                                Just oldItem ->
                                    Dict.remove oldId model.items
                                        |> Dict.insert postResponse.newId { oldItem | id = postResponse.newId, synced = True, lastSyncedRevision = postResponse.revision }

                                Nothing ->
                                    model.items

                        newModel =
                            { model | items = newItems }
                    in
                    ( newModel
                    , writeToLocalStorage (encodeModel newModel)
                    )

                Err httpError ->
                    ( model, Cmd.none )

        DraftTagsChanged itemId newTagList ->
            let
                newModel =
                    { model | items = Dict.update itemId (updateDraftTags newTagList) model.items }
            in
            ( newModel, writeToLocalStorage (encodeModel newModel) )

        DraftTagsInputChanged itemId remainingText ->
            let
                newModel =
                    { model | items = Dict.update itemId (updateDraftTagsInput remainingText) model.items }
            in
            ( newModel, writeToLocalStorage (encodeModel newModel) )

        DeleteItem itemId ->
            ( model, deleteItem model.apiKey itemId )

        ReceivedMQTTMessageDeletedItem mqttMsg ->
            let
                maybeItemId =
                    parseMQTTMessageItemDeleted mqttMsg

                newItems =
                    case maybeItemId of
                        Just itemId ->
                            Dict.remove itemId model.items

                        Nothing ->
                            model.items

                newFilters =
                    mergeFilterTags model.filterTags (filterTagsFromNames (filterTagNames newItems))

                newModel =
                    { model | items = newItems, filterTags = newFilters }
            in
            ( newModel, writeToLocalStorage (encodeModel newModel) )

        ReceivedMQTTMessageUpdatedItem mqttMsg ->
            let
                maybeUpdatedItem =
                    parseMQTTMessageNewItem mqttMsg

                newItems =
                    case maybeUpdatedItem of
                        Just updatedItem ->
                            updateFromReceivedItem updatedItem model.items

                        Nothing ->
                            model.items

                newFilters =
                    mergeFilterTags model.filterTags (filterTagsFromNames (filterTagNames newItems))

                newModel =
                    { model | items = newItems, filterTags = newFilters }
            in
            ( newModel, writeToLocalStorage (encodeModel newModel) )

        ReceivedGeolocation portMsg ->
            case parseGeolocation portMsg of
                Just geolocation ->
                    let
                        newModel =
                            { model | geolocation = Just geolocation }
                    in
                    ( newModel, writeToLocalStorage (encodeModel newModel) )

                Nothing ->
                    ( model, Cmd.none )

        CollectResponseReceived _ ->
            ( model, Cmd.none )

        GotFocus _ ->
            ( model, syncItems model.apiKey model.clientId (List.sortBy .orderIndexDefault (Dict.values model.items)) )

        NoTagsFilterClicked ->
            let
                newModel =
                    { model | noTagsFilterActive = not model.noTagsFilterActive }
            in
            ( newModel, writeToLocalStorage (encodeModel newModel) )

        NoOp ->
            ( model, Cmd.none )


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
            let
                newItem =
                    { item | draftTitle = newTitle }
            in
            Just { newItem | draftChanged = ItemData.draftHasChanged newItem }

        Nothing ->
            Nothing


updateDraftTags : List String -> Maybe ItemData -> Maybe ItemData
updateDraftTags newDraftTags maybeItem =
    case maybeItem of
        Just item ->
            let
                newItem =
                    { item | draftTags = newDraftTags }
            in
            Just { newItem | draftChanged = ItemData.draftHasChanged newItem }

        Nothing ->
            Nothing


updateDraftTagsInput : String -> Maybe ItemData -> Maybe ItemData
updateDraftTagsInput newText maybeItem =
    case maybeItem of
        Just item ->
            let
                newItem =
                    { item | draftTagsInput = newText }
            in
            Just { newItem | draftChanged = ItemData.draftHasChanged newItem }

        Nothing ->
            Nothing


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
            Just { item | editing = not item.editing, draftTitle = item.title, draftTags = item.tags, draftTagsInput = "", draftChanged = False }

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


setSynced : Bool -> Maybe ItemData -> Maybe ItemData
setSynced newSynced maybeItem =
    case maybeItem of
        Just item ->
            Just
                { item
                    | synced = newSynced
                }

        Nothing ->
            Nothing


setDone : Int -> Maybe ItemData -> Maybe ItemData
setDone newStatus maybeItem =
    case maybeItem of
        Just item ->
            Just
                { item
                    | done = newStatus
                }

        Nothing ->
            Nothing


addNewItem : String -> Dict String ItemData -> Dict String ItemData
addNewItem newId dict =
    let
        newIndex =
            case ItemData.maxOrderIndex (Dict.values dict) of
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
        , draftChanged = False
        , done = 0
        , orderIndexDefault = newIndex
        , orderIndexOverride = newIndex
        , editing = True
        , synced = False
        , new = True
        , lastSyncedRevision = -1
        , oldId = ""
        }
        dict


addReceivedItem : ItemDataReceived -> Dict String ItemData -> Dict String ItemData
addReceivedItem itemDataReceived dict =
    let
        newIndex =
            case ItemData.maxOrderIndex (Dict.values dict) of
                Just maxIndex ->
                    maxIndex + 1

                Nothing ->
                    0

        itemData =
            ItemData.receivedToItem newIndex itemDataReceived
    in
    Dict.insert itemData.id itemData dict


updateFromReceivedItem : ItemDataReceived -> Dict String ItemData -> Dict String ItemData
updateFromReceivedItem itemDataReceived dict =
    Dict.update itemDataReceived.id
        (\maybeItem ->
            case maybeItem of
                Just item ->
                    if item.editing then
                        Just item

                    else
                        Just { item | title = itemDataReceived.title, tags = itemDataReceived.tags, done = itemDataReceived.done }

                Nothing ->
                    Nothing
        )
        dict



-- SUBSCRIPTIONS


type alias ChipsInitArgs =
    { parentSelector : String
    , tags : List String
    }


port receiveMQTTMessageDoneStatus : (String -> msg) -> Sub msg


port receiveMQTTMessageNewItem : (String -> msg) -> Sub msg


port receiveMQTTMessageDeletedItem : (String -> msg) -> Sub msg


port receiveMQTTMessageUpdatedItem : (String -> msg) -> Sub msg


port receiveGeolocation : (Decode.Value -> msg) -> Sub msg


port writeToLocalStorage : Encode.Value -> Cmd msg


port gotFocus : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveMQTTMessageDoneStatus ReceivedMQTTMessageDoneStatus
        , receiveMQTTMessageNewItem ReceivedMQTTMessageNewItem
        , receiveMQTTMessageDeletedItem ReceivedMQTTMessageDeletedItem
        , receiveMQTTMessageUpdatedItem ReceivedMQTTMessageUpdatedItem
        , receiveGeolocation ReceivedGeolocation
        , gotFocus GotFocus
        ]


type alias Geolocation =
    { latitude : Float, longitude : Float }


parseGeolocation : Decode.Value -> Maybe Geolocation
parseGeolocation portMsg =
    case Decode.decodeValue (Decode.map2 Geolocation (Decode.field "latitude" Decode.float) (Decode.field "longitude" Decode.float)) portMsg of
        Ok geolocation ->
            Just geolocation

        Err _ ->
            Nothing


parseMQTTClientId : String -> Maybe String
parseMQTTClientId rawString =
    case Decode.decodeString (Decode.field "clientId" Decode.string) rawString of
        Ok clientId ->
            Just clientId

        Err _ ->
            Nothing


parseMQTTMessageDoneStatus : String -> Maybe MqttMessageDoneStatus
parseMQTTMessageDoneStatus rawString =
    case Decode.decodeString (Decode.map2 MqttMessageDoneStatus (Decode.field "id" Decode.string) (Decode.field "status" Decode.int)) rawString of
        Ok mqttData ->
            Just mqttData

        Err _ ->
            Nothing


parseMQTTMessageNewItem : String -> Maybe ItemDataReceived
parseMQTTMessageNewItem rawString =
    case Decode.decodeString ItemData.jsonParseItemDataReceived rawString of
        Ok itemDataReceived ->
            Just itemDataReceived

        Err _ ->
            Nothing


parseMQTTMessageItemDeleted : String -> Maybe String
parseMQTTMessageItemDeleted rawString =
    case Decode.decodeString (Decode.field "id" Decode.string) rawString of
        Ok itemId ->
            Just itemId

        Err _ ->
            Nothing



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ headerView model
        , main_ [ Aria.ariaLabel "Listenbereich" ]
            [ div [ class "container" ]
                [ itemCardsView model
                , addCardButton
                ]
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
                    ([ noTagsFilterView model.noTagsFilterActive ] ++ List.map filterTagChip (FilterTag.sort model.filterTags))
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


noTagsFilterView : Bool -> Html Msg
noTagsFilterView isActive =
    div
        [ class
            ("chip green-text green lighten-5"
                ++ (case isActive of
                        True ->
                            " darken-1 white-text"

                        False ->
                            ""
                   )
            )
        , onClick NoTagsFilterClicked
        ]
        [ text "No tags" ]


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
    div [ class "row" ]
        [ div [ class "col s12 l8 offset-l2" ]
            (List.map (cardView (List.map .tag model.filterTags)) (itemsToShow model))
        ]


cardView : List String -> ItemData -> Html Msg
cardView filterTags itemData =
    if itemData.editing then
        editCard filterTags itemData

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
            [ span [ class "right" ]
                [ if itemData.synced then
                    text ""

                  else
                    desyncIcon
                , editButton itemData
                ]
            , span
                [ class
                    ("card-title prevent-select"
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


desyncIcon : Html Msg
desyncIcon =
    i [ class "material-icons grey-text right-align desync-icon" ] [ text "cloud_off" ]


editCard : List String -> ItemData -> Html Msg
editCard filterTags item =
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
                   , editChipsView filterTags item
                   , div [ class "card-action valign-wrapper justify-right" ]
                        [ cancelButton item.id
                        , a [ class "green btn finish-edit", Aria.role "button", Aria.ariaLabel "Bestätigen", onClick (FinishEditing item.id) ] [ i [ class "material-icons" ] [ text "check" ] ]
                        ]
                   ]
            )
        ]


editChipsView : List String -> ItemData -> Html Msg
editChipsView filterTags item =
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
        , attribute "autocompleteOptions" (Encode.encode 0 (Encode.list Encode.string filterTags))
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
        , onWithOptions "click" { preventDefault = True, stopPropagation = True } (\event -> DeleteItem itemId)
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


isVisible : Model -> ItemData -> Bool
isVisible model item =
    let
        filters =
            FilterTag.activeTags model.filterTags

        filteringActive =
            (List.length filters > 0) || model.noTagsFilterActive
    in
    if not filteringActive || item.editing || (model.noTagsFilterActive && (List.length item.tags == 0)) then
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


filterTagNames : Dict String ItemData -> List String
filterTagNames items =
    Set.toList (ItemData.uniqueTags (Dict.values items))


sortAPIResponseDecoder : Decode.Decoder (List Int)
sortAPIResponseDecoder =
    Decode.field "sort_indices" (Decode.list Decode.int)


argsort : List comparable -> List Int
argsort l =
    List.indexedMap Tuple.pair l
        |> List.sortBy Tuple.second
        |> List.map Tuple.first


encodeFilterTag : FilterTag -> Encode.Value
encodeFilterTag { tag, isActive } =
    Encode.object
        [ ( "tag", Encode.string tag )
        , ( "isActive", Encode.bool isActive )
        ]


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)



-- DICT MERGING


newOnly : Dict String ItemData -> String -> ItemData -> Dict String ItemData -> Dict String ItemData
newOnly oldDict key val res =
    case Dict.get val.oldId oldDict of
        Just oldItem ->
            Dict.insert key
                { oldItem
                    | id = key
                    , oldId = oldItem.id
                    , title = val.title
                    , tags = val.tags
                    , done = val.done
                    , orderIndexDefault = val.orderIndexDefault
                    , lastSyncedRevision = val.lastSyncedRevision
                    , synced = True
                }
                res

        Nothing ->
            Dict.insert key val res


both : String -> ItemData -> ItemData -> Dict String ItemData -> Dict String ItemData
both key valLeft valRight res =
    if valRight.editing then
        Dict.insert key valRight res

    else
        Dict.insert key
            { valRight
                | title = valLeft.title
                , tags = valLeft.tags
                , done = valLeft.done
                , orderIndexDefault = valLeft.orderIndexDefault
                , lastSyncedRevision = valLeft.lastSyncedRevision
                , synced = True
            }
            res


oldOnly : String -> ItemData -> Dict String ItemData -> Dict String ItemData
oldOnly key val res =
    res


mergeIntoItemDict : Dict String ItemData -> Dict String ItemData -> Dict String ItemData
mergeIntoItemDict newDict oldDict =
    Dict.merge (newOnly oldDict) both oldOnly newDict oldDict Dict.empty
