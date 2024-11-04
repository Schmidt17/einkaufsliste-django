module ItemData exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Set


type alias ItemData =
    { id : String
    , title : String
    , tags : List String
    , draftTitle : String
    , draftTags : List String
    , draftTagsInput : String
    , draftChanged : Bool
    , done : Int
    , orderIndexDefault : Int
    , orderIndexOverride : Int
    , editing : Bool
    , synced : Bool
    , new : Bool
    , lastSyncedRevision : Int
    , oldId : String
    }


encode : ItemData -> Encode.Value
encode { id, title, tags, draftTitle, draftTags, draftTagsInput, draftChanged, done, orderIndexDefault, orderIndexOverride, editing, synced, new, lastSyncedRevision, oldId } =
    Encode.object
        [ ( "id", Encode.string id )
        , ( "title", Encode.string title )
        , ( "tags", Encode.list Encode.string tags )
        , ( "draftTitle", Encode.string draftTitle )
        , ( "draftTags", Encode.list Encode.string draftTags )
        , ( "draftTagsInput", Encode.string draftTagsInput )
        , ( "draftChanged", Encode.bool draftChanged )
        , ( "done", Encode.int done )
        , ( "orderIndexDefault", Encode.int orderIndexDefault )
        , ( "orderIndexOverride", Encode.int orderIndexOverride )
        , ( "editing", Encode.bool editing )
        , ( "synced", Encode.bool synced )
        , ( "new", Encode.bool new )
        , ( "lastSyncedRevision", Encode.int lastSyncedRevision )
        , ( "oldId", Encode.string oldId )
        ]


maxOrderIndex : List ItemData -> Maybe Int
maxOrderIndex items =
    List.maximum (List.map .orderIndexDefault items)


draftHasChanged : ItemData -> Bool
draftHasChanged item =
    (item.draftTitle /= item.title) || not (listEqual item.draftTags item.tags) || (String.trim item.draftTagsInput /= "")


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


uniqueTags : List ItemData -> Set.Set String
uniqueTags items =
    Set.fromList (allTags items)


allTags : List ItemData -> List String
allTags =
    List.concatMap .tags


itemListToDict : List ItemData -> Dict String ItemData
itemListToDict items =
    Dict.fromList (itemListToAssoc items)


itemListToAssoc : List ItemData -> List ( String, ItemData )
itemListToAssoc items =
    List.map (\item -> ( item.id, item )) items


type alias ItemDataReceived =
    { id : String
    , title : String
    , tags : List String
    , done : Int
    , revision : Int
    , oldId : Maybe String
    }


jsonParseItemDataReceived : Decode.Decoder ItemDataReceived
jsonParseItemDataReceived =
    Decode.map6 ItemDataReceived
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "tags" (Decode.list Decode.string))
        (Decode.field "done" Decode.int)
        (Decode.field "revision" (Decode.oneOf [ Decode.int, Decode.null 0 ]))
        (Decode.maybe (Decode.field "oldId" Decode.string))


jsonParseReceivedItemList : Decode.Decoder (List ItemDataReceived)
jsonParseReceivedItemList =
    Decode.list jsonParseItemDataReceived


parseReceivedItems : String -> List ItemDataReceived
parseReceivedItems rawString =
    case Decode.decodeString jsonParseReceivedItemList rawString of
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
    , draftChanged = False
    , done = itemReceived.done
    , orderIndexDefault = index
    , orderIndexOverride = index
    , editing = False
    , synced = True
    , new = False
    , lastSyncedRevision = itemReceived.revision
    , oldId =
        case itemReceived.oldId of
            Just oldId ->
                oldId

            Nothing ->
                ""
    }
