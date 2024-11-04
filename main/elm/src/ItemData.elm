module ItemData exposing (..)

import Dict exposing (Dict)
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
