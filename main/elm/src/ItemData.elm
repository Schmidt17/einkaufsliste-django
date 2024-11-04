module ItemData exposing (..)

import Json.Encode as Encode


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
