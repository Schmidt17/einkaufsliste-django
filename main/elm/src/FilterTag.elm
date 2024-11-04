module FilterTag exposing (..)

import Json.Encode as Encode


type alias FilterTag =
    { tag : String
    , isActive : Bool
    }


encode : FilterTag -> Encode.Value
encode { tag, isActive } =
    Encode.object
        [ ( "tag", Encode.string tag )
        , ( "isActive", Encode.bool isActive )
        ]


maybeActiveTag : FilterTag -> Maybe String
maybeActiveTag filterTag =
    if filterTag.isActive then
        Just filterTag.tag

    else
        Nothing



-- LISTS OF FILTERTAGS


toggleByTag : String -> List FilterTag -> List FilterTag
toggleByTag tag tagList =
    let
        toggle tagToMatch filterTag =
            if tagToMatch == filterTag.tag then
                { filterTag | isActive = not filterTag.isActive }

            else
                filterTag
    in
    List.map (toggle tag) tagList


activeTags : List FilterTag -> List String
activeTags filterTags =
    List.filterMap maybeActiveTag filterTags


sort : List FilterTag -> List FilterTag
sort filterTags =
    List.sortBy .tag filterTags
