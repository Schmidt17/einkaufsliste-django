module FilterTag exposing (..)


type alias FilterTag =
    { tag : String
    , isActive : Bool
    }


maybeActiveTag : FilterTag -> Maybe String
maybeActiveTag filterTag =
    if filterTag.isActive then
        Just filterTag.tag

    else
        Nothing


activeTags : List FilterTag -> List String
activeTags filterTags =
    List.filterMap maybeActiveTag filterTags


sort : List FilterTag -> List FilterTag
sort filterTags =
    List.sortBy .tag filterTags
