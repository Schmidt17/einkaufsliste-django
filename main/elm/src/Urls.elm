module Urls exposing (..)


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
