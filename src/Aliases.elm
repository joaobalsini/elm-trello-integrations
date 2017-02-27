module Aliases exposing (..)


type alias Board =
    { id : String
    , name : String
    , desc : String
    , lists : List TrelloList
    }


type alias TrelloList =
    { id : String
    , name : String
    , cards : List TrelloCard
    }


type alias TrelloCard =
    { id : String
    , name : String
    }
