module Aliases exposing (..)


type alias Board =
    { id : String
    , name : String
    , desc : String
    , lists : List TrelloList
    , labels : List TrelloLabel
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


type alias TrelloLabel =
    { color : String
    , name : String
    }
