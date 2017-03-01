module Aliases exposing (..)


type alias Board =
    { id : String
    , name : String
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
    , taskId : Maybe String
    , name : String
    , desc : String
    , labels : List TrelloLabel
    , attachments : List TrelloAttachment
    }


type alias TrelloAttachment =
    { name : String
    , url : String
    }


type alias TrelloLabel =
    { color : String
    , name : String
    }
