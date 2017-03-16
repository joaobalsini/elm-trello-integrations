module Aliases exposing (..)


type alias TrelloBoard =
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
    , activityId : Maybe String
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


type alias Activity =
    { id : String
    , name : String
    , startDate : Maybe String
    , endDate : Maybe String
    , activityGroupId : String
    , trelloCards : List TrelloCard
    }


initActivity : Activity
initActivity =
    { id = ""
    , name = ""
    , startDate = Nothing
    , endDate = Nothing
    , activityGroupId = ""
    , trelloCards = []
    }


type alias ActivityGroup =
    { id : String
    , name : String
    }


initActivityGroup : ActivityGroup
initActivityGroup =
    { id = ""
    , name = ""
    }


type alias FieldError =
    { fieldName : String
    , errorMessage : Maybe String
    }
