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
    , boardId : String
    }


type alias TrelloCard =
    { id : String
    , activityId : Maybe String
    , name : String
    , desc : String
    , listId : String
    , boardId : String
    , labels : List TrelloLabel
    , attachments : List TrelloAttachment
    }


type alias TrelloListPlusBoardId =
    { trelloList : List TrelloList
    , boardId : String
    }


type alias TrelloLabelPlusBoardId =
    { trelloLabel : List TrelloLabel
    , boardId : String
    }


type alias TrelloCardPlusListIdPlusBoardId =
    { trelloCard : TrelloCard
    , listId : String
    , boardId : String
    }


type alias TrelloCardIdPlusListIdPlusBoardId =
    { trelloCardId : String
    , listId : String
    , boardId : String
    }


type alias TrelloCardPlusListIdPlusActivityId =
    { trelloCard : TrelloCard
    , listId : String
    , activityId : String
    }


initTrelloCard : TrelloCard
initTrelloCard =
    { id = ""
    , activityId = Nothing
    , name = ""
    , desc = ""
    , listId = ""
    , boardId = ""
    , labels = []
    , attachments = []
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
