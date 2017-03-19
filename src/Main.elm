port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import Routes exposing (..)
import Message exposing (..)
import TrelloBoard
import Activity
import ActivityGroup
import Date


-- import ActivityGroup

import Aliases exposing (..)


main : Program Never Model Msg
main =
    Navigation.program locationToMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- model


type alias Model =
    { route : Route
    , lastRoute : Route
    , trelloBoard : TrelloBoard.Model
    , activity : Activity.Model
    , activityGroup : ActivityGroup.Model
    , message : Message.Message
    , trelloAuthorized : Bool
    , boards : List TrelloBoard
    , activities : List Activity
    , activityGroups : List ActivityGroup
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


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            locationToRoute location

        ( trelloBoardInitModel, trelloBoardCmd ) =
            TrelloBoard.init

        ( activityInitModel, activityCmd ) =
            Activity.init

        ( activityGroupInitModel, activityGroupCmd ) =
            ActivityGroup.init

        ( messageInitModel, messageCmd ) =
            Message.init

        initModel =
            { route = route
            , lastRoute = TrelloBoardRoute
            , trelloBoard = trelloBoardInitModel
            , activity = activityInitModel
            , activityGroup = activityGroupInitModel
            , message = Message.initMessage
            , trelloAuthorized = False
            , boards = []
            , activities = []
            , activityGroups = []
            }

        cmds =
            Cmd.batch
                [ Cmd.map TrelloBoardMsg trelloBoardCmd
                , Cmd.map ActivityMsg activityCmd
                , Cmd.map ActivityGroupMsg activityGroupCmd
                , Cmd.map MessageMsg messageCmd
                ]
    in
        ( initModel, cmds )



-- this function is triggered whenever the user changes the url


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location
        |> locationToRoute
        |> ChangePage


getBoardByIdFromList : List TrelloBoard -> String -> Maybe TrelloBoard
getBoardByIdFromList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getBoardByIdFromList xs id


getTrelloListByIdFromList : List TrelloList -> String -> Maybe TrelloList
getTrelloListByIdFromList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getTrelloListByIdFromList xs id


getTrelloCardByIdFromList : List TrelloCard -> String -> Maybe TrelloCard
getTrelloCardByIdFromList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getTrelloCardByIdFromList xs id


updateBoardAtId : TrelloBoard -> String -> TrelloBoard -> TrelloBoard
updateBoardAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


updateTrelloListAtId : TrelloList -> String -> TrelloList -> TrelloList
updateTrelloListAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


updateTrelloCardAtId : TrelloCard -> String -> TrelloCard -> TrelloCard
updateTrelloCardAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


updateActivityAtId : Activity -> String -> Activity -> Activity
updateActivityAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


updateActivityGroupAtId : ActivityGroup -> String -> ActivityGroup -> ActivityGroup
updateActivityGroupAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


updateActivityCardsForGivenCardsAndActivity : List TrelloCard -> Activity -> Activity
updateActivityCardsForGivenCardsAndActivity cards originalActivity =
    let
        trelloCards =
            (List.filter (cardBelongsToActivity originalActivity) cards)

        count =
            Debug.log ("TrelloCards for id: " ++ originalActivity.id) (List.length trelloCards)
    in
        { originalActivity | trelloCards = trelloCards }


cardBelongsToActivity : Activity -> TrelloCard -> Bool
cardBelongsToActivity searchedActivity trelloCard =
    case trelloCard.activityId of
        Nothing ->
            False

        Just cardActivityId ->
            if searchedActivity.id == cardActivityId then
                True
            else
                False



-- update


type Msg
    = Navigate Route
    | ChangePage Route
    | Authorize
    | Authorized String
    | Deauthorize
    | LoadBoards
    | BoardsLoaded (List TrelloBoard)
    | ListsLoaded TrelloListPlusBoardId
    | LabelsLoaded TrelloLabelPlusBoardId
    | CardLoaded TrelloCardPlusListIdPlusBoardId
    | DateLoaded String
    | TrelloBoardMsg TrelloBoard.Msg
    | ActivityMsg Activity.Msg
    | ActivityGroupMsg ActivityGroup.Msg
    | MessageMsg Message.Msg


port authorizeTrello : () -> Cmd msg


port deauthorizeTrello : () -> Cmd msg


port loadBoards : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Navigate is used once a user clicks in a link
        Navigate route ->
            let
                ( model_, msg_ ) =
                    update (ChangePage route) model
            in
                ( model_, Navigation.newUrl <| routeToHash route )

        -- ChangePage is used once a user changes the URL manually
        ChangePage route ->
            let
                lastRoute =
                    model.route
            in
                ( { model | route = route, lastRoute = lastRoute }, Cmd.none )

        Authorize ->
            ( model, authorizeTrello () )

        Authorized string ->
            ( { model | message = successMessage string, trelloAuthorized = True }, loadBoards () )

        Deauthorize ->
            ( { model | message = successMessage "Successfully deauthorized!", trelloAuthorized = False }, deauthorizeTrello () )

        LoadBoards ->
            if model.trelloAuthorized then
                ( model, loadBoards () )
            else
                ( { model | message = errorMessage "You should authorize trello first" }, Cmd.none )

        BoardsLoaded boards ->
            ( { model | boards = boards }, Cmd.none )

        DateLoaded string ->
            let
                activityModel =
                    model.activity

                newActivityModel =
                    { activityModel | actualDate = Result.toMaybe (Date.fromString string) }
            in
                ( { model | activity = newActivityModel }, Cmd.none )

        ListsLoaded { trelloList, boardId } ->
            let
                oldBoard =
                    getBoardByIdFromList model.boards boardId

                -- update boards list and selectedBoard
                ( newBoards, selectedBoard ) =
                    case oldBoard of
                        Nothing ->
                            ( model.boards, Nothing )

                        Just board ->
                            let
                                updatedBoard =
                                    { board | lists = trelloList }

                                selectedBoard =
                                    case model.trelloBoard.selectedBoard of
                                        Nothing ->
                                            model.trelloBoard.selectedBoard

                                        Just board ->
                                            if board.id == boardId then
                                                Just updatedBoard
                                            else
                                                Just board
                            in
                                ( List.map (updateBoardAtId updatedBoard boardId) model.boards, selectedBoard )

                currentTrelloBoard =
                    model.trelloBoard

                updatedTrelloBoard =
                    { currentTrelloBoard | selectedBoard = selectedBoard }

                -- ok, we need to go through all the cards and update activities information, i.e. what are the activity's cards if any...
                cards =
                    case selectedBoard of
                        Nothing ->
                            []

                        Just board ->
                            List.concatMap (.cards) board.lists

                updatedActivities =
                    List.map (updateActivityCardsForGivenCardsAndActivity cards) model.activities
            in
                ( { model | boards = newBoards, trelloBoard = updatedTrelloBoard, activities = updatedActivities }
                , Cmd.none
                )

        LabelsLoaded { trelloLabel, boardId } ->
            let
                oldBoard =
                    getBoardByIdFromList model.boards boardId

                -- update boards labels and selectedBoard
                ( newBoards, selectedBoard ) =
                    case oldBoard of
                        Nothing ->
                            ( model.boards, Nothing )

                        Just board ->
                            let
                                updatedBoard =
                                    { board | labels = trelloLabel }

                                selectedBoard =
                                    case model.trelloBoard.selectedBoard of
                                        Nothing ->
                                            model.trelloBoard.selectedBoard

                                        Just board ->
                                            if board.id == boardId then
                                                Just updatedBoard
                                            else
                                                Just board
                            in
                                ( List.map (updateBoardAtId updatedBoard boardId) model.boards, selectedBoard )

                currentTrelloBoard =
                    model.trelloBoard

                updatedTrelloBoard =
                    { currentTrelloBoard | selectedBoard = selectedBoard }
            in
                ( { model | boards = newBoards, trelloBoard = updatedTrelloBoard }
                , Cmd.none
                )

        CardLoaded { trelloCard, listId, boardId } ->
            -- here we need to update the list in the List TrelloList inside Board
            -- and then Board inside List Board
            let
                oldBoard =
                    getBoardByIdFromList model.boards boardId

                ( newBoards, selectedBoard, selectedCard ) =
                    case oldBoard of
                        Nothing ->
                            ( model.boards, Nothing, Nothing )

                        Just board ->
                            let
                                oldList =
                                    getTrelloListByIdFromList board.lists listId
                            in
                                case oldList of
                                    Nothing ->
                                        ( model.boards, Nothing, Nothing )

                                    Just list ->
                                        let
                                            updatedList =
                                                { list | cards = List.map (updateTrelloCardAtId trelloCard trelloCard.id) list.cards }

                                            newLists =
                                                List.map (updateTrelloListAtId updatedList listId) board.lists

                                            updatedBoard =
                                                { board | lists = newLists }

                                            selectedBoard =
                                                case model.trelloBoard.selectedBoard of
                                                    Nothing ->
                                                        model.trelloBoard.selectedBoard

                                                    Just board ->
                                                        if board.id == boardId then
                                                            Just updatedBoard
                                                        else
                                                            Just board

                                            selectedCard =
                                                case model.trelloBoard.selectedCard of
                                                    Nothing ->
                                                        model.trelloBoard.selectedCard

                                                    Just card ->
                                                        if card.id == trelloCard.id then
                                                            Just trelloCard
                                                        else
                                                            Just card

                                            newBoards =
                                                List.map (updateBoardAtId updatedBoard boardId) model.boards
                                        in
                                            ( newBoards, selectedBoard, selectedCard )

                currentTrelloBoard =
                    model.trelloBoard

                updatedTrelloBoard =
                    { currentTrelloBoard | selectedBoard = selectedBoard, selectedCard = selectedCard }
            in
                ( { model | boards = newBoards, trelloBoard = updatedTrelloBoard }
                , Cmd.none
                )

        TrelloBoardMsg msg ->
            case msg of
                TrelloBoard.LoadBoards ->
                    update LoadBoards model

                TrelloBoard.Authorize ->
                    update Authorize model

                _ ->
                    let
                        ( trelloBoardModel, cmd, message ) =
                            TrelloBoard.update msg model.trelloBoard
                    in
                        ( { model | trelloBoard = trelloBoardModel, message = message }
                        , Cmd.map TrelloBoardMsg cmd
                        )

        ActivityMsg msg ->
            -- Here we handle the units list, below we do the same for the materials list --
            case msg of
                -- We "intercept" the message to activity, in case its Added, we add the actiivty to the acvities list and pass the message to activity module
                Activity.ActivityAdded activity ->
                    let
                        newActivities =
                            activity :: model.activities

                        ( activityModel, cmd, message ) =
                            Activity.update msg model.activity
                    in
                        ( { model | activity = activityModel, message = message, activities = newActivities }
                        , Cmd.map ActivityMsg cmd
                        )

                -- in case its Updated we update the unit in the units list and pass the message to unit module
                Activity.ActivityUpdated updatedActivity ->
                    let
                        newActivities =
                            List.map (updateActivityAtId updatedActivity updatedActivity.id) model.activities

                        ( activityModel, cmd, message ) =
                            Activity.update msg model.activity
                    in
                        ( { model | activity = activityModel, message = message, activities = newActivities }
                        , Cmd.map ActivityMsg cmd
                        )

                -- in case its Removed we remove the unit from the units list and pass the message to unit module
                Activity.ActivityRemoved id ->
                    let
                        newActivities =
                            List.filter (\activity -> activity.id /= id)
                                model.activities

                        ( activityModel, cmd, message ) =
                            Activity.update msg model.activity
                    in
                        ( { model | activity = activityModel, message = message, activities = newActivities }
                        , Cmd.map ActivityMsg cmd
                        )

                Activity.TrelloCardAdded trelloCard ->
                    let
                        ( activityModel, cmd, message ) =
                            Activity.update msg model.activity
                    in
                        ( { model | activity = activityModel, message = message }
                        , Cmd.map ActivityMsg cmd
                        )

                -- otherwise we just pass the message to unit module
                _ ->
                    let
                        ( activityModel, cmd, message ) =
                            Activity.update msg model.activity
                    in
                        ( { model | activity = activityModel, message = message }
                        , Cmd.map ActivityMsg cmd
                        )

        ActivityGroupMsg msg ->
            -- Here we handle the units list, below we do the same for the materials list --
            case msg of
                -- We "intercept" the message to activity, in case its Added, we add the actiivty to the acvities list and pass the message to activity module
                ActivityGroup.ActivityGroupAdded activityGroup ->
                    let
                        newActivityGroups =
                            activityGroup :: model.activityGroups

                        ( activityGroupModel, cmd, message ) =
                            ActivityGroup.update msg model.activityGroup
                    in
                        ( { model | activityGroup = activityGroupModel, message = message, activityGroups = newActivityGroups }
                        , Cmd.map ActivityGroupMsg cmd
                        )

                -- in case its Updated we update the unit in the units list and pass the message to unit module
                ActivityGroup.ActivityGroupUpdated updatedActivityGroup ->
                    let
                        newActivityGroups =
                            List.map (updateActivityGroupAtId updatedActivityGroup updatedActivityGroup.id) model.activityGroups

                        ( activityGroupModel, cmd, message ) =
                            ActivityGroup.update msg model.activityGroup
                    in
                        ( { model | activityGroup = activityGroupModel, message = message, activityGroups = newActivityGroups }
                        , Cmd.map ActivityGroupMsg cmd
                        )

                -- in case its Removed we remove the unit from the units list and pass the message to unit module
                ActivityGroup.ActivityGroupRemoved id ->
                    let
                        newActivityGroups =
                            List.filter (\activityGroup -> activityGroup.id /= id)
                                model.activityGroups

                        ( activityGroupModel, cmd, message ) =
                            ActivityGroup.update msg model.activityGroup
                    in
                        ( { model | activityGroup = activityGroupModel, message = message, activityGroups = newActivityGroups }
                        , Cmd.map ActivityGroupMsg cmd
                        )

                -- otherwise we just pass the message to unit module
                _ ->
                    let
                        ( activityGroupModel, cmd, message ) =
                            ActivityGroup.update msg model.activityGroup
                    in
                        ( { model | activityGroup = activityGroupModel, message = message }
                        , Cmd.map ActivityGroupMsg cmd
                        )

        MessageMsg msg ->
            let
                ( messageModel, cmd ) =
                    Message.update msg model.message
            in
                ( { model | message = messageModel }
                , Cmd.map MessageMsg cmd
                )


view : Model -> Html Msg
view model =
    let
        -- get the page through the view method of each Module passing the parameters needed and render that page
        page =
            case model.route of
                TrelloBoardRoute ->
                    Html.map TrelloBoardMsg
                        (TrelloBoard.view model.trelloBoard model.boards model.trelloAuthorized)

                ActivityRoute ->
                    Html.map ActivityMsg
                        (Activity.view model.activity model.activities model.activityGroups model.trelloBoard.selectedBoard)

                ActivityGroupRoute ->
                    Html.map ActivityGroupMsg
                        (ActivityGroup.view model.activityGroup model.activityGroups)

                NotFoundRoute ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Page Not Found!" ]
                        ]
    in
        div []
            [ div [ class "ui fixed inverted menu" ] [ pageHeader model ]
            , Html.map MessageMsg (Message.view model.message)
            , div [ class "ui main text container" ] [ page ]
            , confirmModalView model
            ]


pageHeader : Model -> Html Msg
pageHeader model =
    let
        menuOptions =
            if model.trelloBoard.selectedBoard /= Nothing then
                [ a [ class "item", onClick (Navigate TrelloBoardRoute) ] [ text "Show Trello Board" ]
                , a [ class "item", onClick (Navigate ActivityRoute) ] [ text "Activities" ]
                , a [ class "item", onClick (Navigate ActivityGroupRoute) ] [ text "Activity Groups" ]
                , a [ class "item right", onClick (Deauthorize) ] [ text "Deauthorize Trello" ]
                ]
            else if model.trelloAuthorized then
                [ a [ class "item", onClick (Navigate TrelloBoardRoute) ] [ text "Trello Board select" ]
                , a [ class "item right", onClick (Deauthorize) ] [ text "Deauthorize Trello" ]
                ]
            else
                [ a [ class "item right", onClick (Authorize) ] [ text "Authorize and select board" ] ]
    in
        div [ class "ui container" ]
            menuOptions


confirmModalView : Model -> Html Msg
confirmModalView model =
    div [ class "ui modal hidden" ]
        [ div [ class "ui header" ] [ text "Are you sure?" ]
        , div [ class "actions" ]
            [ div [ class "ui red cancel button" ] [ text "Nope" ]
            , div [ class "ui green ok button" ] [ text "Yep" ]
            ]
        ]



-- subscriptions


port trelloAuthorized : (String -> msg) -> Sub msg


port boardsLoaded : (List TrelloBoard -> msg) -> Sub msg


port listsLoaded : (TrelloListPlusBoardId -> msg) -> Sub msg


port labelsLoaded : (TrelloLabelPlusBoardId -> msg) -> Sub msg


port cardLoaded : (TrelloCardPlusListIdPlusBoardId -> msg) -> Sub msg


port actualDateLoaded : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        trelloBoardSub =
            TrelloBoard.subscriptions model.trelloBoard

        messageSub =
            Message.subscriptions model.message

        activitySub =
            Activity.subscriptions model.activity

        activityGroupSub =
            ActivityGroup.subscriptions model.activityGroup
    in
        Sub.batch
            [ Sub.map TrelloBoardMsg trelloBoardSub
            , Sub.map MessageMsg messageSub
            , Sub.map ActivityMsg activitySub
            , Sub.map ActivityGroupMsg activityGroupSub
            , trelloAuthorized Authorized
            , boardsLoaded BoardsLoaded
            , listsLoaded ListsLoaded
            , labelsLoaded LabelsLoaded
            , cardLoaded CardLoaded
            , actualDateLoaded DateLoaded
            ]
