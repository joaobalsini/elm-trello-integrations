port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import Routes exposing (..)
import Message exposing (..)
import Index
import Login
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
    , login : Login.Model
    , index : Index.Model
    , message : Message.Message
    , trelloAuthorized : Bool
    , boards : List Board
    }


type alias TrelloListPlusBoardId =
    { trelloList : List TrelloList
    , boardId : String
    }


type alias TrelloLabelPlusBoardId =
    { trelloLabel : List TrelloLabel
    , boardId : String
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            locationToRoute location

        ( indexInitModel, indexCmd ) =
            Index.init

        ( loginInitModel, loginCmd ) =
            Login.init

        ( messageInitModel, messageCmd ) =
            Message.init

        initModel =
            { route = route
            , lastRoute = IndexRoute
            , index = indexInitModel
            , login = loginInitModel
            , message = Message.initMessage
            , trelloAuthorized = False
            , boards = []
            }

        cmds =
            Cmd.batch
                [ Cmd.map LoginMsg loginCmd
                , Cmd.map IndexMsg indexCmd
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


getBoardByIdFromList : List Board -> String -> Maybe Board
getBoardByIdFromList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getBoardByIdFromList xs id


updateBoardAtId : Board -> String -> Board -> Board
updateBoardAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement



-- update


type Msg
    = Navigate Route
    | ChangePage Route
    | Authorize
    | Authorized String
    | Deauthorize
    | LoadBoards
    | BoardsLoaded (List Board)
    | ListsLoaded TrelloListPlusBoardId
    | LabelsLoaded TrelloLabelPlusBoardId
    | IndexMsg Index.Msg
    | LoginMsg Login.Msg
    | MessageMsg Message.Msg


port authorizeTrello : () -> Cmd msg


port deauthorizeTrello : () -> Cmd msg


port loadBoards : () -> Cmd msg


port loadLists : String -> Cmd msg


port loadLabels : String -> Cmd msg


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
            let
                cmdsLoadList =
                    (List.map (\board -> loadLists board.id) boards)

                cmdsLoadLabels =
                    (List.map (\board -> loadLabels board.id) boards)

                cmds =
                    Cmd.batch (cmdsLoadList ++ cmdsLoadLabels)
            in
                ( { model | boards = boards }, cmds )

        ListsLoaded { trelloList, boardId } ->
            let
                oldBoard =
                    getBoardByIdFromList model.boards boardId

                newBoards =
                    case oldBoard of
                        Nothing ->
                            model.boards

                        Just board ->
                            let
                                updatedBoard =
                                    { board | lists = trelloList }
                            in
                                List.map (updateBoardAtId updatedBoard boardId) model.boards
            in
                ( { model | boards = newBoards }
                , Cmd.none
                )

        LabelsLoaded { trelloLabel, boardId } ->
            let
                oldBoard =
                    getBoardByIdFromList model.boards boardId

                newBoards =
                    case oldBoard of
                        Nothing ->
                            model.boards

                        Just board ->
                            let
                                updatedBoard =
                                    { board | labels = trelloLabel }
                            in
                                List.map (updateBoardAtId updatedBoard boardId) model.boards
            in
                ( { model | boards = newBoards }
                , Cmd.none
                )

        IndexMsg msg ->
            case msg of
                Index.LoadBoards ->
                    update LoadBoards model

                Index.LoadLists string ->
                    ( model, loadLists string )

                Index.LoadLabels string ->
                    ( model, loadLabels string )

                Index.Authorize ->
                    update Authorize model

                _ ->
                    let
                        ( indexModel, cmd, message ) =
                            Index.update msg model.index
                    in
                        ( { model | index = indexModel, message = message }
                        , Cmd.map IndexMsg cmd
                        )

        LoginMsg msg ->
            let
                ( loginModel, cmd, message ) =
                    Login.update msg model.login
            in
                ( { model | login = loginModel, message = message }
                , Cmd.map LoginMsg cmd
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
                IndexRoute ->
                    Html.map IndexMsg
                        (Index.view model.index model.boards model.trelloAuthorized)

                LoginRoute ->
                    Html.map LoginMsg
                        (Login.view model.login)

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
            ]


pageHeader : Model -> Html Msg
pageHeader model =
    let
        authorizeTrelloLink =
            if model.trelloAuthorized then
                a [ class "item right", onClick (Deauthorize) ] [ text "Deauthorize Trello" ]
            else
                a [ class "item right", onClick (Authorize) ] [ text "Authorize Trello" ]
    in
        div [ class "ui container" ]
            [ a [ class "item", onClick (Navigate IndexRoute) ] [ text "Index" ]
            , authorizeTrelloLink
            , a [ class "item right", onClick (Navigate LoginRoute) ] [ text "Login" ]
            ]



-- subscriptions


port trelloAuthorized : (String -> msg) -> Sub msg


port boardsLoaded : (List Board -> msg) -> Sub msg


port listsLoaded : (TrelloListPlusBoardId -> msg) -> Sub msg


port labelsLoaded : (TrelloLabelPlusBoardId -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        loginSub =
            Login.subscriptions model.login

        indexSub =
            Index.subscriptions model.index

        messageSub =
            Message.subscriptions model.message
    in
        Sub.batch
            [ Sub.map IndexMsg indexSub
            , Sub.map LoginMsg loginSub
            , Sub.map MessageMsg messageSub
            , trelloAuthorized Authorized
            , boardsLoaded BoardsLoaded
            , listsLoaded ListsLoaded
            , labelsLoaded LabelsLoaded
            ]
