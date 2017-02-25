port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import Routes exposing (..)
import Message exposing (..)
import Index
import Login


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



-- update


type Msg
    = Navigate Route
    | ChangePage Route
    | IndexMsg Index.Msg
    | LoginMsg Login.Msg
    | MessageMsg Message.Msg


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

        IndexMsg msg ->
            let
                ( indexModel, cmd ) =
                    Index.update msg model.index
            in
                ( { model | index = indexModel }
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
                        (Index.view model.index)

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
    div [ class "ui container" ]
        [ a [ class "item", onClick (Navigate IndexRoute) ] [ text "Index" ]
        , a [ class "item right", onClick (Navigate LoginRoute) ] [ text "Login" ]
        ]



-- subscriptions


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
            ]
