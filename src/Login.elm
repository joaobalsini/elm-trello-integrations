module Login exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Message exposing (..)
import Routes exposing (..)
import Navigation


-- model


type alias Model =
    { username : String
    , password : String
    , error : Maybe String
    , redirectRoute : Route
    }


initModel : Model
initModel =
    { username = ""
    , password = ""
    , error = Nothing
    , redirectRoute = IndexRoute
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = UsernameInput String
    | PasswordInput String
    | Submit
    | Cancel
    | Error String


update : Msg -> Model -> ( Model, Cmd Msg, Message )
update msg model =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Cmd.none, Message.initMessage )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none, Message.initMessage )

        Submit ->
            ( model, Navigation.newUrl (routeToHash model.redirectRoute), Message.warningMessage "Login submit not defined" )

        Cancel ->
            ( model, Navigation.newUrl (routeToHash model.redirectRoute), Message.infoMessage "Login canceled" )

        Error error ->
            ( { model | error = Just error }, Cmd.none, Message.errorMessage "Login error" )



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ h1 [ class "ui header" ] [ text "Login" ]
        , errorPanel model.error
        , loginForm model
        ]


loginForm : Model -> Html Msg
loginForm model =
    Html.form [ class "ui large form", onSubmit Submit ]
        [ div [ class "ui stacked segment" ]
            [ div [ class "field" ]
                [ label [] [ text "User Name" ]
                , input
                    [ type_ "text"
                    , value model.username
                    , onInput UsernameInput
                    ]
                    []
                ]
            , div [ class "field" ]
                [ label [] [ text "Password" ]
                , input
                    [ type_ "password"
                    , value model.password
                    , onInput PasswordInput
                    ]
                    []
                ]
            , div []
                [ label [] []
                , button [ type_ "submit", class "ui large submit button" ] [ text "Login" ]
                , a [ class "ui large button red", onClick Cancel ] [ text "Cancel" ]
                ]
            ]
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
