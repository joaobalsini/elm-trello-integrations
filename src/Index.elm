port module Index exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


-- model


type alias Model =
    { message : Maybe String
    , query : String
    }


initModel : Model
initModel =
    { message = Nothing
    , query = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = SearchInput String
    | Search


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none )

        Search ->
            ( model, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ h1 [ class "ui header" ] [ text "Initial Page - Index" ]
        , messagePanel model.message
        , searchForm model.query
        ]


messagePanel : Maybe String -> Html a
messagePanel message =
    case message of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg
                , button [ type_ "button" ] [ text "×" ]
                ]


searchForm : String -> Html Msg
searchForm query =
    Html.form [ onSubmit Search ]
        [ input
            [ type_ "text"
            , placeholder "Search..."
            , value query
            , onInput SearchInput
            ]
            []
        , button [ type_ "submit" ] [ text "Search" ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
