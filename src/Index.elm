port module Index exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Aliases exposing (..)
import Message exposing (..)


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
    | LoadBoards
    | LoadLists String


update : Msg -> Model -> ( Model, Cmd Msg, Message )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none, initMessage )

        Search ->
            ( model, Cmd.none, errorMessage "Search not implemented" )

        LoadBoards ->
            ( model, Cmd.none, initMessage )

        LoadLists boardId ->
            ( model, Cmd.none, initMessage )



-- view


view : Model -> List Board -> Html Msg
view model boards =
    div [ class "main" ]
        [ h1 [ class "ui header" ] [ text "Boards" ]
        , boardsList boards
        , div [ class "ui section divider" ] [ text "" ]
        , searchForm model.query
        ]


boardsList : List Board -> Html Msg
boardsList boards =
    let
        renderedHtml : Html Msg
        renderedHtml =
            if List.length boards == 0 then
                a [ class "ui button", onClick (LoadBoards) ] [ text "Load Boards" ]
            else
                List.map boardCard boards
                    |> div [ class "ui two column grid" ]
    in
        renderedHtml


listsList : List TrelloList -> String -> Html Msg
listsList lists boardId =
    let
        renderedHtml : Html Msg
        renderedHtml =
            if List.length lists == 0 then
                a [ class "ui button", onClick (LoadLists boardId) ] [ text "Load Lists" ]
            else
                List.map listToLi lists
                    |> ul []
    in
        renderedHtml


listToLi : TrelloList -> Html Msg
listToLi list =
    li []
        [ text list.name
        , cardsToList list.cards
        ]


cardsToList : List TrelloCard -> Html Msg
cardsToList cards =
    List.map cardToLi cards
        |> ul []


cardToLi : TrelloCard -> Html Msg
cardToLi card =
    li [] [ text card.name ]


boardCard : Board -> Html Msg
boardCard board =
    div [ class "column" ]
        [ div [ class "ui segment" ]
            [ div [ class "header" ] [ text board.name ]
            , div [ class "description" ] [ text board.desc ]
            , listsList board.lists board.id
            ]
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
        [ div [ class "ui icon input right" ]
            [ input
                [ type_ "text"
                , placeholder "Search..."
                , value query
                , onInput SearchInput
                ]
                []
            , i [ class "inverted circular search link icon", onClick Search ] [ text "" ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
