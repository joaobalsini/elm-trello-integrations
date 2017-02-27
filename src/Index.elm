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
    , selectedBoard : Maybe Board
    }


initModel : Model
initModel =
    { message = Nothing
    , query = ""
    , selectedBoard = Nothing
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
    | SelectBoard Board
    | UnselectBoard


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

        SelectBoard board ->
            ( { model | selectedBoard = Just board }, Cmd.none, initMessage )

        UnselectBoard ->
            ( { model | selectedBoard = Nothing }, Cmd.none, initMessage )



-- view


view : Model -> List Board -> Html Msg
view model boards =
    let
        renderedHtml =
            case model.selectedBoard of
                Nothing ->
                    div [ class "main" ]
                        [ h1 [ class "ui header" ] [ text "Boards" ]
                        , boardsList boards
                        , div [ class "ui section divider" ] [ text "" ]
                        , searchForm model.query
                        ]

                Just board ->
                    showBoard board
    in
        renderedHtml


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


boardCard : Board -> Html Msg
boardCard board =
    div [ class "column" ]
        [ div [ class "ui segment" ]
            [ div [ class "header" ] [ text board.name ]
            , div [ class "description" ] [ text board.desc ]
            , trelloListList board.lists board.id
            , a [ class "ui button", onClick (SelectBoard board) ] [ text "Select board" ]
            ]
        ]


showBoard : Board -> Html Msg
showBoard board =
    div [ class "main" ]
        [ h1 [ class "ui header" ] [ text ("Showing board " ++ board.name) ]
        , h4 [] [ text "Labels" ]
        , trelloLabelsAsCollumns board.labels
        , div [ class "ui section divider" ] [ text "" ]
        , h4 [] [ text "Lists" ]
        , trelloListsAsCollumns board.lists
        , div [ class "ui section divider" ] [ text "" ]
        , a [ class "ui button", onClick (UnselectBoard) ] [ text "Show all boards" ]
        ]


trelloLabelsAsCollumns : List TrelloLabel -> Html Msg
trelloLabelsAsCollumns list =
    List.map trelloLabelAsCollumn list
        |> div [ class "ui five column grid" ]


trelloLabelAsCollumn : TrelloLabel -> Html Msg
trelloLabelAsCollumn label =
    div [ class "column" ]
        [ div
            [ class "ui segment" ]
            [ div [ class "header" ] [ text label.color ]
            , div [ class "content" ] [ text label.name ]
            ]
        ]


trelloListsAsCollumns : List TrelloList -> Html Msg
trelloListsAsCollumns list =
    List.map trelloListAsCollumn list
        |> div [ class "ui two column grid" ]


trelloListAsCollumn : TrelloList -> Html Msg
trelloListAsCollumn trelloList =
    div [ class "column" ]
        [ div
            [ class "ui segment" ]
            [ div [ class "header" ] [ text trelloList.name ]
            , cardsToList trelloList.cards
            ]
        ]


trelloListList : List TrelloList -> String -> Html Msg
trelloListList lists boardId =
    let
        renderedHtml : Html Msg
        renderedHtml =
            if List.length lists == 0 then
                a [ class "ui button", onClick (LoadLists boardId) ] [ text "Load Lists" ]
            else
                List.map trelloListToLi lists
                    |> ul []
    in
        renderedHtml


trelloListToLi : TrelloList -> Html Msg
trelloListToLi list =
    li []
        [ text list.name
          -- , cardsToList list.cards
        ]


cardsToList : List TrelloCard -> Html Msg
cardsToList cards =
    List.map cardToLi cards
        |> ul []


cardToLi : TrelloCard -> Html Msg
cardToLi card =
    li [] [ text card.name ]


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
