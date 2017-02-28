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
    , selectedLabel : Maybe TrelloLabel
    }


initModel : Model
initModel =
    { message = Nothing
    , query = ""
    , selectedBoard = Nothing
    , selectedLabel = Nothing
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
    | LoadLabels String
    | Authorize
    | SelectBoard Board
    | UnselectBoard
    | SelectLabel TrelloLabel
    | UnselectLabel


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

        LoadLabels boardId ->
            ( model, Cmd.none, initMessage )

        Authorize ->
            ( model, Cmd.none, initMessage )

        SelectBoard board ->
            ( { model | selectedBoard = Just board }, Cmd.none, initMessage )

        UnselectBoard ->
            ( { model | selectedBoard = Nothing }, Cmd.none, initMessage )

        SelectLabel label ->
            ( { model | selectedLabel = Just label }, Cmd.none, initMessage )

        UnselectLabel ->
            ( { model | selectedLabel = Nothing }, Cmd.none, initMessage )



-- view


view : Model -> List Board -> Bool -> Html Msg
view model boards authorized =
    let
        renderedHtml =
            case model.selectedBoard of
                Nothing ->
                    div [ class "main" ]
                        [ h1 [ class "ui header" ] [ text "Boards" ]
                        , boardsList boards authorized
                        , div [ class "ui section divider" ] [ text "" ]
                        , searchForm model.query
                        ]

                Just board ->
                    showBoard model board
    in
        renderedHtml


boardsList : List Board -> Bool -> Html Msg
boardsList boards authorized =
    let
        renderedHtml : Html Msg
        renderedHtml =
            if authorized then
                List.map boardCard boards
                    |> div [ class "ui two column grid" ]
            else
                a [ class "ui button", onClick (Authorize) ] [ text "Authorize and load boards" ]
    in
        renderedHtml


boardCard : Board -> Html Msg
boardCard board =
    div [ class "column" ]
        [ div [ class "ui segment" ]
            [ div [ class "header" ] [ text board.name ]
            , trelloListList board.lists board.id
            , a [ class "ui button", onClick (SelectBoard board) ] [ text "Select board" ]
            ]
        ]


showBoard : Model -> Board -> Html Msg
showBoard model board =
    let
        paragraphSelectLabel =
            case model.selectedLabel of
                Nothing ->
                    p [] [ text "Click at labels to select" ]

                Just label ->
                    p []
                        [ a [ class "ui button small red", onClick UnselectLabel ] [ text "Clear selection" ]
                        ]
    in
        div [ class "main" ]
            [ h1 [ class "ui header" ] [ text ("Showing board " ++ board.name) ]
            , h3 [] [ text "Labels" ]
            , paragraphSelectLabel
            , trelloLabelsAsCollumns model board.labels
            , div [ class "ui section divider" ] [ text "" ]
            , h3 [] [ text "Lists" ]
            , trelloListsAsCollumns model board.lists
            , div [ class "ui section divider" ] [ text "" ]
            , a [ class "ui button", onClick (UnselectBoard) ] [ text "Show all boards" ]
            ]


trelloLabelsAsCollumns : Model -> List TrelloLabel -> Html Msg
trelloLabelsAsCollumns model list =
    List.map (trelloLabelAsCollumn model) list
        |> div [ class "ui five column grid" ]


trelloLabelAsCollumn : Model -> TrelloLabel -> Html Msg
trelloLabelAsCollumn model label =
    let
        labelClass =
            if model.selectedLabel == Just label then
                "ui segment inverted " ++ label.color
            else
                "ui segment  " ++ label.color
    in
        div [ class "column", onClick (SelectLabel label) ]
            [ div
                [ class labelClass ]
                [ div [ class "content" ] [ text label.name ] ]
            ]


trelloListsAsCollumns : Model -> List TrelloList -> Html Msg
trelloListsAsCollumns model list =
    List.map (trelloListAsCollumn model) list
        |> div [ class "ui two column grid" ]


trelloListAsCollumn : Model -> TrelloList -> Html Msg
trelloListAsCollumn model trelloList =
    div [ class "column" ]
        [ div
            [ class "ui segment" ]
            [ div [ class "header" ] [ text trelloList.name ]
            , cardsToList (List.filter (\card -> cardHasLabel card model.selectedLabel) trelloList.cards)
            ]
        ]


cardHasLabel : TrelloCard -> Maybe TrelloLabel -> Bool
cardHasLabel card maybeLabel =
    case maybeLabel of
        Nothing ->
            True

        Just maybeLabel ->
            List.member maybeLabel card.labels


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
    let
        namePlusId =
            case card.taskId of
                Nothing ->
                    card.name

                Just taskId ->
                    card.name ++ " [taskId= " ++ taskId ++ "]"
    in
        li []
            [ text namePlusId
            , cardLabelsToSpans card.labels
            ]


cardLabelsToSpans : List TrelloLabel -> Html Msg
cardLabelsToSpans labels =
    List.map cardLabelToSpan labels
        |> span [ class "labels" ]


cardLabelToSpan : TrelloLabel -> Html Msg
cardLabelToSpan label =
    span [ class label.color, title label.name ] [ text "[  ]" ]


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
