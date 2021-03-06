port module TrelloBoard exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Aliases exposing (..)
import Message exposing (..)


-- model


type alias Model =
    { message : Maybe String
    , query : String
    , selectedBoard : Maybe TrelloBoard
    , selectedLabel : Maybe TrelloLabel
    , selectedCard : Maybe TrelloCard
    , showBoardDetails : Bool
    }


initModel : Model
initModel =
    { message = Nothing
    , query = ""
    , selectedBoard = Nothing
    , selectedLabel = Nothing
    , selectedCard = Nothing
    , showBoardDetails = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = SearchInput String
    | Search
    | LoadBoards
    | Authorize
    | SelectBoard TrelloBoard
    | UnselectBoard
    | SelectLabel TrelloLabel
    | UnselectLabel
    | SelectCard TrelloCard
    | UnselectCard
    | ShowBoardDetails
    | HideBoardDetails


port loadLists : String -> Cmd msg


port loadLabels : String -> Cmd msg


port loadCard : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg, Message )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none, initMessage )

        Search ->
            ( model, Cmd.none, errorMessage "Search not implemented" )

        LoadBoards ->
            ( model, Cmd.none, initMessage )

        Authorize ->
            ( model, Cmd.none, initMessage )

        SelectBoard board ->
            let
                cmds =
                    Cmd.batch [ loadLists board.id, loadLabels board.id ]
            in
                ( { model | selectedBoard = Just board }, cmds, initMessage )

        UnselectBoard ->
            ( { model | selectedBoard = Nothing, selectedLabel = Nothing, showBoardDetails = False }, Cmd.none, initMessage )

        SelectLabel label ->
            ( { model | selectedLabel = Just label }, Cmd.none, initMessage )

        UnselectLabel ->
            ( { model | selectedLabel = Nothing }, Cmd.none, initMessage )

        SelectCard card ->
            ( { model | selectedCard = Just card }, loadCard card.id, initMessage )

        UnselectCard ->
            ( { model | selectedCard = Nothing }, Cmd.none, initMessage )

        ShowBoardDetails ->
            ( { model | showBoardDetails = True }, Cmd.none, initMessage )

        HideBoardDetails ->
            ( { model | showBoardDetails = False }, Cmd.none, initMessage )



-- view


view : Model -> List TrelloBoard -> Bool -> Html Msg
view model boards authorized =
    let
        renderedHtml =
            if authorized then
                case model.selectedCard of
                    Nothing ->
                        case model.selectedBoard of
                            Nothing ->
                                div [ class "main" ]
                                    [ h1 [ class "ui header" ] [ text "Please select one board" ]
                                    , boardsList boards authorized
                                    , div [ class "ui section divider" ] [ text "" ]
                                    , searchForm model.query
                                    ]

                            Just board ->
                                showBoard model board

                    Just card ->
                        showCard model card
            else
                div [ class "main" ]
                    [ h1 [ class "ui header" ] [ text "Boards" ]
                    , a [ class "ui button", onClick (Authorize) ] [ text "Authorize and load boards" ]
                    ]
    in
        renderedHtml


boardsList : List TrelloBoard -> Bool -> Html Msg
boardsList boards authorized =
    List.map boardCard boards
        |> div [ class "ui two column grid" ]


boardCard : TrelloBoard -> Html Msg
boardCard board =
    div [ class "column" ]
        [ div [ class "ui segment" ]
            [ div [ class "header" ] [ text board.name ]
            , a [ class "ui button", onClick (SelectBoard board) ] [ text "Select board" ]
            ]
        ]


showBoard : Model -> TrelloBoard -> Html Msg
showBoard model board =
    let
        showHideBoardButton =
            if model.showBoardDetails then
                a [ class "ui button", onClick (HideBoardDetails) ] [ text "Hide board details" ]
            else
                a [ class "ui button", onClick (ShowBoardDetails) ] [ text "Show board details" ]

        divBoardDetails =
            if model.showBoardDetails then
                showBoardDetails model board
            else
                div [] []
    in
        div [ class "main" ]
            [ h1 [ class "ui header" ] [ text ("Board with name " ++ board.name ++ " is selected.") ]
            , showHideBoardButton
            , a [ class "ui button", onClick (UnselectBoard) ] [ text "Cancel board selection" ]
            , divBoardDetails
            ]


showBoardDetails : Model -> TrelloBoard -> Html Msg
showBoardDetails model board =
    let
        paragraphSelectLabel =
            case model.selectedLabel of
                Nothing ->
                    p [] [ text "Click at labels to filter by label" ]

                Just label ->
                    p []
                        [ a [ class "ui button small red", onClick UnselectLabel ] [ text "Clear selection" ]
                        ]
    in
        div []
            [ div [ class "ui section divider" ] [ text "" ]
            , h3 [] [ text "Labels" ]
            , paragraphSelectLabel
            , trelloLabelsAsCollumns model True board.labels
            , div [ class "ui section divider" ] [ text "" ]
            , h3 [] [ text "Lists and its cards with tags" ]
            , trelloListsAsCollumns model board.lists
            ]


showCard : Model -> TrelloCard -> Html Msg
showCard model card =
    let
        referalActivity =
            case card.activityId of
                Nothing ->
                    h3 [] [ text "" ]

                Just activityId ->
                    let
                        justTaskId =
                            Just activityId
                    in
                        h3 [] [ text ("Subactivity of activity of id: " ++ activityId) ]
    in
        div [ class "main" ]
            [ h1 [ class "ui header" ] [ text ("Showing card " ++ card.name) ]
            , h3 [] [ text ("Id (Trello): " ++ card.id) ]
            , referalActivity
            , h3 [] [ text "Labels" ]
            , trelloLabelsAsCollumns model False card.labels
            , h3 [] [ text "Attachments" ]
            , trelloAttachmentsAsCollumns card.attachments
            , h3 [] [ text "Description" ]
            , p [] [ text card.desc ]
            , a [ class "ui button", onClick (UnselectCard) ] [ text "Back to board" ]
            ]


trelloLabelsAsCollumns : Model -> Bool -> List TrelloLabel -> Html Msg
trelloLabelsAsCollumns model canSelect list =
    let
        renderedListOrEmptyMessage =
            if List.isEmpty list then
                p [] [ text "No labels" ]
            else
                List.map (trelloLabelAsCollumn model canSelect) list
                    |> div [ class "ui five column grid" ]
    in
        renderedListOrEmptyMessage


trelloLabelAsCollumn : Model -> Bool -> TrelloLabel -> Html Msg
trelloLabelAsCollumn model canSelect label =
    let
        labelClass =
            if canSelect && model.selectedLabel == Just label then
                "ui segment inverted " ++ label.color
            else
                "ui segment  " ++ label.color

        onClickActionOrNothing =
            if canSelect then
                [ class "column", onClick (SelectLabel label) ]
            else
                [ class "column" ]
    in
        div onClickActionOrNothing
            [ div
                [ class labelClass ]
                [ div [ class "content" ] [ text label.name ] ]
            ]


trelloAttachmentsAsCollumns : List TrelloAttachment -> Html Msg
trelloAttachmentsAsCollumns list =
    let
        renderedListOrEmptyMessage =
            if List.isEmpty list then
                p [] [ text "No attachments" ]
            else
                List.map (trelloAttachmentsAsCollumn) list
                    |> div [ class "ui five column grid" ]
    in
        renderedListOrEmptyMessage


trelloAttachmentsAsCollumn : TrelloAttachment -> Html Msg
trelloAttachmentsAsCollumn attachment =
    div []
        [ div
            [ class "ui segment" ]
            [ div [ class "content" ]
                [ a [ href attachment.url ] [ text attachment.name ] ]
            ]
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
            case card.activityId of
                Nothing ->
                    card.name ++ " "

                Just activityId ->
                    card.name ++ " [activityId= " ++ activityId ++ "] "

        aLink =
            button [ class "ui icon button compact circular", onClick (SelectCard card) ]
                [ i [ class "zoom in icon" ] [] ]
    in
        li []
            [ text namePlusId
            , aLink
            , cardLabelsToSpans card.labels
            ]


cardLabelsToSpans : List TrelloLabel -> Html Msg
cardLabelsToSpans labels =
    List.map cardLabelToSpan labels
        |> span [ class "labels" ]


cardLabelToSpan : TrelloLabel -> Html Msg
cardLabelToSpan label =
    span [ class label.color, title label.name ] [ text "[]" ]


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
