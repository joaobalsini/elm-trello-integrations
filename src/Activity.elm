port module Activity exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Message exposing (..)
import Routes exposing (..)
import Aliases exposing (Activity, ActivityGroup, initActivity, FieldError, TrelloCard, initTrelloCard, TrelloBoard, TrelloList)
import Date exposing (..)
import Date.Extra as DateExtra
import Regex
import Array


-- model


type alias Model =
    { activityFormName : String
    , activityFormNameError : Maybe String
    , actualDate : Maybe Date
    , activityFormStartDate : String
    , activityFormStartDateError : Maybe String
    , activityFormEndDate : String
    , activityFormEndDateError : Maybe String
    , activityFormActivityGroupId : String
    , activityFormActivityGroupIdError : Maybe String
    , activityFormShowErrorPanel : Bool
    , activity : Activity
    , redirectRoute : Route
    , activityFormShowingForm : Bool
    , waitingServerConfirmation : Bool
    , trelloCardFormName : String
    , trelloCardFormNameError : Maybe String
    , trelloCardFormListId : String
    , trelloCardFormListIdError : Maybe String
    , trelloCardFormActivityId : String
    , trelloCardFormActivityIdError : Maybe String
    , trelloCardFormShowingForm : Bool
    , trelloCardFormShowErrorPanel : Bool
    , trelloCard : TrelloCard
    }


initModel : Model
initModel =
    { activityFormName = ""
    , activityFormNameError = Nothing
    , activityFormStartDate = ""
    , activityFormStartDateError = Nothing
    , actualDate = Nothing
    , activityFormEndDate = ""
    , activityFormEndDateError = Nothing
    , activityFormActivityGroupId = ""
    , activityFormActivityGroupIdError = Nothing
    , activity = initActivity
    , activityFormShowErrorPanel = False
    , redirectRoute = TrelloBoardRoute
    , activityFormShowingForm = False
    , waitingServerConfirmation = False
    , trelloCardFormName = ""
    , trelloCardFormNameError = Nothing
    , trelloCardFormListId = ""
    , trelloCardFormListIdError = Nothing
    , trelloCardFormActivityId = ""
    , trelloCardFormActivityIdError = Nothing
    , trelloCardFormShowingForm = False
    , trelloCardFormShowErrorPanel = False
    , trelloCard = initTrelloCard
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


port getDate : () -> Cmd msg


port addActivity : Activity -> Cmd msg


port removeActivity : Activity -> Cmd msg


port updateActivity : Activity -> Cmd msg


port addTrelloCard : TrelloCard -> Cmd msg


type Msg
    = GetDate
    | ActivityFormNameInput String
    | ActivityFormStartDateInput String
    | ActivityFormEndDateInput String
    | ActivityFormActivityGroupIdInput String
    | NewActivity (Maybe ActivityGroup)
    | EditActivity Activity
    | RemoveActivity Activity
    | ActivityAdded Activity
    | ActivityUpdated Activity
    | ActivityRemoved String
    | SubmitActivityForm
    | CancelActivityForm
    | NewTrelloCard (Maybe Activity) (Maybe TrelloList)
    | TrelloCardFormSubmit
    | TrelloCardFormNameInput String
    | TrelloCardFormListIdInput String
    | TrelloCardFormActivityIdInput String
    | TrelloCardFormCancel
    | TrelloCardAdded TrelloCard


parseDate : String -> Maybe Date
parseDate string =
    let
        exp =
            Regex.regex "(((0[1-9]|[12]\\d|3[01])\\/(0[13578]|1[02])\\/((19|[2-9]\\d)\\d{2}))|((0[1-9]|[12]\\d|30)\\/(0[13456789]|1[012])\\/((19|[2-9]\\d)\\d{2}))|((0[1-9]|1\\d|2[0-8])\\/02\\/((19|[2-9]\\d)\\d{2}))|(29\\/02\\/((1[6-9]|[2-9]\\d)(0[48]|[2468][048]|[13579][26])|((16|[2468][048]|[3579][26])00))))"

        matches =
            Regex.find (Regex.AtMost 1) exp string

        list_matches =
            List.map .match matches

        parsedDate =
            case list_matches of
                [] ->
                    Nothing

                x :: xs ->
                    -- get first match
                    let
                        array_day_month_year =
                            Array.fromList (String.split "/" x)
                    in
                        case Maybe.map3 (fromDayMonthYearToDate) (Array.get 0 array_day_month_year) (Array.get 1 array_day_month_year) (Array.get 2 array_day_month_year) of
                            Just x ->
                                x

                            Nothing ->
                                Nothing
    in
        parsedDate


fromDayMonthYearToDate : String -> String -> String -> Maybe Date
fromDayMonthYearToDate day month year =
    Result.toMaybe (Date.fromString (year ++ "-" ++ month ++ "-" ++ day ++ "T12:00:00"))


update : Msg -> Model -> ( Model, Cmd Msg, Message )
update msg model =
    case msg of
        GetDate ->
            ( model, getDate (), Message.initMessage )

        ActivityFormNameInput name ->
            let
                actualActivity =
                    model.activity

                ( nameError, activityName ) =
                    if String.length name < 5 then
                        ( Just "Should be 5 or more characters", "" )
                    else
                        ( Nothing, name )
            in
                ( { model | activityFormName = name, activityFormNameError = nameError, activity = { actualActivity | name = activityName } }, Cmd.none, initMessage )

        ActivityFormStartDateInput date ->
            let
                actualActivity =
                    model.activity

                parsedDate =
                    parseDate date

                ( dateError, activityDateAsIsoString, possiblyFormattedDate ) =
                    case parsedDate of
                        Nothing ->
                            ( Just "Should be in format dd/mm/yyyy", Nothing, date )

                        Just d ->
                            ( Nothing, Just (DateExtra.toIsoString d), date )

                updatedActivity =
                    { actualActivity | startDate = activityDateAsIsoString }
            in
                ( { model | activityFormStartDate = possiblyFormattedDate, activityFormStartDateError = dateError, activity = updatedActivity }, Cmd.none, initMessage )

        ActivityFormEndDateInput date ->
            let
                actualActivity =
                    model.activity

                parsedDate =
                    parseDate date

                ( dateError, activityDateAsIsoString, possiblyFormattedDate ) =
                    case parsedDate of
                        Nothing ->
                            ( Just "Should be in format dd/mm/yyyy", Nothing, date )

                        Just d ->
                            ( Nothing, Just (DateExtra.toIsoString d), date )

                updatedActivity =
                    { actualActivity | endDate = activityDateAsIsoString }
            in
                ( { model | activityFormEndDate = possiblyFormattedDate, activityFormEndDateError = dateError, activity = updatedActivity }, Cmd.none, initMessage )

        ActivityFormActivityGroupIdInput activityGroupId ->
            let
                actualActivity =
                    model.activity

                ( activityActivityGroupIdError, activityActivityGroupId ) =
                    if String.length activityGroupId < 1 then
                        ( Just "Should be filled", "" )
                    else
                        ( Nothing, activityGroupId )
            in
                ( { model | activityFormActivityGroupId = activityGroupId, activityFormActivityGroupIdError = activityActivityGroupIdError, activity = { actualActivity | activityGroupId = activityActivityGroupId } }, Cmd.none, initMessage )

        NewActivity maybeActivityGroup ->
            let
                activityGroupId =
                    case maybeActivityGroup of
                        Nothing ->
                            ""

                        Just activityGroup ->
                            activityGroup.id
            in
                ( { initModel | activityFormShowingForm = True, activityFormActivityGroupId = activityGroupId }, Cmd.none, initMessage )

        EditActivity activity ->
            let
                maybeStartDate =
                    case activity.startDate of
                        Nothing ->
                            Nothing

                        Just date ->
                            DateExtra.fromIsoString date

                maybeEndDate =
                    case activity.endDate of
                        Nothing ->
                            Nothing

                        Just date ->
                            DateExtra.fromIsoString date

                ( startDateError, formattedStartDate ) =
                    case maybeStartDate of
                        Nothing ->
                            ( Just "Should be in format dd/mm/yyyy", "" )

                        Just d ->
                            ( Nothing, DateExtra.toFormattedString "dd/MM/y" d )

                ( endDateError, formattedEndDate ) =
                    case maybeEndDate of
                        Nothing ->
                            ( Just "Should be in format dd/mm/yyyy", "" )

                        Just d ->
                            ( Nothing, DateExtra.toFormattedString "dd/MM/y" d )
            in
                ( { model | activityFormName = activity.name, activityFormStartDate = formattedStartDate, activityFormEndDate = formattedEndDate, activityFormActivityGroupId = activity.activityGroupId, activity = activity, activityFormShowingForm = True }, Cmd.none, initMessage )

        RemoveActivity activity ->
            ( { initModel | waitingServerConfirmation = True }, removeActivity activity, initMessage )

        ActivityAdded activity ->
            let
                message =
                    if model.waitingServerConfirmation then
                        Message.successMessage "Activity successfully added"
                    else
                        initMessage
            in
                ( model, Cmd.none, message )

        ActivityUpdated activity ->
            let
                message =
                    if model.waitingServerConfirmation then
                        Message.successMessage "Activity successfully updated"
                    else
                        initMessage
            in
                ( model, Cmd.none, message )

        ActivityRemoved id ->
            let
                message =
                    if model.waitingServerConfirmation then
                        Message.successMessage "Activity successfully removed"
                    else
                        initMessage
            in
                ( model, Cmd.none, message )

        SubmitActivityForm ->
            let
                --simulate changes in all form fields, to see if it they are valid if they didn't change
                ( model1, _, _ ) =
                    update (ActivityFormNameInput model.activityFormName) model

                ( model2, _, _ ) =
                    update (ActivityFormStartDateInput model1.activityFormStartDate) model1

                ( model3, _, _ ) =
                    update (ActivityFormEndDateInput model2.activityFormEndDate) model2

                -- how to select the first element????
                ( model4, _, _ ) =
                    update (ActivityFormActivityGroupIdInput model3.activityFormActivityGroupId) model3

                showError =
                    model4.activityFormNameError /= Nothing || model4.activityFormStartDateError /= Nothing || model4.activityFormEndDateError /= Nothing || model4.activityFormActivityGroupIdError /= Nothing

                ( updatedModel, cmd ) =
                    if showError then
                        ( { model4 | activityFormShowErrorPanel = True }, Cmd.none )
                    else if model.activity.id == "" then
                        ( { initModel | waitingServerConfirmation = True }, addActivity model.activity )
                    else
                        ( { initModel | waitingServerConfirmation = True }, updateActivity model.activity )
            in
                ( updatedModel, cmd, initMessage )

        CancelActivityForm ->
            ( initModel, Cmd.none, initMessage )

        NewTrelloCard maybeActivity maybeList ->
            let
                trelloCardActivityId =
                    case maybeActivity of
                        Nothing ->
                            ""

                        Just activity ->
                            activity.id

                trelloCardListId =
                    case maybeList of
                        Nothing ->
                            ""

                        Just list ->
                            list.id
            in
                ( { initModel | trelloCardFormShowingForm = True, trelloCardFormActivityId = trelloCardActivityId, trelloCardFormListId = trelloCardListId }, Cmd.none, initMessage )

        TrelloCardFormSubmit ->
            let
                --simulate changes in all form fields, to see if it they are valid if they didn't change
                ( model1, _, _ ) =
                    update (TrelloCardFormNameInput model.trelloCardFormName) model

                ( model2, _, _ ) =
                    update (TrelloCardFormListIdInput model1.trelloCardFormListId) model1

                ( model3, _, _ ) =
                    update (TrelloCardFormActivityIdInput model2.trelloCardFormActivityId) model2

                showError =
                    model3.trelloCardFormNameError /= Nothing || model3.trelloCardFormListIdError /= Nothing || model3.trelloCardFormActivityIdError /= Nothing

                ( updatedModel, cmd ) =
                    if showError then
                        ( { model3 | trelloCardFormShowErrorPanel = True }, Cmd.none )
                    else
                        ( { initModel | waitingServerConfirmation = True }, addTrelloCard model.trelloCard )
            in
                ( updatedModel, cmd, initMessage )

        TrelloCardFormNameInput name ->
            let
                actualTrelloCard =
                    model.trelloCard

                ( nameError, trelloCardName ) =
                    if String.length name < 5 then
                        ( Just "Should be 5 or more characters", "" )
                    else
                        ( Nothing, name )
            in
                ( { model | trelloCardFormName = name, trelloCardFormNameError = nameError, trelloCard = { actualTrelloCard | name = trelloCardName } }, Cmd.none, initMessage )

        TrelloCardFormListIdInput listId ->
            let
                trelloCardListIdError =
                    if String.length listId < 1 then
                        Just "Should be filled"
                    else
                        Nothing
            in
                ( { model | trelloCardFormListId = listId, trelloCardFormListIdError = trelloCardListIdError }, Cmd.none, initMessage )

        TrelloCardFormActivityIdInput activityId ->
            let
                actualTrelloCard =
                    model.trelloCard

                ( trelloCardActivityIdError, trelloCardActivityId ) =
                    if String.length activityId < 1 then
                        ( Just "Should be filled", Nothing )
                    else
                        ( Nothing, Just activityId )
            in
                ( { model | trelloCardFormActivityId = activityId, trelloCardFormActivityIdError = trelloCardActivityIdError, trelloCard = { actualTrelloCard | activityId = trelloCardActivityId } }, Cmd.none, initMessage )

        TrelloCardFormCancel ->
            ( initModel, Cmd.none, initMessage )

        TrelloCardAdded trelloCard ->
            let
                message =
                    if model.waitingServerConfirmation then
                        Message.successMessage "TrelloCard successfully added"
                    else
                        initMessage
            in
                ( model, Cmd.none, message )



-- view


view : Model -> List Activity -> List ActivityGroup -> Maybe TrelloBoard -> Html Msg
view model activities activityGroups selectedBoard =
    let
        tempView =
            if model.activityFormShowingForm then
                div [ class "main" ]
                    [ h1 [ class "ui header" ] [ text "Activity Form" ]
                    , activityFormErrorPanel model
                    , activityForm model activityGroups
                    ]
            else if model.trelloCardFormShowingForm then
                div [ class "main" ]
                    [ h1 [ class "ui header" ] [ text "Trello Card Form" ]
                    , trelloCardFormErrorPanel model
                    , trelloCardForm model selectedBoard activities
                    ]
            else
                div [ class "main" ]
                    [ activityTable selectedBoard activities activityGroups
                    , br [] []
                    , button [ class "ui button right", onClick (NewActivity (List.head activityGroups)) ] [ text "New" ]
                    ]
    in
        tempView


activityTable : Maybe TrelloBoard -> List Activity -> List ActivityGroup -> Html Msg
activityTable selectedBoard activities activityGroups =
    -- filter using query afterwards
    let
        table_ =
            if List.isEmpty activities then
                div [] [ text "No activity found!" ]
            else
                List.map (activityToTr selectedBoard activityGroups) activities
                    |> tbody []
                    |> (\r -> activitiesTh :: [ r ])
                    |> table [ class "ui celled table" ]
    in
        div []
            [ h1 [ class "ui header" ] [ text "Activities list" ]
            , table_
            ]


activitiesTh : Html Msg
activitiesTh =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] [ text "Start date" ]
            , th [] [ text "End Date" ]
            , th [] [ text "Cards" ]
            , th [] [ text "Activity Group" ]
            , th [] [ text "Actions" ]
            ]
        ]


getActivityGroupByIdFromList : List ActivityGroup -> String -> Maybe ActivityGroup
getActivityGroupByIdFromList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getActivityGroupByIdFromList xs id


activityToTr : Maybe TrelloBoard -> List ActivityGroup -> Activity -> Html Msg
activityToTr selectedBoard activityGroups activity =
    let
        formattedStartDate =
            case activity.startDate of
                Nothing ->
                    ""

                Just dateString ->
                    case DateExtra.fromIsoString dateString of
                        Nothing ->
                            ""

                        Just date ->
                            DateExtra.toFormattedString "dd/MM/y" date

        formattedEndDate =
            case activity.endDate of
                Nothing ->
                    ""

                Just dateString ->
                    case DateExtra.fromIsoString dateString of
                        Nothing ->
                            ""

                        Just date ->
                            DateExtra.toFormattedString "dd/MM/y" date

        activityGroupName =
            case (getActivityGroupByIdFromList activityGroups activity.activityGroupId) of
                Nothing ->
                    "ActivityGroup with id " ++ activity.activityGroupId ++ " not found!!!"

                Just activityGroup ->
                    activityGroup.name
    in
        tr []
            [ td [] [ text activity.id ]
            , td [] [ text activity.name ]
            , td [] [ text formattedStartDate ]
            , td [] [ text formattedEndDate ]
            , td [] [ (trelloCardsToUl selectedBoard activity.trelloCards activity) ]
            , td [] [ text activityGroupName ]
            , td []
                [ button [ class "ui button", onClick (EditActivity activity) ] [ text "Edit" ]
                , button [ class "ui button", onClick (RemoveActivity activity) ] [ text "Remove" ]
                ]
            ]


trelloCardsToUl : Maybe TrelloBoard -> List TrelloCard -> Activity -> Html Msg
trelloCardsToUl selectedBoard trelloCards activity =
    let
        lists =
            case selectedBoard of
                Nothing ->
                    []

                Just board ->
                    board.lists
    in
        if selectedBoard == Nothing then
            div []
                [ text "Please select a trello board first!"
                ]
        else if List.isEmpty trelloCards then
            div []
                [ text "No cards found"
                , br [] []
                , button [ class "ui button", onClick (NewTrelloCard (Just activity) (List.head lists)) ] [ text "Add Card" ]
                ]
        else
            div []
                [ ul [] (List.map trelloCardtoLi trelloCards)
                , button [ class "ui button", onClick (NewTrelloCard (Just activity) (List.head lists)) ] [ text "Add Card" ]
                ]


trelloCardtoLi : TrelloCard -> Html Msg
trelloCardtoLi trelloCard =
    li [] [ text trelloCard.name ]


activityForm : Model -> List ActivityGroup -> Html Msg
activityForm model activityGroups =
    Html.form [ class "ui large form", onSubmit SubmitActivityForm ]
        [ div [ class "ui stacked segment" ]
            [ div
                [ classList
                    [ ( "field", True ), ( "error", model.activityFormNameError /= Nothing ) ]
                ]
                [ label [] [ text "Name" ]
                , input
                    [ type_ "text"
                    , value model.activityFormName
                    , onInput ActivityFormNameInput
                    ]
                    []
                ]
            , div
                [ classList
                    [ ( "field", True ), ( "error", model.activityFormStartDateError /= Nothing ) ]
                ]
                [ label [] [ text "Start Date" ]
                , input
                    [ type_ "text"
                    , value model.activityFormStartDate
                    , onInput ActivityFormStartDateInput
                    ]
                    []
                ]
            , div
                [ classList
                    [ ( "field", True ), ( "error", model.activityFormEndDateError /= Nothing ) ]
                ]
                [ label [] [ text "End Date" ]
                , input
                    [ type_ "text"
                    , value model.activityFormEndDate
                    , onInput ActivityFormEndDateInput
                    ]
                    []
                ]
            , div
                [ classList
                    [ ( "field", True ), ( "error", model.activityFormActivityGroupIdError /= Nothing ) ]
                ]
                [ label [] [ text "Activity Group Id" ]
                  --                 [ select [ onInput SetDuration ]
                  --  (List.range 0 12 |> List.map intToOption)
                , select [ onInput ActivityFormActivityGroupIdInput, value model.activityFormActivityGroupId ]
                    (List.map (activityGroupToOption model.activityFormActivityGroupId) activityGroups)
                ]
            , div []
                [ label [] []
                , button [ type_ "submit", class "ui large submit button" ] [ text "Submit" ]
                , a [ class "ui large button red", onClick CancelActivityForm ] [ text "Cancel" ]
                ]
            ]
        ]


trelloCardForm : Model -> Maybe TrelloBoard -> List Activity -> Html Msg
trelloCardForm model selectedBoard activities =
    case selectedBoard of
        Nothing ->
            div [] [ text "Authorize trello and select one board first" ]

        Just _ ->
            let
                trelloLists =
                    case selectedBoard of
                        Nothing ->
                            []

                        Just board ->
                            board.lists
            in
                Html.form [ class "ui large form", onSubmit TrelloCardFormSubmit ]
                    [ div [ class "ui stacked segment" ]
                        [ div
                            [ classList
                                [ ( "field", True ), ( "error", model.trelloCardFormNameError /= Nothing ) ]
                            ]
                            [ label [] [ text "Name" ]
                            , input
                                [ type_ "text"
                                , value model.trelloCardFormName
                                , onInput TrelloCardFormNameInput
                                ]
                                []
                            ]
                        , div
                            [ classList
                                [ ( "field", True ), ( "error", model.trelloCardFormListIdError /= Nothing ) ]
                            ]
                            [ label [] [ text "List" ]
                              --                 [ select [ onInput SetDuration ]
                              --  (List.range 0 12 |> List.map intToOption)
                            , select [ onInput TrelloCardFormListIdInput, value model.trelloCardFormListId ]
                                (List.map (trelloListToOption) trelloLists)
                            ]
                        , div
                            [ classList
                                [ ( "field", True ), ( "error", model.trelloCardFormActivityIdError /= Nothing ) ]
                            ]
                            [ label [] [ text "List" ]
                              --                 [ select [ onInput SetDuration ]
                              --  (List.range 0 12 |> List.map intToOption)
                            , select [ onInput TrelloCardFormActivityIdInput, value model.trelloCardFormActivityId ]
                                (List.map (activityToOption model.trelloCardFormActivityId) activities)
                            ]
                        , div []
                            [ label [] []
                            , button [ type_ "submit", class "ui large submit button" ] [ text "Submit" ]
                            , a [ class "ui large button red", onClick TrelloCardFormCancel ] [ text "Cancel" ]
                            ]
                        ]
                    ]


trelloListToOption : TrelloList -> Html Msg
trelloListToOption list =
    option [ value list.id ] [ text list.name ]


activityGroupToOption : String -> ActivityGroup -> Html Msg
activityGroupToOption selectedActivityGroupId activityGroup =
    option [ value activityGroup.id, selected (selectedActivityGroupId == activityGroup.id) ] [ text activityGroup.name ]


activityToOption : String -> Activity -> Html Msg
activityToOption selectedActivityId activity =
    option [ value activity.id, selected (selectedActivityId == activity.id) ] [ text activity.name ]


activityFormErrorPanel : Model -> Html a
activityFormErrorPanel model =
    let
        list : List FieldError
        list =
            [ { fieldName = "Name", errorMessage = model.activityFormNameError }
            , { fieldName = "Start date", errorMessage = model.activityFormStartDateError }
            , { fieldName = "End date", errorMessage = model.activityFormEndDateError }
            , { fieldName = "Activity Group", errorMessage = model.activityFormActivityGroupIdError }
            ]

        elementsWithError : List FieldError
        elementsWithError =
            List.filter (\el -> el.errorMessage /= Nothing) list
    in
        if model.activityFormShowErrorPanel then
            div [ id "formErrors", class "ui message error " ]
                [ div [ class "header" ] [ text "We had some issues:" ]
                , ul [ class "list" ] (List.map (\el -> li [] [ text (el.fieldName ++ ":" ++ (Maybe.withDefault "" el.errorMessage)) ]) elementsWithError)
                ]
        else
            div [] []


trelloCardFormErrorPanel : Model -> Html a
trelloCardFormErrorPanel model =
    let
        list : List FieldError
        list =
            [ { fieldName = "Name", errorMessage = model.trelloCardFormNameError }
            , { fieldName = "List", errorMessage = model.trelloCardFormListIdError }
            , { fieldName = "Activity", errorMessage = model.trelloCardFormActivityIdError }
            ]

        elementsWithError : List FieldError
        elementsWithError =
            List.filter (\el -> el.errorMessage /= Nothing) list
    in
        if model.trelloCardFormShowErrorPanel then
            div [ id "formErrors", class "ui message error " ]
                [ div [ class "header" ] [ text "We had some issues:" ]
                , ul [ class "list" ] (List.map (\el -> li [] [ text (el.fieldName ++ ":" ++ (Maybe.withDefault "" el.errorMessage)) ]) elementsWithError)
                ]
        else
            div [] []


port activityAdded : (Activity -> msg) -> Sub msg


port activityUpdated : (Activity -> msg) -> Sub msg


port activityRemoved : (String -> msg) -> Sub msg


port trelloCardAdded : (TrelloCard -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ activityAdded ActivityAdded
        , activityUpdated ActivityUpdated
        , activityRemoved ActivityRemoved
        , trelloCardAdded TrelloCardAdded
        ]
