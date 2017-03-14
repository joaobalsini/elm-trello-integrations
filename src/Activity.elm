port module Activity exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Message exposing (..)
import Routes exposing (..)
import Aliases exposing (Activity, ActivityGroup, initActivity, FieldError)
import Date exposing (..)
import Date.Extra as DateExtra
import Regex
import Array


-- model


type alias Model =
    { name : String
    , nameError : Maybe String
    , actualDate : Maybe Date
    , startDate : String
    , startDateError : Maybe String
    , endDate : String
    , endDateError : Maybe String
    , activityGroupId : String
    , activityGroupIdError : Maybe String
    , showErrorPanel : Bool
    , activity : Activity
    , redirectRoute : Route
    , showingForm : Bool
    , waitingServerConfirmation : Bool
    }


initModel : Model
initModel =
    { name = ""
    , nameError = Nothing
    , startDate = ""
    , startDateError = Nothing
    , actualDate = Nothing
    , endDate = ""
    , endDateError = Nothing
    , activityGroupId = ""
    , activityGroupIdError = Nothing
    , activity = initActivity
    , showErrorPanel = False
    , redirectRoute = TrelloBoardRoute
    , showingForm = False
    , waitingServerConfirmation = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


port getDate : () -> Cmd msg


port addActivity : Activity -> Cmd msg


port removeActivity : Activity -> Cmd msg


port updateActivity : Activity -> Cmd msg


type Msg
    = GetDate
    | NameInput String
    | StartDateInput String
    | EndDateInput String
    | ActivityGroupIdInput String
    | NewActivity (Maybe ActivityGroup)
    | EditActivity Activity
    | RemoveActivity Activity
    | ActivityAdded Activity
    | ActivityUpdated Activity
    | ActivityRemoved String
    | Submit
    | Cancel


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

        NameInput name ->
            let
                actualActivity =
                    model.activity

                ( nameError, activityName ) =
                    if String.length name < 5 then
                        ( Just "Should be 5 or more characters", "" )
                    else
                        ( Nothing, name )
            in
                ( { model | name = name, nameError = nameError, activity = { actualActivity | name = activityName } }, Cmd.none, initMessage )

        StartDateInput date ->
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
                ( { model | startDate = possiblyFormattedDate, startDateError = dateError, activity = updatedActivity }, Cmd.none, initMessage )

        EndDateInput date ->
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
                ( { model | endDate = possiblyFormattedDate, endDateError = dateError, activity = updatedActivity }, Cmd.none, initMessage )

        ActivityGroupIdInput activityGroupId ->
            let
                actualActivity =
                    model.activity

                ( activityActivityGroupIdError, activityActivityGroupId ) =
                    if String.length activityGroupId < 1 then
                        ( Just "Should be filled", "" )
                    else
                        ( Nothing, activityGroupId )
            in
                ( { model | activityGroupId = activityGroupId, activityGroupIdError = activityActivityGroupIdError, activity = { actualActivity | activityGroupId = activityActivityGroupId } }, Cmd.none, initMessage )

        NewActivity maybeActivityGroup ->
            let
                initActivity =
                    initModel.activity

                activityGroupId =
                    case maybeActivityGroup of
                        Nothing ->
                            ""

                        Just activityGroup ->
                            activityGroup.id

                newActivity =
                    { initActivity | activityGroupId = activityGroupId }
            in
                ( { initModel | showingForm = True, activityGroupId = activityGroupId }, Cmd.none, initMessage )

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
                ( { model | name = activity.name, startDate = formattedStartDate, endDate = formattedEndDate, activityGroupId = activity.activityGroupId, activity = activity, showingForm = True }, Cmd.none, initMessage )

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

        Submit ->
            let
                --simulate changes in all form fields, to see if it they are valid if they didn't change
                ( model1, _, _ ) =
                    update (NameInput model.name) model

                ( model2, _, _ ) =
                    update (StartDateInput model1.startDate) model1

                ( model3, _, _ ) =
                    update (EndDateInput model2.endDate) model2

                -- how to select the first element????
                ( model4, _, _ ) =
                    update (ActivityGroupIdInput model3.activityGroupId) model3

                showError =
                    model4.nameError /= Nothing || model4.startDateError /= Nothing || model4.endDateError /= Nothing || model4.activityGroupIdError /= Nothing

                ( updatedModel, cmd ) =
                    if showError then
                        ( { model4 | showErrorPanel = True }, Cmd.none )
                    else if model.activity.id == "" then
                        ( { initModel | waitingServerConfirmation = True }, addActivity model.activity )
                    else
                        ( { initModel | waitingServerConfirmation = True }, updateActivity model.activity )
            in
                ( updatedModel, cmd, initMessage )

        Cancel ->
            ( initModel, Cmd.none, initMessage )



-- view


view : Model -> List Activity -> List ActivityGroup -> Html Msg
view model activities activityGroups =
    let
        tempView =
            if model.showingForm then
                div [ class "main" ]
                    [ h1 [ class "ui header" ] [ text "Activity Form" ]
                    , errorPanel model
                    , activityForm model activityGroups
                    ]
            else
                div [ class "main" ]
                    [ activityTable model activities activityGroups
                    , br [] []
                    , button [ class "ui button right", onClick (NewActivity (List.head activityGroups)) ] [ text "New" ]
                    ]
    in
        tempView


activityTable : Model -> List Activity -> List ActivityGroup -> Html Msg
activityTable model activities activityGroups =
    -- filter using query afterwards
    let
        table_ =
            if List.isEmpty activities then
                div [] [ text "No activity found!" ]
            else
                List.map (activityToTr activityGroups) activities
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


activityToTr : List ActivityGroup -> Activity -> Html Msg
activityToTr activityGroups activity =
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
            , td [] [ text activityGroupName ]
            , td []
                [ button [ class "ui button", onClick (EditActivity activity) ] [ text "Edit" ]
                , button [ class "ui button", onClick (RemoveActivity activity) ] [ text "Remove" ]
                ]
            ]


activityForm : Model -> List ActivityGroup -> Html Msg
activityForm model activityGroups =
    Html.form [ class "ui large form", onSubmit Submit ]
        [ div [ class "ui stacked segment" ]
            [ div
                [ classList
                    [ ( "field", True ), ( "error", model.nameError /= Nothing ) ]
                ]
                [ label [] [ text "Name" ]
                , input
                    [ type_ "text"
                    , value model.name
                    , onInput NameInput
                    ]
                    []
                ]
            , div
                [ classList
                    [ ( "field", True ), ( "error", model.startDateError /= Nothing ) ]
                ]
                [ label [] [ text "Start Date" ]
                , input
                    [ type_ "text"
                    , value model.startDate
                    , onInput StartDateInput
                    ]
                    []
                ]
            , div
                [ classList
                    [ ( "field", True ), ( "error", model.endDateError /= Nothing ) ]
                ]
                [ label [] [ text "End Date" ]
                , input
                    [ type_ "text"
                    , value model.endDate
                    , onInput EndDateInput
                    ]
                    []
                ]
            , div
                [ classList
                    [ ( "field", True ), ( "error", model.activityGroupIdError /= Nothing ) ]
                ]
                [ label [] [ text "Activity Group Id" ]
                  --                 [ select [ onInput SetDuration ]
                  --  (List.range 0 12 |> List.map intToOption)
                , select [ onInput ActivityGroupIdInput ]
                    (List.map (activityGroupToOption model.activityGroupId) activityGroups)
                ]
            , div []
                [ label [] []
                , button [ type_ "submit", class "ui large submit button" ] [ text "Submit" ]
                , a [ class "ui large button red", onClick Cancel ] [ text "Cancel" ]
                ]
            ]
        ]


activityGroupToOption : String -> ActivityGroup -> Html Msg
activityGroupToOption selectedActivityGroupId activityGroup =
    option [ value activityGroup.id, selected (selectedActivityGroupId == activityGroup.id) ] [ text activityGroup.name ]


errorPanel : Model -> Html a
errorPanel model =
    let
        list : List FieldError
        list =
            [ { fieldName = "Name", errorMessage = model.nameError }
            , { fieldName = "Start date", errorMessage = model.startDateError }
            , { fieldName = "End date", errorMessage = model.endDateError }
            , { fieldName = "Activity Group", errorMessage = model.activityGroupIdError }
            ]

        elementsWithError : List FieldError
        elementsWithError =
            List.filter (\el -> el.errorMessage /= Nothing) list
    in
        if model.showErrorPanel then
            div [ class "ui message error " ]
                [ div [ class "header" ] [ text "We had some issues:" ]
                , ul [ class "list" ] (List.map (\el -> li [] [ text (el.fieldName ++ ":" ++ (Maybe.withDefault "" el.errorMessage)) ]) elementsWithError)
                ]
        else
            div [] []


port activityAdded : (Activity -> msg) -> Sub msg


port activityUpdated : (Activity -> msg) -> Sub msg


port activityRemoved : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ activityAdded ActivityAdded
        , activityUpdated ActivityUpdated
        , activityRemoved ActivityRemoved
        ]
