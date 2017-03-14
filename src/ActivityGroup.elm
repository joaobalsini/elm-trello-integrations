port module ActivityGroup exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Message exposing (..)
import Routes exposing (..)
import Aliases exposing (Activity, ActivityGroup, initActivity, initActivityGroup, FieldError)


-- model


type alias Model =
    { name : String
    , nameError : Maybe String
    , activityGroup : ActivityGroup
    , showErrorPanel : Bool
    , redirectRoute : Route
    , showingForm : Bool
    , waitingServerConfirmation : Bool
    }


initModel : Model
initModel =
    { name = ""
    , nameError = Nothing
    , activityGroup = initActivityGroup
    , showErrorPanel = False
    , redirectRoute = TrelloBoardRoute
    , showingForm = False
    , waitingServerConfirmation = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


port addActivityGroup : ActivityGroup -> Cmd msg


port removeActivityGroup : ActivityGroup -> Cmd msg


port updateActivityGroup : ActivityGroup -> Cmd msg


type Msg
    = NameInput String
    | NewActivityGroup
    | EditActivityGroup ActivityGroup
    | RemoveActivityGroup ActivityGroup
    | ActivityGroupAdded ActivityGroup
    | ActivityGroupUpdated ActivityGroup
    | ActivityGroupRemoved String
    | Submit
    | Cancel


update : Msg -> Model -> ( Model, Cmd Msg, Message )
update msg model =
    case msg of
        NameInput name ->
            let
                actualActivityGroup =
                    model.activityGroup

                ( nameError, activityGroupName ) =
                    if String.length name < 5 then
                        ( Just "Should be 5 or more characters", "" )
                    else
                        ( Nothing, name )
            in
                ( { model | name = name, nameError = nameError, activityGroup = { actualActivityGroup | name = activityGroupName } }, Cmd.none, initMessage )

        NewActivityGroup ->
            ( { initModel | showingForm = True }, Cmd.none, initMessage )

        EditActivityGroup activityGroup ->
            ( { model | name = activityGroup.name, activityGroup = activityGroup, showingForm = True }, Cmd.none, initMessage )

        RemoveActivityGroup activityGroup ->
            ( { initModel | waitingServerConfirmation = True }, removeActivityGroup activityGroup, initMessage )

        ActivityGroupAdded activityGroup ->
            let
                message =
                    if model.waitingServerConfirmation then
                        Message.successMessage "Activity Group successfully added"
                    else
                        initMessage
            in
                ( model, Cmd.none, message )

        ActivityGroupUpdated activityGroup ->
            let
                message =
                    if model.waitingServerConfirmation then
                        Message.successMessage "Activity Group successfully updated"
                    else
                        initMessage
            in
                ( model, Cmd.none, message )

        ActivityGroupRemoved id ->
            let
                message =
                    if model.waitingServerConfirmation then
                        Message.successMessage "Activity Group successfully removed"
                    else
                        initMessage
            in
                ( model, Cmd.none, message )

        Submit ->
            let
                --simulate changes in all form fields, to see if it they are valid if they didn't change
                ( model1, _, _ ) =
                    update (NameInput model.name) model

                showError =
                    model1.nameError /= Nothing

                ( updatedModel, cmd ) =
                    if showError then
                        ( { model1 | showErrorPanel = True }, Cmd.none )
                    else if model1.activityGroup.id == "" then
                        ( { initModel | waitingServerConfirmation = True }, addActivityGroup model1.activityGroup )
                    else
                        ( { initModel | waitingServerConfirmation = True }, updateActivityGroup model1.activityGroup )
            in
                ( updatedModel, cmd, initMessage )

        Cancel ->
            ( initModel, Cmd.none, initMessage )



-- view


view : Model -> List ActivityGroup -> Html Msg
view model activityGroups =
    let
        tempView =
            if model.showingForm then
                div [ class "main" ]
                    [ h1 [ class "ui header" ] [ text "ActivityGroup Form" ]
                    , errorPanel model
                    , activityGroupForm model
                    ]
            else
                div [ class "main" ]
                    [ activityGroupTable model activityGroups
                    , br [] []
                    , button [ class "ui button right", onClick NewActivityGroup ] [ text "New" ]
                    ]
    in
        tempView


activityGroupTable : Model -> List ActivityGroup -> Html Msg
activityGroupTable model activityGroups =
    -- filter using query afterwards
    let
        table_ =
            if List.isEmpty activityGroups then
                div [] [ text "No activity group found!" ]
            else
                List.map (activityGroupToTr) activityGroups
                    |> tbody []
                    |> (\r -> activityGroupsTh :: [ r ])
                    |> table [ class "ui celled table" ]
    in
        div []
            [ h1 [ class "ui header" ] [ text "Activity Groups list" ]
            , table_
            ]


activityGroupsTh : Html Msg
activityGroupsTh =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] [ text "Actions" ]
            ]
        ]


activityGroupToTr : ActivityGroup -> Html Msg
activityGroupToTr activityGroup =
    tr []
        [ td [] [ text activityGroup.id ]
        , td [] [ text activityGroup.name ]
        , td []
            [ button [ class "ui button", onClick (EditActivityGroup activityGroup) ] [ text "Edit" ]
            , button [ class "ui button", onClick (RemoveActivityGroup activityGroup) ] [ text "Remove" ]
            ]
        ]


activityGroupForm : Model -> Html Msg
activityGroupForm model =
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
            , div []
                [ label [] []
                , button [ type_ "submit", class "ui large submit button" ] [ text "Submit" ]
                , a [ class "ui large button red", onClick Cancel ] [ text "Cancel" ]
                ]
            ]
        ]


errorPanel : Model -> Html a
errorPanel model =
    let
        list : List FieldError
        list =
            [ { fieldName = "Name", errorMessage = model.nameError }
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


port activityGroupAdded : (ActivityGroup -> msg) -> Sub msg


port activityGroupUpdated : (ActivityGroup -> msg) -> Sub msg


port activityGroupRemoved : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ activityGroupAdded ActivityGroupAdded
        , activityGroupUpdated ActivityGroupUpdated
        , activityGroupRemoved ActivityGroupRemoved
        ]
