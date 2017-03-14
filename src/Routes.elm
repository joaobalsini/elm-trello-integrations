module Routes exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), top)


type Route
    = TrelloBoardRoute
    | ActivityRoute
    | ActivityGroupRoute
    | NotFoundRoute


routeToHash : Route -> String
routeToHash route =
    case route of
        TrelloBoardRoute ->
            "#/"

        ActivityRoute ->
            "#/activities"

        ActivityGroupRoute ->
            "#/activityGroups"

        NotFoundRoute ->
            "#notfound"


matchers : Url.Parser (Route -> a) a
matchers =
    Url.oneOf
        [ Url.map TrelloBoardRoute top
        , Url.map ActivityRoute (Url.s "activities")
        , Url.map ActivityGroupRoute (Url.s "activityGroups")
        ]


locationToRoute : Location -> Route
locationToRoute location =
    case (Url.parseHash matchers location) of
        Nothing ->
            NotFoundRoute

        Just route ->
            route
