module Routes exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), top)


type Route
    = IndexRoute
    | LoginRoute
    | NotFoundRoute


routeToHash : Route -> String
routeToHash route =
    case route of
        IndexRoute ->
            "#/"

        LoginRoute ->
            "#/login"

        NotFoundRoute ->
            "#notfound"


matchers : Url.Parser (Route -> a) a
matchers =
    Url.oneOf
        [ Url.map IndexRoute top
        , Url.map LoginRoute (Url.s "login")
        ]


locationToRoute : Location -> Route
locationToRoute location =
    case (Url.parseHash matchers location) of
        Nothing ->
            NotFoundRoute

        Just route ->
            route
