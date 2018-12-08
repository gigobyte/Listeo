module Selectors exposing (getNavKey, getRoute)

import Browser.Navigation as Nav
import Model exposing (AppModel)
import Route exposing (Route)


getNavKey : AppModel -> Nav.Key
getNavKey model =
    model.key


getRoute : AppModel -> Route
getRoute model =
    model.url
