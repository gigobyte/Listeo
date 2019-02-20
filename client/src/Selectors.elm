module Selectors exposing (getRoute)

import Model exposing (AppModel)
import Route exposing (Route)


getRoute : AppModel -> Route
getRoute model =
    model.route
