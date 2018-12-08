module Selectors exposing (getNavKey)

import Browser.Navigation as Nav
import Model exposing (AppModel)


getNavKey : AppModel -> Nav.Key
getNavKey model =
    model.key
