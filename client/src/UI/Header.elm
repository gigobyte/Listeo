module UI.Header exposing (view)

import Auth.Api exposing (User)
import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import FontAwesome.Attributes as Icon
import FontAwesome.Solid as Icon
import Html.Styled exposing (..)
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import Route
import UI.Colors exposing (blue150, blue50)
import UI.Link as Link
import UI.Logo as Logo
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
    styled div
        [ displayFlex
        , justifyContent spaceBetween
        ]


nav : StyledElement msg
nav =
    styled div
        [ textTransform uppercase
        , displayFlex
        , alignItems center
        , paddingRight <| pct 4
        , fontSize <| rem 1.1
        , fontWeight bold
        ]


navItem : Link.LinkProps -> StyledElement msg
navItem props =
    styled (Link.view props)
        [ padding2 zero (px 10)
        , transition [ Transitions.color 500 ]
        , hover
            [ color blue50
            ]
        ]


logo : StyledElement msg
logo =
    styled div
        [ padding <| px 20
        , paddingLeft <| pct 4
        ]


viewPublicNavItems : List (Html msg)
viewPublicNavItems =
    [ navItem { to = Route.Login } [] [ text "Sign In" ]
    , navItem { to = Route.Register } [] [ text "Register" ]
    , navItem { to = Route.About } [] [ text "About" ]
    ]


viewPrivateNavItems : User -> List (Html Msg)
viewPrivateNavItems user =
    [ navItem { to = Route.Home } [] [ text user.username ]
    , fromUnstyled <| Icon.camera [ Icon.stack1x ]
    ]


view : AppModel -> Html Msg
view model =
    container []
        [ logo [] [ Logo.view ]
        , nav []
            (case model.auth.user of
                Just user ->
                    viewPrivateNavItems user

                Nothing ->
                    viewPublicNavItems
            )
        ]
