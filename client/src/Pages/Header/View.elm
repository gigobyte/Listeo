module Pages.Header.View exposing (view)

import Auth.Api exposing (User)
import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, title)
import Html.Styled.Events exposing (..)
import Msg exposing (Msg(..))
import Pages.Header.Model exposing (Model)
import Route exposing (Route)
import UI.Colors exposing (blue200, crimson100, white)
import UI.Icon as Icon
import UI.Link as Link
import UI.Logo as Logo
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
    styled div
        [ displayFlex
        , justifyContent spaceBetween
        , backgroundColor white
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
            [ color blue200
            ]
        ]


logo : StyledElement msg
logo =
    styled div
        [ padding2 (px 10) (px 20)
        , paddingLeft <| pct 4
        ]


addButton : StyledElement msg
addButton =
    styled Icon.plusCircle
        [ fontSize <| px 25
        , paddingRight <| px 10
        , color crimson100
        , cursor pointer
        ]


viewPublicNavItems : List (Html msg)
viewPublicNavItems =
    [ navItem { to = Route.Login } [] [ text "Sign In" ]
    , navItem { to = Route.Register } [] [ text "Register" ]
    , navItem { to = Route.About } [] [ text "About" ]
    ]


viewPrivateNavItems : User -> Bool -> List (Html Msg)
viewPrivateNavItems user showAddPlaylistButton =
    [ case showAddPlaylistButton of
        True ->
            addButton
                [ title "Add new playlist"
                , attribute "role" "button"
                , onClick AddPlaylistOverlayShown
                ]
                []

        False ->
            text ""
    , navItem { to = Route.Home } [] [ text user.username ]
    , navItem { to = Route.Home } [ onClick Logout ] [ text "Logout" ]
    ]


type alias Props =
    { user : Maybe User
    , route : Route
    }


view : Props -> Html Msg
view props =
    container []
        [ logo [] [ Logo.view ]
        , nav []
            (case props.user of
                Just user ->
                    viewPrivateNavItems user (props.route /= Route.CreatePlaylist)

                Nothing ->
                    viewPublicNavItems
            )
        ]
