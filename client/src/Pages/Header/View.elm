module Pages.Header.View exposing (view)

import Auth.Api exposing (User)
import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, title)
import Html.Styled.Events exposing (..)
import Msg exposing (Msg(..))
import Route
import UI.Colors exposing (blue200, crimson100, white)
import UI.Icon as Icon
import UI.Link as Link
import UI.Logo as Logo
import Utils.Styles exposing (StyledElement)


viewContainer : StyledElement msg
viewContainer =
    styled div
        [ displayFlex
        , justifyContent spaceBetween
        , backgroundColor white
        ]


viewNav : StyledElement msg
viewNav =
    styled div
        [ textTransform uppercase
        , displayFlex
        , alignItems center
        , paddingRight <| pct 4
        , fontSize <| rem 1.1
        , fontWeight bold
        ]


viewNavItem : Link.LinkProps -> StyledElement msg
viewNavItem props =
    styled (Link.view props)
        [ padding2 zero (px 10)
        , transition [ Transitions.color 500 ]
        , hover
            [ color blue200
            ]
        ]


viewLogo : StyledElement msg
viewLogo =
    styled div
        [ padding2 (px 10) (px 20)
        , paddingLeft <| pct 4
        ]


viewAddButton : StyledElement msg
viewAddButton =
    styled Icon.plusCircle
        [ fontSize <| px 25
        , paddingRight <| px 10
        , color crimson100
        , cursor pointer
        ]


viewPublicNavItems : List (Html msg)
viewPublicNavItems =
    [ viewNavItem { to = Route.Login } [] [ text "Sign In" ]
    , viewNavItem { to = Route.Register } [] [ text "Register" ]
    , viewNavItem { to = Route.About } [] [ text "About" ]
    ]


viewPrivateNavItems : User -> Bool -> List (Html Msg)
viewPrivateNavItems user showAddPlaylistButton =
    [ case showAddPlaylistButton of
        True ->
            viewAddButton
                [ title "Add new playlist"
                , attribute "role" "button"
                , onClick AddPlaylistOverlayShown
                ]
                []

        False ->
            text ""
    , viewNavItem { to = Route.Home } [] [ text user.username ]
    , viewNavItem { to = Route.Home } [ onClick Logout ] [ text "Logout" ]
    ]


type alias Props =
    { user : Maybe User
    , showAddPlaylistButton : Bool
    }


view : Props -> Html Msg
view props =
    viewContainer []
        [ viewLogo [] [ Logo.view ]
        , viewNav []
            (case props.user of
                Just user ->
                    viewPrivateNavItems user props.showAddPlaylistButton

                Nothing ->
                    viewPublicNavItems
            )
        ]
