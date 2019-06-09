module UI.Modal exposing (view)

import Css exposing (..)
import Css.Animations as Animations exposing (Keyframes, keyframes)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import Styles exposing (StyledElement)
import UI.Colors exposing (black, blue200, white)
import UI.Icon as Icon


type alias ModalProps msg =
    { onClose : msg
    }


fadeIn : Keyframes {}
fadeIn =
    keyframes
        [ ( 0, [ Animations.opacity zero ] )
        , ( 100, [ Animations.opacity <| int 1 ] )
        ]


viewOverlay : StyledElement msg
viewOverlay =
    styled div
        [ position fixed
        , top zero
        , right zero
        , bottom zero
        , left zero
        , zIndex <| int 999
        , overflow hidden
        ]


viewContainer : StyledElement msg
viewContainer =
    styled div
        [ position relative
        , width <| pct 50
        , margin auto
        , top <| pct 20
        , backgroundColor white
        , padding2 (px 40) zero
        , borderRadius <| px 5
        , animationName fadeIn
        , animationDuration <| ms 200
        ]


viewBackdrop : StyledElement msg
viewBackdrop =
    styled div
        [ position fixed
        , top zero
        , right zero
        , bottom zero
        , left zero
        , opacity <| num 0.3
        , backgroundColor black
        ]


closeIcon : StyledElement msg
closeIcon =
    styled Icon.times
        [ position absolute
        , top <| px 10
        , right <| px 10
        , textAlign right
        , color blue200
        , cursor pointer
        ]


view : ModalProps msg -> StyledElement msg
view props attrs children =
    div []
        [ viewOverlay attrs
            [ viewContainer [] (closeIcon [ onClick props.onClose ] [] :: children)
            ]
        , viewBackdrop [] []
        ]
