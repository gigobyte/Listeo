module UI.Modal exposing (view)

import Css exposing (..)
import Css.Animations as Animations exposing (Keyframes, keyframes)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import UI.Colors exposing (black, blue150, white)
import UI.Icon as Icon
import Utils.Styles exposing (StyledElement)


type alias ModalProps msg =
    { onClose : msg
    }


fadeIn : Keyframes {}
fadeIn =
    keyframes
        [ ( 0, [ Animations.opacity zero ] )
        , ( 100, [ Animations.opacity <| int 1 ] )
        ]


overlay : StyledElement msg
overlay =
    styled div
        [ position fixed
        , top zero
        , right zero
        , bottom zero
        , left zero
        , zIndex <| int 999
        , overflow hidden
        ]


container : StyledElement msg
container =
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


backdrop : StyledElement msg
backdrop =
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
        , color blue150
        , cursor pointer
        ]


view : ModalProps msg -> StyledElement msg
view props attrs children =
    div []
        [ overlay attrs
            [ container [] ([ closeIcon [ onClick props.onClose ] [] ] ++ children)
            ]
        , backdrop [] []
        ]
