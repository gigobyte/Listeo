module Msg exposing (Msg(..))

import Browser
import Url


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    -- Login
    | LoginUsernameUpdated String
    | LoginPasswordUpdated String
    | LoginAttempted
