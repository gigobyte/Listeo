module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Url exposing (Url)


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
      -- Login
    | LoginUsernameUpdated String
    | LoginPasswordUpdated String
    | LoginAttempted
