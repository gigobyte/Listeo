module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Pages.Register.Api exposing (RegisterResponse)
import RemoteData exposing (WebData)
import Url exposing (Url)


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
      -- Login
    | LoginUsernameUpdated String
    | LoginPasswordUpdated String
    | LoginAttempted
      -- Register
    | RegisterUsernameUpdated String
    | RegisterPasswordUpdated String
    | RegisterAttempted
    | Register (WebData RegisterResponse)
