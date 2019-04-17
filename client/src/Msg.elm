module Msg exposing (Msg(..))

import Auth.Api exposing (User)
import Browser exposing (UrlRequest)
import Pages.CreatePlaylist.Api exposing (CreatePlaylistResponse, CreatePlaylistResponseError)
import Pages.CreatePlaylist.Model exposing (PlaylistPrivacy, PlaylistStyle)
import Pages.Login.Api exposing (LoginResponse, LoginResponseError)
import Pages.Register.Api exposing (RegisterResponseError)
import Url exposing (Url)
import Utils.ErrorResponse exposing (HttpError, ResponseData)


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
      -- General
    | FetchUser (ResponseData () User)
    | Logout
      -- Login
    | LoginUsernameUpdated String
    | LoginPasswordUpdated String
    | LoginAttempted
    | Login (ResponseData LoginResponseError LoginResponse)
      -- Register
    | RegisterUsernameUpdated String
    | RegisterPasswordUpdated String
    | RegisterAttempted
    | Register (ResponseData RegisterResponseError ())
      -- Add playlist
    | AddPlaylistOverlayShown
    | CreateNewPlaylistSelected
    | AddPlaylistModalClosed
      -- Create playlist
    | PlaylistNameUpdated String
    | PlaylistTagInputUpdated String
    | PlaylistTagAdded String
    | PlaylistTagRemoved String
    | PlaylistPrivacySelected PlaylistPrivacy
    | PlaylistStyleSelected PlaylistStyle
    | CreatePlaylistAttempted
    | CreatePlaylist (ResponseData CreatePlaylistResponseError CreatePlaylistResponse)
