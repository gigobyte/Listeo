module Msg exposing (Msg(..))

import Auth.Api exposing (User)
import Browser exposing (UrlRequest)
import Pages.CreatePlaylist.Api exposing (CreatePlaylistResponse)
import Pages.CreatePlaylist.Model exposing (PlaylistPrivacy, PlaylistStyle)
import Pages.Login.Api exposing (LoginResponse)
import Pages.Register.Api exposing (RegisterResponse)
import RemoteData exposing (WebData)
import Url exposing (Url)


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
      -- General
    | FetchUser (WebData User)
    | Logout
      -- Login
    | LoginUsernameUpdated String
    | LoginPasswordUpdated String
    | LoginAttempted
    | Login (WebData LoginResponse)
      -- Register
    | RegisterUsernameUpdated String
    | RegisterPasswordUpdated String
    | RegisterAttempted
    | Register (WebData RegisterResponse)
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
    | CreatePlaylist (WebData CreatePlaylistResponse)
