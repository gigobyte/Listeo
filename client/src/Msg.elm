module Msg exposing (Msg(..))

import Auth.Api exposing (User)
import Browser exposing (UrlRequest)
import Pages.CreatePlaylist.Model exposing (PlaylistStyle, PlaylistPrivacy)
import Pages.Login.Api exposing (LoginResponse)
import Pages.Register.Api exposing (RegisterResponse)
import RemoteData exposing (WebData)
import UI.TagInput exposing (Tag)
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
    | PlaylistTagAdded Tag
    | PlaylistTagRemoved Tag
    | PlaylistPrivacySelected PlaylistPrivacy
    | PlaylistStyleSelected PlaylistStyle
    | CreatePlaylistAttempted
