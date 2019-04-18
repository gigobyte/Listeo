module Feature.Playlist.GetPlaylist.GetPlaylistError where

    import Protolude
    import Data.Aeson (ToJSON)
    
    instance ToJSON GetPlaylistError
    data GetPlaylistError
        = PlaylistNotFound
        deriving Generic
    
    