import React from 'react'
import useTitle from 'react-use/esm/useTitle'
import { Playlist } from './Playlist'
import { http, useAsync, DataStatus } from '../../http'
import { createEndpoint } from '../../endpoint'

interface ViewPlaylistProps {
  playlistId: string
}

const fetchPlaylist = (playlistId: string): Promise<Playlist> =>
  http.get(createEndpoint<Playlist>('/playlist/' + playlistId))

export const ViewPlaylist = ({ playlistId }: ViewPlaylistProps) => {
  const playlist = useAsync(fetchPlaylist, [playlistId])

  useTitle(
    (() => {
      switch (playlist.status) {
        case DataStatus.Success:
          return `${playlist.name} - Listeo`

        default:
          return 'Playlist - Listeo'
      }
    })()
  )

  return <div />
}
