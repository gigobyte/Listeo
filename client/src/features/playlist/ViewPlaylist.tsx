import React from 'react'
import useTitle from 'react-use/esm/useTitle'
import { useAsync } from 'react-async'
import { Playlist } from './Playlist'
import { http } from '../../http'
import { createEndpoint } from '../../endpoint'

interface ViewPlaylistProps {
  playlistId: string
}

const fetchPlaylist = (playlistId: string): Promise<Playlist> =>
  http.get(createEndpoint<Playlist>('/playlist' + playlistId))

export const ViewPlaylist = ({ playlistId }: ViewPlaylistProps) => {
  const { data, error, isPending } = useAsync(fetchPlaylist)

  useTitle('Create Playlist - Listeo')

  return <div />
}
