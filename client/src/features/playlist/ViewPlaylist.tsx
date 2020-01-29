import React, { useState } from 'react'
import styled from 'styled-components'
import useTitle from 'react-use/esm/useTitle'
import { Playlist, PlaylistPrivacy } from './Playlist'
import {
  http,
  useAsync,
  DataStatus,
  FailedRequest,
  PromiseWithError
} from '../../http'
import { createEndpoint } from '../../endpoint'
import { Spinner } from '../../ui/Spinner'
import { Icons } from '../../ui/Icon'
import { colors } from '../../ui/color'
import { Tooltip } from '../../ui/Tooltip'
import { Tag } from '../../ui/TagInput'
import { Button, ButtonStyle } from '../../ui/Button'
import { AddVideoModal } from '../video/AddVideoModal'

interface ViewPlaylistProps {
  playlistId: string
}

enum PlaylistResponseError {
  PlaylistNotFound = 'PlaylistNotFound',
  PlaylistIsPrivate = 'PlaylistIsPrivate'
}

interface PlaylistFailResponse extends FailedRequest {
  error: PlaylistResponseError
}

const fetchPlaylist = (
  playlistId: string
): PromiseWithError<Playlist, PlaylistFailResponse> =>
  http.get(createEndpoint<Playlist>('/playlist/' + playlistId))

const PlaylistHeader = styled.div`
  display: flex;
`

const PlaylistTitle = styled.h1`
  margin: 0;
`

const PlaylistActions = styled.div`
  align-self: center;
  padding-left: 15px;
  padding-top: 5px;
`

const PlaylistDescription = styled.span`
  display: inline-block;
  padding-top: 15px;
`

const PrivateIcon = styled(Icons.eyeSlash)`
  color: ${colors.crimson100};
`

const PublicIcon = styled(Icons.eye)`
  color: ${colors.green100};
`

const PlaylistTags = styled.div`
  display: flex;
  padding: 20px 0;
`

export const ViewPlaylist = ({ playlistId }: ViewPlaylistProps) => {
  const { data: playlist, refetch: refetchPlaylist } = useAsync(fetchPlaylist, [
    playlistId
  ])
  const [isAddVideoModalOpen, setIsAddVideoModalOpen] = useState(false)

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

  switch (playlist.status) {
    case DataStatus.Success:
      return (
        <div>
          <PlaylistHeader>
            <PlaylistTitle>{playlist.name}</PlaylistTitle>
            <PlaylistActions>
              {playlist.privacy === PlaylistPrivacy.Private && (
                <Tooltip label="This playlist is private">
                  <PrivateIcon />
                </Tooltip>
              )}
              {playlist.privacy === PlaylistPrivacy.Public && (
                <Tooltip label="This playlist is public">
                  <PublicIcon />
                </Tooltip>
              )}
            </PlaylistActions>
          </PlaylistHeader>
          <PlaylistDescription>{playlist.description}</PlaylistDescription>
          <PlaylistTags>
            {playlist.tags.map(tag => (
              <Tag key={tag.name} label={tag.name} />
            ))}
          </PlaylistTags>
          <Button
            styling={ButtonStyle.Primary}
            icon={<Icons.plusCircle />}
            onClick={() => setIsAddVideoModalOpen(true)}
          >
            Add
          </Button>
          <Button styling={ButtonStyle.Default} icon={<Icons.play />}>
            Play
          </Button>
          <Button styling={ButtonStyle.Default} icon={<Icons.edit />}>
            Edit
          </Button>
          {playlist.videos.map(video => (
            <div>
              <img style={{ width: 100, height: 100 }} src={video.thumbnail} />
              {video.title}
            </div>
          ))}
          {isAddVideoModalOpen && (
            <AddVideoModal
              playlistId={playlistId}
              onClose={() => setIsAddVideoModalOpen(false)}
              onRefetchPlaylist={refetchPlaylist}
            />
          )}
        </div>
      )

    case DataStatus.Fail:
      if (playlist.error === PlaylistResponseError.PlaylistIsPrivate)
        return <div>You do not have access to this playlist</div>
      else return null

    default:
      return <Spinner />
  }
}
