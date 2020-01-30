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
import youtubeLogo from '../../assets/source_logos/youtube.jpg'
import vimeoLogo from '../../assets/source_logos/vimeo.jpg'
import { VideoSource, getDuration } from '../video/Video'

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

const VideoWrapper = styled.div`
  width: 100px;
  display: flex;
`

const VideoThumbnail = styled.img`
  width: 200px;
`

const VideoDetails = styled.div`
  flex: 1;
`

const sourceToLogo = {
  [VideoSource.YouTube]: youtubeLogo,
  [VideoSource.Vimeo]: vimeoLogo
}

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
          {playlist.description && (
            <PlaylistDescription>{playlist.description}</PlaylistDescription>
          )}
          {playlist.tags.length > 0 && (
            <PlaylistTags>
              {playlist.tags.map(tag => (
                <Tag key={tag.name} label={tag.name} />
              ))}
            </PlaylistTags>
          )}
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
            <VideoWrapper key={video.id}>
              <VideoThumbnail src={video.thumbnail} />
              <VideoDetails>
                <img src={sourceToLogo[video.source]} />
                {video.title}
                {video.tags.map(tag => (
                  <Tag key={tag.name} label={tag.name} />
                ))}
                {getDuration(video.duration)}
              </VideoDetails>
            </VideoWrapper>
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
