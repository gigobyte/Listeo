import React, { useState, useCallback } from 'react'
import styled from 'styled-components'
import useTitle from 'react-use/esm/useTitle'
import { Playlist, PlaylistPrivacy } from './playlist/Playlist'
import {
  http,
  useAsync,
  DataStatus,
  FailedRequest,
  PromiseWithError
} from '../utils/http'
import { Spinner } from '../ui/Spinner'
import { Icons } from '../ui/Icon'
import { colors } from '../ui/color'
import { Tooltip } from '../ui/Tooltip'
import { Tag } from '../ui/TagInput'
import { Button, ButtonStyle } from '../ui/Button'
import { AddVideoModal } from './video/AddVideoModal'
import youtubeLogo from '../assets/source_logos/youtube.jpg'
import vimeoLogo from '../assets/source_logos/vimeo.jpg'
import { VideoSource, getDuration, Video } from './video/Video'
import { formatDate } from '../utils/formatting'
import { Id } from '../utils/id'

interface ViewPlaylistProps {
  playlistId: Id<Playlist>
}

enum PlaylistResponseError {
  PlaylistNotFound = 'PlaylistNotFound',
  PlaylistIsPrivate = 'PlaylistIsPrivate'
}

interface PlaylistFailResponse extends FailedRequest {
  error: PlaylistResponseError
}

const fetchPlaylist = (
  playlistId: Id<Playlist>
): PromiseWithError<Playlist, PlaylistFailResponse> =>
  http.get<Playlist>('/playlist/' + playlistId)

const deleteVideo = (
  playlistId: Id<Playlist>,
  videoId: Id<Video>
): Promise<void> =>
  http.delete<void>('/playlist/' + playlistId + '/video/' + videoId)

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
  display: flex;
  padding: 5px 0;
`

const VideoThumbnail = styled.img`
  width: 200px;
  height: 113px;
`

const VideoThumbnailWrapper = styled.div`
  position: relative;
`

const VideoDetails = styled.div`
  padding-left: 10px;
`

const VideoHeader = styled.div`
  display: flex;
  align-items: center;
  padding-bottom: 10px;
`

const VideoTitle = styled.span`
  font-size: 1.2rem;
  padding-left: 5px;
`

const VideoSourceImage = styled.img`
  width: 24px;
`

const VideoTags = styled.div`
  display: flex;
  padding-top: 17px;
`

const DeleteButtonWrapper = styled.div`
  display: flex;
  justify-content: flex-end;
  align-items: center;
  flex: 1;
  font-size: 1.8rem;
  padding-right: 20px;
`

const Separator = styled.hr`
  border-top: 1px solid ${colors.gray300};
  border-left: 0;
`

const VideoDuration = styled.span`
  position: absolute;
  right: 0;
  bottom: 4px;
  padding: 2px 3px 0px 3px;
  color: ${colors.white};
  background-color: rgba(0, 0, 0, 0.7);
  font-size: 0.9rem;
`

const sourceToLogo = {
  [VideoSource.YouTube]: youtubeLogo,
  [VideoSource.Vimeo]: vimeoLogo
}

export const ViewPlaylist = ({ playlistId }: ViewPlaylistProps) => {
  const playlist = useAsync(fetchPlaylist, [playlistId])

  const [isAddVideoModalOpen, setIsAddVideoModalOpen] = useState(false)

  const deletePlaylistVideo = useCallback(
    (videoId: Id<Video>) => {
      deleteVideo(playlistId, videoId).then(playlist.refetch)
    },
    [playlistId]
  )

  useTitle(
    (() => {
      switch (playlist.response.status) {
        case DataStatus.Success:
          return `${playlist.response.data.name} - Listeo`

        default:
          return 'Playlist - Listeo'
      }
    })()
  )

  switch (playlist.response.status) {
    case DataStatus.Success: {
      const playlistData = playlist.response.data

      return (
        <div>
          <PlaylistHeader>
            <PlaylistTitle>{playlistData.name}</PlaylistTitle>
            <PlaylistActions>
              {playlistData.privacy === PlaylistPrivacy.Private && (
                <Tooltip label="This playlist is private">
                  <PrivateIcon />
                </Tooltip>
              )}
              {playlistData.privacy === PlaylistPrivacy.Public && (
                <Tooltip label="This playlist is public">
                  <PublicIcon />
                </Tooltip>
              )}
            </PlaylistActions>
          </PlaylistHeader>
          {playlistData.description && (
            <PlaylistDescription>
              {playlistData.description}
            </PlaylistDescription>
          )}
          {playlistData.tags.length > 0 && (
            <PlaylistTags>
              {playlistData.tags.map(tag => (
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
          {playlistData.videos.map(video => (
            <>
              <VideoWrapper key={video.id}>
                <VideoThumbnailWrapper>
                  <VideoThumbnail src={video.thumbnail} />
                  <VideoDuration>{getDuration(video.duration)}</VideoDuration>
                </VideoThumbnailWrapper>
                <VideoDetails>
                  <VideoHeader>
                    <VideoSourceImage src={sourceToLogo[video.source]} />
                    <VideoTitle>{video.title}</VideoTitle>
                  </VideoHeader>
                  <div>Date added: {formatDate(video.createdOn)}</div>
                  <VideoTags>
                    {video.tags.map(tag => (
                      <Tag key={tag.name} label={tag.name} />
                    ))}
                  </VideoTags>
                </VideoDetails>
                <DeleteButtonWrapper>
                  <Icons.trash
                    clickable
                    onClick={() => deletePlaylistVideo(video.id)}
                  />
                </DeleteButtonWrapper>
              </VideoWrapper>
              <Separator />
            </>
          ))}
          {isAddVideoModalOpen && (
            <AddVideoModal
              playlistId={playlistId}
              onClose={() => setIsAddVideoModalOpen(false)}
              onRefetchPlaylist={playlist.refetch}
            />
          )}
        </div>
      )
    }

    case DataStatus.Fail:
      if (
        playlist.response.error.error ===
        PlaylistResponseError.PlaylistIsPrivate
      )
        return <div>You do not have access to this playlist</div>
      else return null

    default:
      return <Spinner />
  }
}
