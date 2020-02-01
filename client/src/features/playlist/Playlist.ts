import { PlaylistTag } from './PlaylistTag'
import { Video } from '../video/Video'
import { Id } from '../../utils/id'

export enum PlaylistPrivacy {
  Public = 'Public',
  Private = 'Private'
}

export enum PlaylistStyle {
  Ranked = 'Ranked',
  Unordered = 'Unordered'
}

export interface Playlist {
  id: Id<Playlist>
  name: string
  description: string
  style: PlaylistStyle
  privacy: PlaylistPrivacy
  createdOn: string
  tags: PlaylistTag[]
  videos: Video[]
}
