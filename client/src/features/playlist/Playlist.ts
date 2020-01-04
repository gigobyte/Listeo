import { PlaylistTag } from './PlaylistTag'

export enum PlaylistPrivacy {
  Public = 'Public',
  Private = 'Private'
}

export enum PlaylistStyle {
  Ranked = 'Ranked',
  Unordered = 'Unordered'
}

export interface Playlist {
  id: string
  name: string
  style: PlaylistStyle
  privacy: PlaylistPrivacy
  createdOn: string
  tags: PlaylistTag[]
}
