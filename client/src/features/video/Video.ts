import { Id } from '../../id'

export enum VideoSource {
  YouTube = 'YouTube',
  Vimeo = 'Vimeo'
}

export interface Video {
  id: Id<Video>
  title: string
  source: VideoSource
  url: string
  note: string
  thumbnail: string
  createdOn: string
}
