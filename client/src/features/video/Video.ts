import { Id } from '../../id'

export enum VideoSource {
  YouTube = 'YouTube',
  Vimeo = 'Vimeo'
}

export interface Video {
  id: Id<Video>
  name: string
  source: VideoSource
  link: string
  note: string
  createdOn: string
}
