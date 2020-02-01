import { Id } from '../../utils/id'
import { parse } from 'iso8601-duration'

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
  duration: string
  tags: VideoTag[]
}

export interface VideoTag {
  name: string
}

const durationFormatter = Intl.NumberFormat(undefined, {
  minimumIntegerDigits: 2
})

export const getDuration = (iso8106Duration: string): string => {
  const duration = parse(iso8106Duration)
  return `${durationFormatter.format(
    duration.hours ?? 0
  )}:${durationFormatter.format(
    duration.minutes ?? 0
  )}:${durationFormatter.format(duration.seconds ?? 0)}`
}
