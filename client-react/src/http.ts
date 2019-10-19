import { Endpoint } from './endpoint'

export enum HttpStatus {
  Unauthorized = 401
}

export interface FailedRequest {
  status: HttpStatus
}

export const http = {
  async get<T, E>(url: Endpoint<T>): Promise<T> {
    const res = await window.fetch(url)
    if (!res.ok) throw { status: res.status }
    return res.json()
  }
}

export enum DataStatus {
  NotAsked = 'NotAsked',
  Loading = 'Loading',
  Success = 'Success',
  Fail = 'Fail'
}

export type RemoteData<T> =
  | { status: DataStatus.NotAsked }
  | { status: DataStatus.Loading }
  | { status: DataStatus.Success } & T
  | { status: DataStatus.Fail }

export const remoteData = {
  notAsked: { status: DataStatus.NotAsked },
  loading: { status: DataStatus.Loading },
  success: <T>(data: T): RemoteData<T> => ({
    status: DataStatus.Success,
    ...data
  }),
  fail: { status: DataStatus.Fail }
} as const
