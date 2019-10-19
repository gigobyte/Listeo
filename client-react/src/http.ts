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
  | { type: DataStatus.NotAsked }
  | { type: DataStatus.Loading }
  | { type: DataStatus.Success; data: T }
  | { type: DataStatus.Fail }

export const remoteData = {
  notAsked: { type: DataStatus.NotAsked },
  loading: { type: DataStatus.Loading },
  success: <T>(data: T): RemoteData<T> => ({ type: DataStatus.Success, data }),
  fail: { type: DataStatus.Fail }
} as const
