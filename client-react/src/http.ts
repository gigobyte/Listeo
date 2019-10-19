import { Endpoint } from './endpoint'
import { useDispatch } from 'react-redux'
import { useJwt } from './session'

export enum HttpStatus {
  Unauthorized = 401
}

export interface FailedRequest {
  status: HttpStatus
}

export const createHttp = (jwt: string | null) => ({
  async get<T>(url: Endpoint<T>): Promise<T> {
    const res = await window.fetch(url, {
      method: 'GET',
      headers: {
        Authorization: 'Bearer ' + jwt
      }
    })

    if (!res.ok) {
      throw { status: res.status }
    }

    return res.json()
  },

  async post<T>(url: Endpoint<T>, body: any): Promise<T> {
    const rawRes = await window.fetch(url, {
      method: 'POST',
      body: JSON.stringify(body),
      headers: {
        Authorization: 'Bearer ' + jwt
      }
    })
    const res = await rawRes.json()

    if (!rawRes.ok) {
      throw { status: rawRes.status, ...res }
    }

    return res
  }
})

export const useHttp = () => {
  const jwt = useJwt()

  return createHttp(jwt)
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
