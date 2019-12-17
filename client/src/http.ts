import { Endpoint } from './endpoint'
import { useDispatch } from 'react-redux'
import { useJwt } from './session'

export enum HttpStatus {
  Unauthorized = 401,
  ServerError = 500
}

export interface FailedRequest {
  statusCode: HttpStatus
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
      const fail: FailedRequest = { statusCode: res.status }
      throw fail
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
      const fail: FailedRequest = { statusCode: res.status }
      throw { ...fail, ...res }
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

export type RemoteData<T, E = FailedRequest> =
  | { status: DataStatus.NotAsked }
  | { status: DataStatus.Loading }
  | { status: DataStatus.Success } & T
  | { status: DataStatus.Fail } & E

export const remoteData = {
  notAsked: { status: DataStatus.NotAsked },
  loading: { status: DataStatus.Loading },
  success: <T>(data: T): RemoteData<T, never> => ({
    status: DataStatus.Success,
    ...data
  }),
  fail: <E>(error: E): RemoteData<never, E> => ({
    status: DataStatus.Fail,
    ...error
  })
} as const
