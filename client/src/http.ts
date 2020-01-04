import { Endpoint } from './endpoint'
import { useState, useEffect } from 'react'

// These are the only status codes the back-end can return that can be handled
export enum HttpStatus {
  BadRequest = 400,
  Unauthorized = 401
}

export interface FailedRequest {
  statusCode: HttpStatus
}

export const http = {
  async get<T>(url: Endpoint<T>): Promise<T> {
    const jwt = localStorage.getItem('jwt')
    const rawRes = await window.fetch(url, {
      method: 'GET',
      headers: {
        Authorization: 'Bearer ' + jwt
      }
    })

    if (rawRes.status === 500) {
      window.location.assign('/error')
      throw {}
    }

    if (!rawRes.ok) {
      const fail: FailedRequest = { statusCode: rawRes.status }
      throw fail
    }

    return rawRes.json()
  },

  async post<T>(url: Endpoint<T>, body: unknown): Promise<T> {
    const jwt = localStorage.getItem('jwt')
    const rawRes = await window.fetch(url, {
      method: 'POST',
      body: JSON.stringify(body),
      headers: {
        Authorization: 'Bearer ' + jwt
      }
    })

    if (rawRes.status === 500) {
      window.location.assign('/error')
      throw {}
    }

    const res = await rawRes.json()

    if (!rawRes.ok) {
      const fail: FailedRequest = { statusCode: res.status }
      throw { ...fail, ...res }
    }

    return res
  }
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
  | ({ status: DataStatus.Success } & T)
  | ({ status: DataStatus.Fail } & E)

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
  }),
  showError: <T, E>(
    error: RemoteData<T, E>,
    show: (error: E) => string
  ): string => {
    switch (error.status) {
      case DataStatus.Fail:
        return show(error)

      default:
        return ''
    }
  }
} as const

export const useAsync = <TArgs extends unknown[], TRes>(
  promiseFn: (...args: TArgs) => Promise<TRes>,
  args: TArgs
): RemoteData<TRes> => {
  const [data, setData] = useState<RemoteData<TRes>>(remoteData.loading)

  useEffect(() => {
    promiseFn(...args)
      .then(res => setData(remoteData.success(res)))
      .catch(err => setData(remoteData.fail(err)))
  }, args)

  return data
}
