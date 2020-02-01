import { Endpoint } from './endpoint'
import { useState, useEffect } from 'react'

// These are the only status codes the back-end can return that can be handled
export enum HttpStatus {
  BadRequest = 400,
  Unauthorized = 401
}

export type PromiseWithError<T, E> = Promise<T>

export interface FailedRequest {
  statusCode: HttpStatus
}

const handleRequest = async <T>(
  request: () => Promise<Response>
): Promise<T> => {
  let rawRes: Response

  try {
    rawRes = await request()
  } catch {
    window.location.assign('/error')
    throw {}
  }

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

export const http = {
  async get<T>(url: Endpoint<T>): Promise<T> {
    const jwt = localStorage.getItem('jwt')

    return handleRequest(() =>
      window.fetch(url, {
        method: 'GET',
        headers: {
          Authorization: 'Bearer ' + jwt
        }
      })
    )
  },

  async post<T>(url: Endpoint<T>, body: unknown): Promise<T> {
    const jwt = localStorage.getItem('jwt')

    return handleRequest(() =>
      window.fetch(url, {
        method: 'POST',
        body: JSON.stringify(body),
        headers: {
          Authorization: 'Bearer ' + jwt
        }
      })
    )
  },

  async delete<T>(url: Endpoint<T>): Promise<T> {
    const jwt = localStorage.getItem('jwt')

    return handleRequest(() =>
      window.fetch(url, {
        method: 'DELETE',
        headers: {
          Authorization: 'Bearer ' + jwt
        }
      })
    )
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

export const useAsync = <TArgs extends unknown[], TRes, TErr = FailedRequest>(
  promiseFn: (...args: TArgs) => PromiseWithError<TRes, TErr>,
  args: TArgs
): { data: RemoteData<TRes, TErr>; refetch: () => void } => {
  const [data, setData] = useState<RemoteData<TRes, TErr>>(remoteData.loading)

  const startFetch = () =>
    promiseFn(...args)
      .then(res => setData(remoteData.success(res)))
      .catch(err => setData(remoteData.fail(err)))

  useEffect(() => {
    startFetch()
  }, args)

  return {
    data,
    refetch: startFetch
  }
}
