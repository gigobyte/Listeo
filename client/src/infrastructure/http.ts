import { Endpoint } from './endpoint'
import { useState, useEffect } from 'react'
import axios, { AxiosError } from 'axios'

// These are the only status codes the back-end can return that can be handled
export enum HttpStatus {
  BadRequest = 400,
  Unauthorized = 401
}

export type PromiseWithError<T, E> = Promise<T>

export interface FailedRequest {
  statusCode: HttpStatus
}

const handleErrors = (err: AxiosError) => {
  if (err.response?.status === 500) {
    window.location.assign('/error')
  }

  return err
}

export const http = {
  async get<T>(url: Endpoint<T>): Promise<T> {
    const jwt = localStorage.getItem('jwt')

    return axios
      .get(url, {
        headers: {
          Authorization: 'Bearer ' + jwt
        }
      })
      .then(res => res.data)
      .catch(handleErrors)
  },

  async post<T>(url: Endpoint<T>, body: unknown): Promise<T> {
    const jwt = localStorage.getItem('jwt')

    return axios
      .post(url, {
        body,
        headers: {
          Authorization: 'Bearer ' + jwt
        }
      })
      .then(res => res.data)
      .catch(handleErrors)
  },

  async delete<T>(url: Endpoint<T>): Promise<T> {
    const jwt = localStorage.getItem('jwt')

    return axios
      .delete(url, {
        headers: {
          Authorization: 'Bearer ' + jwt
        }
      })
      .then(res => res.data)
      .catch(handleErrors)
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
