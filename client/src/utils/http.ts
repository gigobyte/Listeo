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
    throw {}
  }

  throw { ...err.response?.data, statusCode: err.response?.status }
}

export const http = {
  async get<T>(url: string): Promise<T> {
    const jwt = localStorage.getItem('jwt')

    return axios
      .get(process.env.API_ROOT + url, {
        headers: {
          Authorization: 'Bearer ' + jwt
        }
      })
      .then(res => res.data)
      .catch(handleErrors)
  },

  async post<T>(url: string, body: unknown): Promise<T> {
    const jwt = localStorage.getItem('jwt')

    return axios
      .post(process.env.API_ROOT + url, body, {
        headers: {
          Authorization: 'Bearer ' + jwt
        }
      })
      .then(res => res.data)
      .catch(handleErrors)
  },

  async delete<T>(url: string): Promise<T> {
    const jwt = localStorage.getItem('jwt')

    return axios
      .delete(process.env.API_ROOT + url, {
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
  | { status: DataStatus.Success; data: T }
  | { status: DataStatus.Fail; error: E }

export const notAsked = { status: DataStatus.NotAsked } as const
export const loading = { status: DataStatus.Loading } as const
export const success = <T>(data: T) =>
  ({
    status: DataStatus.Success,
    data
  } as const)
export const fail = <E>(error: E) =>
  ({
    status: DataStatus.Fail,
    error
  } as const)

export const isSuccess = <T, E>(
  remoteData: RemoteData<T, E>
): remoteData is { status: DataStatus.Success; data: T } =>
  remoteData.status === DataStatus.Success

export const isFail = <T, E>(
  remoteData: RemoteData<T, E>
): remoteData is { status: DataStatus.Fail; error: E } =>
  remoteData.status === DataStatus.Fail

export const showError = <T, E>(
  error: RemoteData<T, E>,
  show: (error: E) => string
): string => {
  switch (error.status) {
    case DataStatus.Fail:
      return show(error.error)

    default:
      return ''
  }
}

export const useAsync = <TArgs extends unknown[], TRes, TErr = FailedRequest>(
  promiseFn: (...args: TArgs) => PromiseWithError<TRes, TErr>,
  args: TArgs
): { response: RemoteData<TRes, TErr>; refetch: () => void } => {
  const [data, setData] = useState<RemoteData<TRes, TErr>>(loading)

  const startFetch = () =>
    promiseFn(...args)
      .then(res => setData(success(res)))
      .catch(err => setData(fail(err)))

  useEffect(() => {
    startFetch()
  }, args)

  return {
    response: data,
    refetch: startFetch
  }
}

export const useCallableAsync = <
  TArgs extends unknown[],
  TRes,
  TErr = FailedRequest
>(
  promiseFn: (...args: TArgs) => PromiseWithError<TRes, TErr>
): { response: RemoteData<TRes, TErr>; fetch: (...args: TArgs) => void } => {
  const [data, setData] = useState<RemoteData<TRes, TErr>>(notAsked)

  const startFetch = (...args: TArgs) => {
    setData(loading)

    promiseFn(...args)
      .then(res => setData(success(res)))
      .catch(err => setData(fail(err)))
  }

  return {
    response: data,
    fetch: startFetch
  }
}
