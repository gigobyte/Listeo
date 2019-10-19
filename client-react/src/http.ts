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
