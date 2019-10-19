import { Endpoint } from './endpoint'

export interface FailedRequest {
  status: number
}

export const http = {
  async get<T>(url: Endpoint<T>): Promise<T> {
    const res = await window.fetch(url)
    if (!res.ok) throw { status: res.status }
    return res.json()
  }
}
