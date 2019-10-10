import { Endpoint } from './endpoint'

export const http = {
  async get<T>(url: Endpoint<T>): Promise<T> {
    const res = await window.fetch(url)
    if (!res.ok) throw new Error()
    return res.json()
  }
}
