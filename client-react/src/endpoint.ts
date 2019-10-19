import { User } from './session'

export type Endpoint<T> = string & { __brand: 'Endpoint' & T }

const createEndpoint = <T>(url: string): Endpoint<T> =>
  (process.env.API_ROOT + url) as Endpoint<T>

export const endpoints = {
  currentUser: createEndpoint<User>('/me')
}
