export type Endpoint<T> = string & { __brand: 'Endpoint' & T }

export const createEndpoint = <T>(url: string): Endpoint<T> =>
  (process.env.API_ROOT + url) as Endpoint<T>
