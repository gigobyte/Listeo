export type Route = string & { __brand: 'Route' }

const createRoute = (url: string): Route => url as Route

export const route = {
  home: createRoute('/'),
  login: createRoute('/login'),
  notFound404: createRoute('/404')
}

export const parseUrl = (url: string): Route =>
  Object.values(route).find(x => x === url) || route.notFound404
