export type Route = string & { __brand: 'Route' }

const createRoute = (url: string): Route => url as Route

export const routes = {
  home: createRoute('/'),
  login: createRoute('/login'),
  register: createRoute('/register'),
  about: createRoute('/about'),
  createPlaylist: createRoute('/create-playlist'),
  notFound404: createRoute('/404')
}

export const parseUrl = (url: string): Route =>
  Object.values(routes).find(x => x === url) || routes.notFound404

export const isAuthProtectedRoute = (route: Route): boolean =>
  route === routes.home

export const isAuthDisallowedRoute = (route: Route): boolean =>
  route === routes.login || route === routes.register
