export type Route = { url: string; params?: Record<string, string> } & {
  __brand: 'Route'
}

const createRoute = (url: string, params?: Record<string, string>): Route =>
  ({ url, params } as Route)

export const routes = {
  home: createRoute('/'),
  login: createRoute('/login'),
  register: createRoute('/register'),
  about: createRoute('/about'),
  createPlaylist: createRoute('/create-playlist'),
  viewPlaylist: (playlistId: string) =>
    createRoute(`/playlist/${playlistId}`, { playlistId }),
  notFound404: createRoute('/404')
}

export const parseUrl = (url: string): Route => {}

export const isAuthProtectedRoute = (route: Route): boolean =>
  route === routes.home

export const isAuthDisallowedRoute = (route: Route): boolean =>
  route === routes.login || route === routes.register
