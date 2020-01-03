/**
 * Temporary implementation until I figure out url parsing
 */

export enum RouteTag {
  Home = 'home',
  Login = 'login',
  Register = 'register',
  About = 'about',
  CreatePlaylist = 'create-playlist',
  ViewPlaylist = 'view-playlist',
  NotFound404 = '404',
  Error = 'error'
}

export type Route = (
  | { tag: RouteTag.Home }
  | { tag: RouteTag.Login }
  | { tag: RouteTag.Register }
  | { tag: RouteTag.About }
  | { tag: RouteTag.CreatePlaylist }
  | { tag: RouteTag.ViewPlaylist; params: { playlistId: string } }
  | { tag: RouteTag.NotFound404 }
  | { tag: RouteTag.Error }) & {
  __brand: 'Route'
}

const createRoute = (tag: RouteTag, params?: Record<string, string>): Route =>
  ({ tag, params } as Route)

export const routes = {
  home: createRoute(RouteTag.Home),
  login: createRoute(RouteTag.Login),
  register: createRoute(RouteTag.Register),
  about: createRoute(RouteTag.About),
  createPlaylist: createRoute(RouteTag.CreatePlaylist),
  viewPlaylist: (playlistId: string) =>
    createRoute(RouteTag.ViewPlaylist, { playlistId }),
  notFound404: createRoute(RouteTag.NotFound404),
  error: createRoute(RouteTag.Error)
}

export const routeToString = (route: Route): string => {
  switch (route.tag) {
    case RouteTag.Home:
      return '/'

    case RouteTag.Login:
      return '/login'

    case RouteTag.Register:
      return '/register'

    case RouteTag.About:
      return '/about'

    case RouteTag.CreatePlaylist:
      return '/create-playlist'

    case RouteTag.ViewPlaylist:
      return '/playlist/' + route.params.playlistId

    case RouteTag.NotFound404:
      return '/404'

    case RouteTag.Error:
      return '/error'
  }
}

export const parseUrl = (url: string): Route => {
  if (url === '/') {
    return routes.home
  } else if (url === '/login') {
    return routes.login
  } else if (url === '/register') {
    return routes.register
  } else if (url === '/about') {
    return routes.about
  } else if (url === '/create-playlist') {
    return routes.createPlaylist
  } else if (url.includes('/playlist/')) {
    return routes.viewPlaylist(url.replace('/playlist/', ''))
  } else if (url === '/404') {
    return routes.notFound404
  } else if (url === '/error') {
    return routes.error
  }

  return routes.notFound404
}

export const isAuthProtectedRoute = (route: Route): boolean =>
  route === routes.home

export const isAuthDisallowedRoute = (route: Route): boolean =>
  route === routes.login || route === routes.register
