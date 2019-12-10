/**
 * Temporary implementation until I figure out url parsing
 */

export type Route = {
  tag: string
  params?: Record<string, string>
} & {
  __brand: 'Route'
}

const createRoute = (tag: string, params?: Record<string, string>): Route =>
  ({ tag, params } as Route)

export const routes = {
  home: createRoute('home'),
  login: createRoute('login'),
  register: createRoute('register'),
  about: createRoute('about'),
  createPlaylist: createRoute('create-playlist'),
  viewPlaylist: (playlistId: string) =>
    createRoute('view-playlist', { playlistId }),
  notFound404: createRoute('404')
}

export const routeToString = (route: Route): string => {
  switch (route.tag) {
    case 'home':
      return '/'

    case 'login':
      return '/login'

    case 'register':
      return '/register'

    case 'about':
      return '/about'

    case 'create-playlist':
      return '/create-playlist'

    case 'view-playlist':
      return '/playlist/' + route.params!.playlistId

    case '404':
      return '/404'
  }

  throw new Error('Invalid route')
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
  }

  return routes.notFound404
}

export const isAuthProtectedRoute = (route: Route): boolean =>
  route === routes.home

export const isAuthDisallowedRoute = (route: Route): boolean =>
  route === routes.login || route === routes.register
