import {
  HttpStatus,
  FailedRequest,
  RemoteData,
  http,
  useAsync,
  PromiseWithError,
  notAsked,
  isSuccess,
  isFail
} from './utils/http'
import {
  Route,
  parseUrl,
  routes,
  isAuthProtectedRoute,
  isAuthDisallowedRoute,
  routeToString
} from './route'
import { createBrowserHistory } from 'history'
import React, { useEffect, useState } from 'react'

interface User {
  username: string
  createdOn: string
}

interface SessionState {
  user: RemoteData<User>
  route: Route
  login: (jwt: string) => void
  logout: () => void
}

const fetchUser = (): PromiseWithError<User, FailedRequest> =>
  http.get<User>('/me')

const history = createBrowserHistory()

const SessionContext = React.createContext<SessionState | null>(null)

export const redirect = (route: Route): void => {
  history.push(routeToString(route))
}

export const SessionProvider: React.FC = ({ children }) => {
  const user = useAsync(fetchUser, [])
  const [route, setRoute] = useState(parseUrl(window.location.pathname))

  useEffect(() => {
    const unlisten = history.listen(location => {
      setRoute(parseUrl(location.pathname))
    })

    return unlisten
  }, [])

  useEffect(() => {
    if (isSuccess(user.response) && isAuthDisallowedRoute(route)) {
      redirect(routes.home)
    }

    if (
      isFail(user.response) &&
      user.response.error.statusCode === HttpStatus.Unauthorized &&
      isAuthProtectedRoute(route)
    ) {
      redirect(routes.login)
    }
  }, [user])

  return (
    <SessionContext.Provider
      value={{
        user: user.response,
        route,
        login: (jwt: string) => {
          localStorage.setItem('jwt', jwt)
          user.refetch()
          redirect(routes.home)
        },
        logout: () => {
          localStorage.removeItem('jwt')
          user.refetch()
        }
      }}
    >
      {children}
    </SessionContext.Provider>
  )
}

export const useUser = (): RemoteData<User> =>
  React.useContext(SessionContext)?.user ?? notAsked

export const useRoute = (): Route =>
  React.useContext(SessionContext)?.route ?? routes.notFound404

export const useSessionContext = (): SessionState =>
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  React.useContext(SessionContext)!
