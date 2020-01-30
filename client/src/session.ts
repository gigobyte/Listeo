import { createSlice, PayloadAction } from '@reduxjs/toolkit'
import {
  HttpStatus,
  FailedRequest,
  RemoteData,
  remoteData,
  http
} from './infrastructure/http'
import { createEndpoint } from './infrastructure/endpoint'
import { useSelector, useDispatch } from 'react-redux'
import {
  Route,
  parseUrl,
  routes,
  isAuthProtectedRoute,
  isAuthDisallowedRoute,
  routeToString
} from './route'
import { createBrowserHistory } from 'history'
import { AppDispatch } from './App'
import { useEffect } from 'react'

interface User {
  username: string
  createdOn: string
}

interface SessionState {
  user: RemoteData<User>
  route: Route
  jwt: string | null
}

const currentUserEndpoint = createEndpoint<User>('/me')
const history = createBrowserHistory()

const initialState: SessionState = {
  user: remoteData.notAsked,
  route: parseUrl(window.location.pathname),
  jwt: localStorage.getItem('jwt')
}

export const useHistory = () => {
  const dispatch = useDispatch()

  useEffect(() => {
    const unlisten = history.listen(location => {
      const route = parseUrl(location.pathname)
      dispatch(session.actions.locationChanged(route))
    })

    return unlisten
  }, [])
}

export const session = {
  ...createSlice({
    name: 'session',
    initialState,
    reducers: {
      locationChanged(state, action: PayloadAction<Route>) {
        state.route = action.payload
      },

      fetchUserStarted(state) {
        state.user = remoteData.loading
      },

      fetchUserSuccess(state, action: PayloadAction<User>) {
        state.user = remoteData.success(action.payload)
      },

      fetchUserFailed(state, action: PayloadAction<FailedRequest>) {
        state.user = remoteData.fail(action.payload)
      },

      storeJwt(state, action: PayloadAction<string>) {
        state.jwt = action.payload
      }
    }
  }),
  effects: {
    fetchUser() {
      return (dispatch: AppDispatch, getState: () => SessionState) => {
        dispatch(session.actions.fetchUserStarted())

        http
          .get(currentUserEndpoint)
          .then(user => {
            dispatch(session.actions.fetchUserSuccess(user))

            if (isAuthDisallowedRoute(getState().route)) {
              session.effects.redirect(routes.home)(dispatch)
            }
          })
          .catch((res: FailedRequest) => {
            dispatch(session.actions.fetchUserFailed(res))

            if (
              res.statusCode === HttpStatus.Unauthorized &&
              isAuthProtectedRoute(getState().route)
            ) {
              dispatch(session.effects.redirect(routes.login))
            }
          })
      }
    },

    redirect(route: Route) {
      return (dispatch: AppDispatch) => {
        dispatch(session.actions.locationChanged(route))
        history.push(routeToString(route))
      }
    },

    authSuccess(token: string) {
      return (dispatch: AppDispatch) => {
        localStorage.setItem('jwt', token)
        dispatch(session.actions.storeJwt(token))
        dispatch(session.effects.fetchUser())
        dispatch(session.effects.redirect(routes.home))
      }
    }
  }
}

export const useUser = () =>
  useSelector((session: SessionState) => session.user)

export const useRoute = () =>
  useSelector((session: SessionState) => session.route)

export const useJwt = () => useSelector((session: SessionState) => session.jwt)
