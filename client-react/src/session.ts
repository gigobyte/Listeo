import { createSlice, PayloadAction } from 'redux-starter-kit'
import { http, HttpStatus, FailedRequest, RemoteData, remoteData } from './http'
import { endpoints } from './endpoint'
import { Dispatch } from 'redux'
import { useSelector } from 'react-redux'
import {
  Route,
  parseUrl,
  routes,
  isAuthProtectedRoute,
  isAuthDisallowedRoute
} from './route'
import { createBrowserHistory } from 'history'

export interface User {
  username: string
  createdOn: string
}

interface SessionState {
  user: RemoteData<User>
  route: Route
}

const history = createBrowserHistory()

const initialState: SessionState = {
  user: remoteData.notAsked,
  route: parseUrl(window.location.pathname)
}

export const session = {
  ...createSlice({
    name: 'session',
    initialState,
    reducers: {
      locationChanged(state, action: PayloadAction<Route>) {
        state.route = action.payload
      },
      fetchUserSuccess(state, action: PayloadAction<User>) {
        state.user = remoteData.success(action.payload)
      },
      fetchUserFailed(state) {
        state.user = remoteData.fail
      }
    }
  }),

  effects: {
    fetchUser() {
      return (dispatch: Dispatch, getState: () => SessionState) =>
        http
          .get(endpoints.currentUser)
          .then(user => {
            dispatch(session.actions.fetchUserSuccess(user))

            if (isAuthDisallowedRoute(getState().route)) {
              session.effects.redirect(routes.home)(dispatch)
            }
          })
          .catch((res: FailedRequest) => {
            dispatch(session.actions.fetchUserFailed())

            if (
              res.status === HttpStatus.Unauthorized &&
              isAuthProtectedRoute(getState().route)
            ) {
              session.effects.redirect(routes.login)(dispatch)
            }
          })
    },

    redirect(route: Route) {
      return (dispatch: Dispatch) => {
        dispatch(session.actions.locationChanged(route))
        history.push(route)
      }
    }
  }
}

export const useUser = () =>
  useSelector((session: SessionState) => session.user)

export const useRoute = () =>
  useSelector((session: SessionState) => session.route)
