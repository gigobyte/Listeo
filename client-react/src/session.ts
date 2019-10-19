import { createSlice, PayloadAction } from 'redux-starter-kit'
import { http, HttpStatus, FailedRequest } from './http'
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

export interface User {
  username: string
  createdOn: string
}

interface SessionState {
  user: User | null
  route: Route
}

const initialState: SessionState = {
  user: null,
  route: parseUrl(window.location.pathname)
}

export const session = {
  ...createSlice({
    name: 'session',
    initialState,
    reducers: {
      fetchUserSuccess(state, action: PayloadAction<User>) {
        state.user = action.payload

        if (isAuthDisallowedRoute(state.route)) {
          state.route = routes.home
        }
      },
      fetchUserFailed(state, action: PayloadAction<FailedRequest>) {
        if (
          action.payload.status === HttpStatus.Unauthorized &&
          isAuthProtectedRoute(state.route)
        ) {
          state.route = routes.login
        }
      }
    }
  }),

  effects: {
    fetchUser() {
      return (dispatch: Dispatch) =>
        http
          .get(endpoints.currentUser)
          .then(user => dispatch(session.actions.fetchUserSuccess(user)))
          .catch(res => dispatch(session.actions.fetchUserFailed(res)))
    }
  }
}

export const useUser = (): User | null =>
  useSelector((session: SessionState) => session.user)

export const useRoute = (): Route =>
  useSelector((session: SessionState) => session.route)
