import { createSlice, PayloadAction, createSelector } from 'redux-starter-kit'
import { http, FailedRequest } from './api/http'
import { endpoint } from './api/endpoint'
import { Dispatch } from 'redux'
import { useSelector } from 'react-redux'
import { Route, parseUrl } from './route'

export interface User {
  username: string
  createdOn: string
}

interface SessionState {
  user: User | null
  route: Route
}

export const session = {
  ...createSlice({
    name: 'session',

    initialState: {
      user: null,
      route: parseUrl(window.location.pathname)
    },

    reducers: {
      fetchUserSuccess(state: SessionState, action: PayloadAction<User>) {
        state.user = action.payload
      },
      fetchUserFailed(
        state: SessionState,
        action: PayloadAction<FailedRequest>
      ) {}
    }
  }),

  effects: {
    fetchUser() {
      return (dispatch: Dispatch) =>
        http
          .get(endpoint.currentUser)
          .then(user => dispatch(session.actions.fetchUserSuccess(user)))
          .catch(hmm => dispatch(session.actions.fetchUserFailed(hmm)))
    }
  }
}

export const useUser = () => useSelector((state: SessionState) => state.user)
