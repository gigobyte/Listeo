import React, { useEffect } from 'react'
import { configureStore } from 'redux-starter-kit'
import { Provider, useDispatch } from 'react-redux'
import { session, useUser, useRoute } from './session'
import { routes } from './route'

const store = configureStore({
  reducer: session.reducer
})

export const App = () => {
  return (
    <Provider store={store}>
      <Main />
    </Provider>
  )
}

export const Main = () => {
  const dispatch = useDispatch()

  useEffect(() => {
    dispatch(session.effects.fetchUser())
  }, [])

  const user = useUser()
  const route = useRoute()

  if (!user) {
    return null
  }

  switch (route) {
    case routes.home:
      return null

    case routes.login:
      return null

    case routes.notFound404:
    default:
      return null
  }
}
