import React, { useEffect } from 'react'
import { configureStore } from 'redux-starter-kit'
import { Provider, useDispatch } from 'react-redux'
import { session, useUser } from './session'

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

  return <div>{JSON.stringify(user)}</div>
}
