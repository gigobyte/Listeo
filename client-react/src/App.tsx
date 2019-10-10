import React, { useContext } from 'react'
import { useAsync } from 'react-async'
import { http } from './api/http'
import { endpoint } from './api/endpoint'
import { SessionContext } from './session'

const fetchUser = () => http.get(endpoint.currentUser)

export const App = () => {
  const { data: user, isPending } = useAsync({ promiseFn: fetchUser })

  if (isPending) {
    return null
  }

  return (
    <SessionContext.Provider value={{ user }}>
      <Main />
    </SessionContext.Provider>
  )
}

export const Main = () => {
  const session = useContext(SessionContext)

  return <div>{JSON.stringify(session.user)}</div>
}
