import React from 'react'

export interface User {
  username: string
  createdOn: string
}

interface SessionContextState {
  user: User | undefined
}

export const SessionContext = React.createContext<SessionContextState>({
  user: undefined
})
