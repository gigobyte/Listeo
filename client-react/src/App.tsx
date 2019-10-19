import React, { useEffect } from 'react'
import styled, { createGlobalStyle } from 'styled-components'
import { configureStore } from 'redux-starter-kit'
import { Provider, useDispatch } from 'react-redux'
import { session, useUser, useRoute } from './session'
import { routes } from './route'
import { colors } from './ui/color'
import { DataStatus } from './http'

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

const GlobalStyle = createGlobalStyle`
  html, body {
    height: 100%;
    margin: 0;
    font-family: 'Museo-Sans';
    background-color: ${colors.gray200}
  }
`

const Screen = styled.main`
  height: 100%;
  display: flex;
  flex-direction: column;
`

const Layout: React.FC = ({ children }) => (
  <>
    <GlobalStyle />
    <Screen>{children}</Screen>
  </>
)

export const Main = () => {
  const dispatch = useDispatch()

  useEffect(() => {
    dispatch(session.effects.fetchUser())
  }, [])

  const user = useUser()
  const route = useRoute()

  switch (user.type) {
    case DataStatus.NotAsked:
    case DataStatus.Loading:
      return null

    default: {
      switch (route) {
        case routes.home:
          return null

        case routes.login:
          return <Layout>Hi login</Layout>

        case routes.notFound404:
        default:
          return null
      }
    }
  }
}
