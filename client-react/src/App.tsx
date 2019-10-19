import React, { useEffect } from 'react'
import styled, { createGlobalStyle } from 'styled-components'
import { configureStore } from 'redux-starter-kit'
import { Provider, useDispatch } from 'react-redux'
import { session, useUser, useRoute } from './session'
import { routes } from './route'
import { colors } from './ui/color'
import { DataStatus } from './http'
import { Header } from './ui/Header'
import MuseoSans from './assets/MuseoSans-100.ttf'

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
  @font-face {
    font-family: Museo-Sans;
    src: url(${MuseoSans});
  }

  html, body {
    height: 100%;
    margin: 0;
    font-family: 'Museo-Sans';
    background-color: ${colors.gray200};
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
    <Screen>
      <Header />
      {children}
    </Screen>
  </>
)

export const Main = () => {
  const dispatch = useDispatch()

  useEffect(() => {
    dispatch(session.effects.fetchUser())
  }, [])

  const user = useUser()
  const route = useRoute()

  switch (user.status) {
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
