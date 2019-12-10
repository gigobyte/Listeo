import React from 'react'
import { useMount } from 'react-use'
import styled, { createGlobalStyle } from 'styled-components'
import { configureStore } from 'redux-starter-kit'
import { Provider, useDispatch } from 'react-redux'
import { session, useUser, useRoute } from './session'
import { routes } from './route'
import { colors } from './ui/color'
import { DataStatus } from './http'
import { Header } from './ui/Header'
import MuseoSans from './assets/MuseoSans-100.ttf'
import { Login } from './pages/Login'
import { Register } from './pages/Register'
import { CreatePlaylist } from './pages/playlist/CreatePlaylist'

const store = configureStore({
  reducer: session.reducer
})

export type AppDispatch = typeof store.dispatch

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

  main {
    height: 100%;
    display: flex;
    flex-direction: column;
  }
`

const Layout: React.FC = ({ children }) => (
  <>
    <GlobalStyle />
    <Header />
    {children}
  </>
)

export const Main = () => {
  const dispatch = useDispatch()

  useMount(() => {
    dispatch(session.effects.fetchUser())
  })

  const user = useUser()
  const route = useRoute()

  switch (user.status) {
    case DataStatus.NotAsked:
    case DataStatus.Loading:
      return null

    default: {
      switch (route) {
        case routes.home:
          return <Layout></Layout>

        case routes.login:
          return (
            <Layout>
              <Login />
            </Layout>
          )

        case routes.register:
          return (
            <Layout>
              <Register />
            </Layout>
          )

        case routes.createPlaylist:
          return (
            <Layout>
              <CreatePlaylist />
            </Layout>
          )

        case routes.notFound404:
        default:
          return null
      }
    }
  }
}
