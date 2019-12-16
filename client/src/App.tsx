import React from 'react'
import { useMount } from 'react-use'
import styled, { createGlobalStyle } from 'styled-components'
import { configureStore } from 'redux-starter-kit'
import { Provider, useDispatch } from 'react-redux'
import { session, useUser, useRoute } from './session'
import { routes, RouteTag } from './route'
import { colors } from './ui/color'
import { DataStatus } from './http'
import { Header } from './ui/Header'
import MuseoSans from './assets/MuseoSans-100.ttf'
import { Login } from './pages/Login'
import { Register } from './pages/Register'
import { CreatePlaylist } from './pages/playlist/CreatePlaylist'
import { ErrorPage } from './pages/ErrorPage'

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

  if (
    user.status === DataStatus.NotAsked ||
    user.status === DataStatus.Loading
  ) {
    return null
  }

  switch (route.tag) {
    case RouteTag.Home:
      return <Layout></Layout>

    case RouteTag.Login:
      return (
        <Layout>
          <Login />
        </Layout>
      )

    case RouteTag.Register:
      return (
        <Layout>
          <Register />
        </Layout>
      )

    case RouteTag.About:
      return <Layout></Layout>

    case RouteTag.CreatePlaylist:
      return (
        <Layout>
          <CreatePlaylist />
        </Layout>
      )

    case RouteTag.ViewPlaylist:
      return <Layout></Layout>

    case RouteTag.NotFound404:
      return <h1>404</h1>

    case RouteTag.Error:
      return (
        <Layout>
          <ErrorPage />
        </Layout>
      )
  }
}
