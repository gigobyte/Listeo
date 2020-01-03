import React from 'react'
import { useMount } from 'react-use'
import { createGlobalStyle } from 'styled-components'
import { configureStore } from 'redux-starter-kit'
import { Provider, useDispatch } from 'react-redux'
import { session, useRoute } from './session'
import { RouteTag } from './route'
import { colors } from './ui/color'
import { Header } from './ui/Header'
import MuseoSans from './assets/MuseoSans-100.ttf'
import { Login } from './features/Login'
import { Register } from './features/Register'
import { CreatePlaylist } from './features/playlist/CreatePlaylist'
import { ErrorPage } from './features/ErrorPage'
import { ViewPlaylist } from './features/playlist/ViewPlaylist'

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
  const route = useRoute()

  useMount(() => {
    if (route.tag !== RouteTag.Error) {
      dispatch(session.effects.fetchUser())
    }
  })

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
      return (
        <Layout>
          <ViewPlaylist playlistId={route.params.playlistId} />
        </Layout>
      )

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
