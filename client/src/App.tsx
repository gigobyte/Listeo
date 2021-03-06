import React from 'react'
import styled, { createGlobalStyle } from 'styled-components'
import { useRoute, useUser, SessionProvider } from './session'
import { RouteTag } from './route'
import { colors } from './ui/color'
import { Header } from './ui/Header'
import MuseoSans from './assets/MuseoSans-100.ttf'
import MuseoSansCyrillic from './assets/MuseoSansCyrl-100.ttf'
import { Login } from './features/Login'
import { Register } from './features/Register'
import { CreatePlaylist } from './features/CreatePlaylist'
import { ErrorPage } from './features/ErrorPage'
import { ViewPlaylist } from './features/ViewPlaylist'
import { DataStatus } from './utils/http'
import { Spinner } from './ui/Spinner'

export const App = () => {
  return (
    <SessionProvider>
      <Main />
    </SessionProvider>
  )
}

const GlobalStyle = createGlobalStyle`
  @font-face {
    font-family: Museo-Sans;
    src: url(${MuseoSans});
  }

  @font-face {
    font-family: Museo-SansCyrillic;
    src: url(${MuseoSansCyrillic});
  }

  html, body {
    height: 100%;
    margin: 0;
    font-family: 'Museo-Sans', 'Museo-SansCyrillic';
    background-color: ${colors.gray200};
  }

  main {
    height: 100%;
    display: flex;
    flex-direction: column;
  }
`

const Content = styled.div`
  padding: 50px;
`

const Layout: React.FC = ({ children }) => (
  <>
    <GlobalStyle />
    <Header />
    <Content>{children}</Content>
  </>
)

export const Main = () => {
  const route = useRoute()
  const user = useUser()

  if (route.tag === RouteTag.Error) {
    return (
      <Layout>
        <ErrorPage />
      </Layout>
    )
  }

  switch (user.status) {
    case DataStatus.Fail:
    case DataStatus.Success: {
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
      }
    }

    case DataStatus.NotAsked:
    case DataStatus.Loading:
      return (
        <Layout>
          <Spinner />
        </Layout>
      )
  }
}
