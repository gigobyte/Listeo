import React, { useState } from 'react'
import styled from 'styled-components'
import { colors } from './color'
import { useUser, useRoute, useSessionContext } from '../session'
import { DataStatus } from '../utils/http'
import { Link } from './Link'
import { routes } from '../route'
import { Icons } from './Icon'
import { Logo } from './Logo'
import { AddPlaylistModal } from '../features/playlist/AddPlaylistModal'

const Container = styled.div`
  display: flex;
  justify-content: space-between;
  background-color: ${colors.white};
`

const Nav = styled.div`
  text-transform: uppercase;
  display: flex;
  align-items: center;
  padding-right: 40px;
  font-size: 1.1rem;
  font-weight: bold;
`

const NavItem = styled.span`
  padding: 0 10px;
  transition: color 500ms;
  &:hover {
    color: ${colors.blue200};
  }
`

const LogoWrapper = styled.div`
  padding: 10px 20px;
  padding-left: 4%;
`

const AddButton = styled(Icons.plusCircle)`
  font-size: 25px;
  padding-right: 10px;
  color: ${colors.crimson100};
`

export const Header: React.FC = () => {
  const user = useUser()
  const route = useRoute()
  const session = useSessionContext()
  const [isAddPlaylistOverlayShown, setIsAddPlaylistOverlayShown] = useState(
    false
  )

  const shouldShowAddPlaylistButton = route !== routes.createPlaylist

  return (
    <>
      <Container>
        <LogoWrapper>
          <Logo />
        </LogoWrapper>
        <Nav>
          {(() => {
            switch (user.status) {
              case DataStatus.Success:
                return (
                  <>
                    {shouldShowAddPlaylistButton && (
                      <AddButton
                        clickable
                        data-test="header--add-playlist"
                        onClick={() => setIsAddPlaylistOverlayShown(true)}
                      />
                    )}
                    <NavItem data-test="header--username">
                      <Link to={routes.home}>{user.data.username}</Link>
                    </NavItem>
                    <NavItem data-test="header--logout">
                      <Link to={routes.home} onClick={() => session.logout()}>
                        Logout
                      </Link>
                    </NavItem>
                  </>
                )

              default:
                return (
                  <>
                    <NavItem data-test="header--login">
                      <Link to={routes.login}>Sign In</Link>
                    </NavItem>
                    <NavItem data-test="header--register">
                      <Link to={routes.register}>Register</Link>
                    </NavItem>
                    <NavItem>
                      <Link to={routes.about}>About</Link>
                    </NavItem>
                  </>
                )
            }
          })()}
        </Nav>
      </Container>
      {isAddPlaylistOverlayShown && (
        <AddPlaylistModal onClose={() => setIsAddPlaylistOverlayShown(false)} />
      )}
    </>
  )
}
