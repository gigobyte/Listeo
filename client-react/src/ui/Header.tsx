import React from 'react'
import styled from 'styled-components'
import { colors } from './color'
import { useUser } from '../session'
import { DataStatus } from '../http'
import { Link } from './Link'
import { routes } from '../route'
import { Icons } from './Icon'
import { Logo } from './Logo'

const Container = styled.div`
  display: flex;
  justify-content: space-between;
  background-color: ${colors.white};
`

const Nav = styled.div`
  text-transform: uppercase;
  display: flex;
  align-items: center;
  padding-right: 4%;
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
  cursor: pointer;
`

export const Header: React.FC = () => {
  const user = useUser()

  return (
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
                  <AddButton />
                  <NavItem>
                    <Link to={routes.home}>{user.username}</Link>
                  </NavItem>
                  <NavItem>
                    <Link to={routes.home}>Logout</Link>
                  </NavItem>
                </>
              )

            default:
              return (
                <>
                  <NavItem>
                    <Link to={routes.login}>Sign In</Link>
                  </NavItem>
                  <NavItem>
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
  )
}
