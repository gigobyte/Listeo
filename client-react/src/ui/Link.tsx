import React, { useCallback } from 'react'
import { Route } from '../route'
import styled from 'styled-components'
import { colors } from './color'
import { useDispatch } from 'react-redux'
import { session } from '../session'

interface LinkProps {
  to: Route
}

const LinkWrapper = styled.a`
  text-decoration: none;
  color: ${colors.blue200};
`

export const Link: React.FC<LinkProps> = ({ to, children }) => {
  const dispatch = useDispatch()
  const handleClick = useCallback(
    (e: React.MouseEvent<HTMLAnchorElement>) => {
      e.preventDefault()
      dispatch(session.effects.redirect(to))
    },
    [to]
  )

  return (
    <LinkWrapper onClick={handleClick} href={to}>
      {children}
    </LinkWrapper>
  )
}
