import React, { useCallback } from 'react'
import { Route, routeToString } from '../route'
import styled from 'styled-components'
import { colors } from './color'
import { redirect } from '../session'

interface LinkProps extends React.AnchorHTMLAttributes<HTMLAnchorElement> {
  to: Route
}

const LinkWrapper = styled.a`
  text-decoration: none;
  color: ${colors.blue200};
`

export const Link: React.FC<LinkProps> = ({ to, onClick, children }) => {
  const handleClick = useCallback(
    (e: React.MouseEvent<HTMLAnchorElement, MouseEvent>) => {
      e.preventDefault()

      if (onClick) {
        onClick(e)
      }

      redirect(to)
    },
    [to]
  )

  return (
    <LinkWrapper onClick={handleClick} href={routeToString(to)}>
      {children}
    </LinkWrapper>
  )
}
