import React from 'react'
import styled from 'styled-components'
import { colors } from './color'
import { Link } from './Link'
import { routes } from '../route'

const LogoWrapper = styled.span`
  font-family: 'Candara Regular';
  color: ${colors.blue200};
  font-weight: bold;
  font-size: 3rem;
`

export const Logo: React.FC = () => (
  <Link to={routes.home}>
    <LogoWrapper>Listeo</LogoWrapper>
  </Link>
)
