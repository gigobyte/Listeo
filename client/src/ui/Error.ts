import { colors } from './color'
import styled from 'styled-components'

interface ErrorProps {
  visible: boolean
}

export const Error = styled.div<ErrorProps>`
  color: ${colors.crimson100};
  max-height: 0;
  transition: max-height 3000ms, opacity 1000ms;
  opacity: 0;

  ${props =>
    props.visible
      ? `
    opacity: 1;
    max-height: 999px;
    margin-bottom: 10px;
  `
      : ''}
`
