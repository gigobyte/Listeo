import { colors } from './color'
import styled from 'styled-components'

interface ErrorProps {
  visible: boolean
}

export const Error = styled.div<ErrorProps>`
  color: ${colors.crimson100};
  max-height: 0;
  max-width: 0;
  transition: max-height 3000ms, opacity 1000ms;
  opacity: 0;
  pointer-events: none;

  ${props =>
    props.visible
      ? `
    opacity: 1;
    max-height: 999px;
    max-width: 100%;
    margin-bottom: 10px;
    pointer-events: initial;
  `
      : ''}
`
