import styled from 'styled-components'
import { colors } from './color'

export const Button = styled.button`
  border-radius: 44px;
  border: 0;
  background-image: linear-gradient(
    310deg,
    ${colors.blue400},
    ${colors.blue300}
  );
  transition: transform 300ms;
  color: ${colors.white};
  padding: 10px 20px;
  font-weight: bold;
  font-size: 1rem;
  font-family: 'Museo-Sans';
  cursor: pointer;
  &:focus {
    outline: none;
  }
  &:active {
    transform: scale(0.95);
  }
  &:disabled {
    cursor: not-allowed;
    opacity: 0.7;
  }
`
