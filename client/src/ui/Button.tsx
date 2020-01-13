import React from 'react'
import styled from 'styled-components'
import { colors } from './color'

export enum ButtonStyle {
  Default = 'Default',
  Primary = 'Primary'
}

export const buttonStyle = {
  Default: `${colors.blue400}, ${colors.blue300}`,
  Primary: `${colors.crimson100}, ${colors.crimson200}`
}

interface ButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
  icon: React.ReactNode | null
  styling: ButtonStyle
}

const ButtonWrapper = styled.button<{ styling: string }>`
  border-radius: 44px;
  border: 0;
  background-image: linear-gradient(310deg, ${props => props.styling});
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

export const Button: React.FC<ButtonProps> = ({
  children,
  icon,
  styling,
  ...props
}) => (
  <ButtonWrapper {...props} styling={buttonStyle[styling]}>
    {icon} {children}
  </ButtonWrapper>
)

export const DefaultButton: React.FC<React.ButtonHTMLAttributes<
  HTMLButtonElement
>> = props => <Button {...props} styling={ButtonStyle.Default} icon={null} />
