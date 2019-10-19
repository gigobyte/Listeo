import React, { useState } from 'react'
import styled from 'styled-components'
import { colors } from './color'

interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  error: string | undefined
  shouldShowError: boolean
}

interface InputErrorProps {
  visible: boolean
}

const InputWrapper = styled.input<InputProps>`
  border-radius: 2px;
  padding: 7px;
  width: 200px;
  padding-left: 15px;
  font-size: 16px;
  margin-bottom: 10px;
  height: 25px;
  border: 0;
  transition: border-color 1000ms;
  font-family: 'Museo-Sans';
  background-color: ${colors.gray100};
  box-shadow: 1px 2px 3px 0 ${colors.gray300};
  &:focus {
    outline: 0;
    box-shadow: 1px 2px 3px 1px ${colors.gray300};
  }
  ${props => (props.error ? 'margin-bottom: 5px' : '')};
`

const InputError = styled.div<InputErrorProps>`
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

export const Input: React.FC<InputProps> = props => (
  <>
    <InputWrapper {...props} />
    <InputError visible={!!props.error && props.shouldShowError}>
      {props.error}
    </InputError>
  </>
)

interface UseInputParams {
  trim: boolean
  validate: (value: string) => string
  shouldShowError: (value: string) => boolean
}

export const useInput = ({
  validate,
  trim,
  shouldShowError
}: UseInputParams) => {
  const [value, setValue] = useState('')
  const error = validate(value)

  return {
    value,
    shouldShowError: shouldShowError(value),
    error,
    isValid: !error,
    onChange: (e: React.ChangeEvent<HTMLInputElement>) =>
      setValue(trim ? e.target.value.trim() : e.target.value)
  }
}
