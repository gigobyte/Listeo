import React, { useState } from 'react'
import styled from 'styled-components'
import { colors } from './color'
import { Error } from './Error'
import { validate } from './validate'

interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  error: string | undefined
  shouldShowError: boolean
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

export const Input: React.FC<InputProps> = props => (
  <>
    <InputWrapper {...props} />
    <Error visible={!!props.error && props.shouldShowError}>
      {props.error}
    </Error>
  </>
)

interface UseInputParams {
  trim: boolean
  validations: [(value: string) => boolean, string][]
  shouldShowError: (value: string) => boolean
}

export const useInput = ({
  validations,
  trim,
  shouldShowError
}: UseInputParams) => {
  const [value, setValue] = useState('')
  const error = validate(validations, value) || ''

  return {
    value,
    shouldShowError: shouldShowError(value),
    error,
    isValid: !error,
    onChange: (e: React.ChangeEvent<HTMLInputElement>) =>
      setValue(trim ? e.target.value.trim() : e.target.value)
  }
}
