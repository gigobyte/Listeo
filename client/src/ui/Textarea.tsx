import React, { useState } from 'react'
import styled from 'styled-components'
import { colors } from './color'
import { Error } from './Error'
import { validate } from './validate'

interface TextareaProps
  extends React.TextareaHTMLAttributes<HTMLTextAreaElement> {
  error: string | undefined
  shouldShowError: boolean
}

const TextareaWrapper = styled.textarea<TextareaProps>`
  border-radius: 2px;
  padding: 7px;
  width: 200px;
  padding-left: 15px;
  font-size: 16px;
  margin-bottom: 10px;
  height: 25px;
  min-height: 75px;
  max-height: 150px;
  border: 0;
  transition: border-color 1000ms;
  font-family: 'Museo-Sans';
  background-color: ${colors.gray100};
  box-shadow: 1px 2px 3px 0 ${colors.gray300};
  &:focus {
    outline: 0;
    box-shadow: 1px 2px 3px 1px ${colors.gray300};
  }
  ${props =>
    props.error
      ? `
    margin-bottom: 5px;
    border-color: ${colors.crimson100};
    `
      : ''};
`

export const Textarea: React.FC<TextareaProps> = props => (
  <>
    <TextareaWrapper {...props} />
    <Error visible={!!props.error && props.shouldShowError}>
      {props.error}
    </Error>
  </>
)

interface UseTextareaParams {
  trim: boolean
  validations: [(value: string) => boolean, string][]
  shouldShowError: (value: string) => boolean
}

export const useTextarea = ({
  validations,
  trim,
  shouldShowError
}: UseTextareaParams) => {
  const [value, setValue] = useState('')
  const error = validate(validations, value) || ''

  return {
    value,
    shouldShowError: shouldShowError(value),
    error,
    isValid: !error,
    setValue: (newValue: string) => setValue(trim ? newValue.trim() : newValue),
    onChange: (e: React.ChangeEvent<HTMLTextAreaElement>) =>
      setValue(trim ? e.target.value.trim() : e.target.value)
  }
}
