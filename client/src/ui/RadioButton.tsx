import React, { useState } from 'react'
import styled from 'styled-components'
import { colors } from './color'

interface RadioButtonProps extends React.InputHTMLAttributes<HTMLInputElement> {
  label: string
}

const Input = styled.input`
  display: none;
`

const Label = styled.label`
  position: relative;
  display: inline-block;
  padding-left: 25px;
  height: 22px;

  input:checked + span:after {
    content: '';
    height: 10px;
    width: 10px;
    background-color: ${colors.blue100};
    position: absolute;
    border-radius: 50%;
    left: 50%;
    top: 50%;
    transform: translate(-50%, -50%);
  }
`

const Circle = styled.span`
  display: inline-block;
  width: 18px;
  height: 18px;
  position: absolute;
  left: 0;
  top: -3px;
  border-radius: 50%;
  border: 2px solid ${colors.blue100};
`

export const RadioButton: React.FC<RadioButtonProps> = ({
  label,
  checked,
  onClick
}) => (
  <Label>
    {label}
    <Input type="radio" checked={checked} onClick={onClick} />
    <Circle />
  </Label>
)

interface UseRadioButtonsParams<T> {
  initialValue: T
  values: T[]
}

export const useRadioButtons = <T extends string | number>({
  initialValue,
  values
}: UseRadioButtonsParams<T>) => {
  const [value, setValue] = useState(initialValue)
  const [checkedValues, setCheckedValue] = useState({
    [values.findIndex(x => x === initialValue)]: true
  })

  return {
    value,
    radioButtons: values.map((value, i) => ({
      checked: !!checkedValues[i],
      onClick: () => {
        setCheckedValue({ [i]: true })
        setValue(value)
      }
    }))
  }
}
