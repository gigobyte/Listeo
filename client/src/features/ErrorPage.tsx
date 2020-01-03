import React from 'react'
import { centered } from '../ui/Container'
import styled from 'styled-components'

const ErrorMessage = styled.h1`
  ${centered};
  height: 60%;
`

export const ErrorPage = () => (
  <ErrorMessage>Ooops, something went wrong!</ErrorMessage>
)
