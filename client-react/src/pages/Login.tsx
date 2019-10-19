import React, { useState } from 'react'
import { useTitle } from 'react-use'
import { centered } from '../ui/Container'
import styled from 'styled-components'
import { Button } from '../ui/Button'
import { Input, useInput } from '../ui/Input'
import { useForm } from '../ui/useForm'
import { routes } from '../route'
import { Link } from '../ui/Link'
import { createEndpoint } from '../endpoint'
import { FailedRequest, useHttp } from '../http'
import { useDispatch } from 'react-redux'
import { session } from '../session'

const LoginForm = styled(centered(styled.form))`
  height: 66%;
`

const Title = styled.h1`
  font-size: 3rem;
`

const SubmitButton = styled(Button).attrs({ type: 'submit' })`
  margin-top: 10px;
  margin-bottom: 15px;
`

enum ValidationError {
  UsernameMissing = 'Please enter username',
  PasswordMissing = 'Please enter password',
  None = ''
}

enum LoginResponseError {
  UserNotFound = 'UserNotFound',
  InvalidRequest = 'InvalidRequest',
  ServerError = 'ServerError'
}

interface LoginSuccessResponse {
  jwt: string
}

interface LoginFailResponse extends FailedRequest {
  error: LoginResponseError
}

const loginEndpoint = createEndpoint<LoginSuccessResponse>('/login')

export const Login = () => {
  useTitle('Login - Listeo')

  const dispatch = useDispatch()
  const http = useHttp()
  const [loginError, setLoginError] = useState()

  const loginForm = useForm({
    onSubmit: () => {
      if (usernameInput.isValid && passwordInput.isValid) {
        http
          .post(loginEndpoint, {
            username: usernameInput.value,
            password: passwordInput.value
          })
          .then(({ jwt }: LoginSuccessResponse) => {
            dispatch(session.effects.loginSuccess(jwt))
          })
          .catch(({ error }: LoginFailResponse) => {
            setLoginError(error)
          })
      }
    }
  })

  const usernameInput = useInput({
    trim: true,
    validate: value =>
      !value ? ValidationError.UsernameMissing : ValidationError.None,
    shouldShowError: _ => loginForm.submitted
  })

  const passwordInput = useInput({
    trim: true,
    validate: value =>
      !value ? ValidationError.PasswordMissing : ValidationError.None,
    shouldShowError: _ => loginForm.submitted
  })

  return (
    <LoginForm {...loginForm}>
      <Title>Sign In</Title>
      <Input {...usernameInput} placeholder="Username" />
      <Input {...passwordInput} placeholder="Password" type="password" />
      <SubmitButton>Let's go</SubmitButton>
      {loginError &&
        (() => {
          switch (loginError) {
            case LoginResponseError.UserNotFound:
              return 'User not found'

            default:
              return 'Something went wrong'
          }
        })()}
      <Link to={routes.register}>Don't have an account?</Link>
    </LoginForm>
  )
}
