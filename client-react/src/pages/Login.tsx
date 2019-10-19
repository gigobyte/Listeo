import React, { useState } from 'react'
import styled from 'styled-components'
import { useTitle } from 'react-use'
import { centered } from '../ui/Container'
import { Button } from '../ui/Button'
import { Input, useInput } from '../ui/Input'
import { useForm } from '../ui/useForm'
import { routes } from '../route'
import { Link } from '../ui/Link'
import { Error } from '../ui/Error'
import { createEndpoint } from '../endpoint'
import {
  FailedRequest,
  useHttp,
  DataStatus,
  remoteData,
  RemoteData
} from '../http'
import { useDispatch } from 'react-redux'
import { session } from '../session'

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

export const Login = () => {
  useTitle('Login - Listeo')

  const dispatch = useDispatch()
  const http = useHttp()
  const [loginResponse, setLoginResponse] = useState<
    RemoteData<LoginSuccessResponse, LoginFailResponse>
  >(remoteData.notAsked)

  const loginForm = useForm({
    onSubmit: () => {
      if (usernameInput.isValid && passwordInput.isValid) {
        http
          .post(loginEndpoint, {
            username: usernameInput.value,
            password: passwordInput.value
          })
          .then((response: LoginSuccessResponse) => {
            dispatch(session.effects.authSuccess(response.jwt))
            setLoginResponse(remoteData.success(response))
          })
          .catch((response: LoginFailResponse) => {
            setLoginResponse(remoteData.fail(response))
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

  const loginRequestErrorText = (() => {
    switch (loginResponse.status) {
      case DataStatus.Fail: {
        switch (loginResponse.error) {
          case LoginResponseError.UserNotFound:
            return 'User not found'

          default:
            return 'Something went wrong'
        }
      }

      default:
        return ''
    }
  })()

  const isSubmitButtonDisabled =
    loginResponse.status === DataStatus.Loading ||
    !usernameInput.isValid ||
    !passwordInput.isValid

  return (
    <LoginForm {...loginForm}>
      <Title>Sign In</Title>
      <Input {...usernameInput} placeholder="Username" />
      <Input {...passwordInput} placeholder="Password" type="password" />
      <SubmitButton disabled={isSubmitButtonDisabled}>Let's go</SubmitButton>
      <Error visible={!!loginRequestErrorText}>{loginRequestErrorText}</Error>
      <Link to={routes.register}>Don't have an account?</Link>
    </LoginForm>
  )
}
