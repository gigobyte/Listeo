import React, { useState } from 'react'
import styled from 'styled-components'
import { useTitle } from 'react-use'
import { centered } from '../ui/Container'
import { DefaultButton } from '../ui/Button'
import { Input, useInput } from '../ui/Input'
import { useForm } from '../ui/Form'
import { routes } from '../route'
import { Link } from '../ui/Link'
import { Error } from '../ui/Error'
import {
  FailedRequest,
  http,
  DataStatus,
  remoteData,
  RemoteData
} from '../utils/http'
import { rule, ifBlank } from '../ui/validate'
import { useSessionContext } from '../session'

enum ValidationError {
  UsernameMissing = 'Please enter username',
  PasswordMissing = 'Please enter password'
}

enum LoginResponseError {
  UserNotFound = 'UserNotFound'
}

interface LoginSuccessResponse {
  jwt: string
}

interface LoginFailResponse extends FailedRequest {
  error: LoginResponseError
}

const LoginForm = styled.form`
  ${centered};
  height: 66%;
`

const Title = styled.h1`
  font-size: 3rem;
`

const SubmitButton = styled(DefaultButton).attrs({ type: 'submit' })`
  margin-top: 10px;
  margin-bottom: 15px;
`
const showLoginResponseError = ({ error }: LoginFailResponse) => {
  switch (error) {
    case LoginResponseError.UserNotFound:
      return 'User not found'
  }
}

export const Login = () => {
  useTitle('Login - Listeo')

  const session = useSessionContext()
  const [loginResponse, setLoginResponse] = useState<
    RemoteData<LoginSuccessResponse, LoginFailResponse>
  >(remoteData.notAsked)

  const loginForm = useForm({
    onSubmit: () => {
      if (usernameInput.isValid && passwordInput.isValid) {
        setLoginResponse(remoteData.loading)
        http
          .post<LoginSuccessResponse>('/login', {
            username: usernameInput.value,
            password: passwordInput.value
          })
          .then(response => {
            session.login(response.jwt)
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
    validations: [rule(ifBlank, ValidationError.UsernameMissing)],
    shouldShowError: _ => loginForm.submitted
  })

  const passwordInput = useInput({
    trim: true,
    validations: [rule(ifBlank, ValidationError.PasswordMissing)],
    shouldShowError: _ => loginForm.submitted
  })

  const loginRequestErrorText = remoteData.showError(
    loginResponse,
    showLoginResponseError
  )

  const isSubmitButtonDisabled =
    loginResponse.status === DataStatus.Loading ||
    usernameInput.isShowingError ||
    passwordInput.isShowingError

  return (
    <LoginForm {...loginForm}>
      <Title>Sign In</Title>
      <Input
        {...usernameInput}
        data-test="login--username"
        placeholder="Username"
      />
      <Input
        {...passwordInput}
        data-test="login--password"
        placeholder="Password"
        type="password"
      />
      <SubmitButton data-test="login--submit" disabled={isSubmitButtonDisabled}>
        Let's go
      </SubmitButton>
      <Error data-test="login--api-error" visible={!!loginRequestErrorText}>
        {loginRequestErrorText}
      </Error>
      <Link to={routes.register}>Don't have an account?</Link>
    </LoginForm>
  )
}
