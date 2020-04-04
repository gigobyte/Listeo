import React, { useEffect } from 'react'
import styled from 'styled-components'
import { useTitle } from 'react-use'
import { centered } from '../ui/Container'
import { DefaultButton } from '../ui/Button'
import { Input, useInput } from '../ui/Input'
import { useForm, Form } from '../ui/Form'
import { routes } from '../route'
import { Link } from '../ui/Link'
import { Error } from '../ui/Error'
import {
  FailedRequest,
  http,
  showError,
  useCallableAsync,
  PromiseWithError,
  isSuccess,
  isLoading
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

const loginEndpoint = (
  username: string,
  password: string
): PromiseWithError<LoginSuccessResponse, LoginFailResponse> =>
  http.post('/login', { username, password })

const LoginForm = styled(Form)`
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
  const [loginResponse, login] = useCallableAsync(loginEndpoint)

  useEffect(() => {
    if (isSuccess(loginResponse)) {
      session.login(loginResponse.data.jwt)
    }
  }, [loginResponse])

  const loginForm = useForm()

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

  const loginRequestErrorText = showError(loginResponse, showLoginResponseError)

  const isSubmitButtonDisabled =
    isLoading(loginResponse) ||
    usernameInput.isShowingError ||
    passwordInput.isShowingError

  const submitLogin = () => {
    if (usernameInput.isValid && passwordInput.isValid) {
      login(usernameInput.value, passwordInput.value)
    }
  }

  return (
    <LoginForm {...loginForm} onSubmit={submitLogin}>
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
