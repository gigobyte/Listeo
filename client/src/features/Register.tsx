import React, { useEffect } from 'react'
import styled from 'styled-components'
import {
  FailedRequest,
  http,
  DataStatus,
  showError,
  useCallableAsync,
  PromiseWithError
} from '../utils/http'
import { centered } from '../ui/Container'
import { DefaultButton } from '../ui/Button'
import { Error } from '../ui/Error'
import { useTitle } from 'react-use'
import { useInput, Input } from '../ui/Input'
import { useForm, Form } from '../ui/Form'
import { routes } from '../route'
import { Link } from '../ui/Link'
import {
  ifBlank,
  ifShorterThan,
  rule,
  ifContains,
  ifRegexFails,
  pass,
  ifLongerThan
} from '../ui/validate'
import { useSessionContext } from '../session'

enum ValidationError {
  UsernameMissing = 'Please enter username',
  InvalidUsername = 'Please enter a valid username, the only special characters allowed are - and _',
  EmailMissing = 'Please enter email',
  InvalidEmail = 'Please enter a valid email',
  PasswordMissing = 'Please enter password',
  UsernameTooShort = 'Username must be at least 4 characters long',
  UsernameTooLong = 'Username is too long',
  PasswordTooShort = 'Password must be at least 6 characters long'
}

enum RegisterResponseError {
  UserAlreadyExists = 'UserAlreadyExists'
}

interface RegisterSuccessResponse {
  jwt: string
}

interface RegisterFailResponse extends FailedRequest {
  error: RegisterResponseError
}

const register = (
  username: string,
  email: string,
  password: string
): PromiseWithError<RegisterSuccessResponse, RegisterFailResponse> =>
  http.post('/register', { username, email, password })

const RegisterForm = styled(Form)`
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

const showRegisterResponseError = ({ error }: RegisterFailResponse) => {
  switch (error) {
    case RegisterResponseError.UserAlreadyExists:
      return 'User already exists'
  }
}

export const Register = () => {
  useTitle('Register - Listeo')

  const session = useSessionContext()
  const registerEndpoint = useCallableAsync(register)

  useEffect(() => {
    if (registerEndpoint.response.status === DataStatus.Success) {
      session.login(registerEndpoint.response.data.jwt)
    }
  }, [registerEndpoint.response])

  const registerForm = useForm()

  const usernameInput = useInput({
    trim: true,
    validations: [
      rule(ifBlank, ValidationError.UsernameMissing),
      rule(ifShorterThan(4), ValidationError.UsernameTooShort),
      rule(ifLongerThan(99), ValidationError.UsernameTooLong),
      rule(ifRegexFails(/(^[A-Za-z0-9_-]+)$/), ValidationError.InvalidUsername)
    ],
    shouldShowError: _ => registerForm.submitted
  })

  const emailInput = useInput({
    trim: true,
    validations: [
      rule(ifBlank, ValidationError.EmailMissing),
      rule(pass(ifContains('@')), ValidationError.InvalidEmail)
    ],
    shouldShowError: _ => registerForm.submitted
  })

  const passwordInput = useInput({
    trim: true,
    validations: [
      rule(ifBlank, ValidationError.PasswordMissing),
      rule(ifShorterThan(6), ValidationError.PasswordTooShort)
    ],
    shouldShowError: _ => registerForm.submitted
  })

  const registerRequestErrorText = showError(
    registerEndpoint.response,
    showRegisterResponseError
  )

  const isSubmitButtonDisabled =
    registerEndpoint.response.status === DataStatus.Loading ||
    usernameInput.isShowingError ||
    emailInput.isShowingError ||
    passwordInput.isShowingError

  const submitRegister = () => {
    if (usernameInput.isValid && emailInput.isValid && passwordInput.isValid) {
      registerEndpoint.fetch(
        usernameInput.value,
        emailInput.value,
        passwordInput.value
      )
    }
  }

  return (
    <RegisterForm {...registerForm} onSubmit={submitRegister}>
      <Title>Register</Title>
      <Input
        data-test="register--username"
        {...usernameInput}
        placeholder="Username"
      />
      <Input data-test="register--email" {...emailInput} placeholder="Email" />
      <Input
        data-test="register--password"
        {...passwordInput}
        placeholder="Password"
        type="password"
      />
      <SubmitButton
        data-test="register--submit"
        disabled={isSubmitButtonDisabled}
      >
        Beam me up!
      </SubmitButton>
      <Error
        data-test="register--api-error"
        visible={!!registerRequestErrorText}
      >
        {registerRequestErrorText}
      </Error>
      <Link to={routes.login}>Already have an account?</Link>
    </RegisterForm>
  )
}
