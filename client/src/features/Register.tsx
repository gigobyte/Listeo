import React, { useState } from 'react'
import styled from 'styled-components'
import {
  FailedRequest,
  http,
  remoteData,
  RemoteData,
  DataStatus
} from '../utils/http'
import { centered } from '../ui/Container'
import { DefaultButton } from '../ui/Button'
import { Error } from '../ui/Error'
import { useTitle } from 'react-use'
import { useInput, Input } from '../ui/Input'
import { useForm } from '../ui/Form'
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

const RegisterForm = styled.form`
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
  const [registerResponse, setRegisterResponse] = useState<
    RemoteData<RegisterSuccessResponse, RegisterFailResponse>
  >(remoteData.notAsked)

  const registerForm = useForm({
    onSubmit: () => {
      if (
        usernameInput.isValid &&
        emailInput.isValid &&
        passwordInput.isValid
      ) {
        setRegisterResponse(remoteData.loading)

        http
          .post<RegisterSuccessResponse>('/register', {
            username: usernameInput.value,
            email: emailInput.value,
            password: passwordInput.value
          })
          .then((response: RegisterSuccessResponse) => {
            session.login(response.jwt)
            setRegisterResponse(remoteData.success(response))
          })
          .catch((response: RegisterFailResponse) => {
            setRegisterResponse(remoteData.fail(response))
          })
      }
    }
  })

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

  const registerRequestErrorText = remoteData.showError(
    registerResponse,
    showRegisterResponseError
  )

  const isSubmitButtonDisabled =
    registerResponse.status === DataStatus.Loading ||
    usernameInput.isShowingError ||
    emailInput.isShowingError ||
    passwordInput.isShowingError

  return (
    <RegisterForm {...registerForm}>
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
