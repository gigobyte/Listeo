import React, { useState } from 'react'
import styled from 'styled-components'
import {
  FailedRequest,
  useHttp,
  remoteData,
  RemoteData,
  DataStatus
} from '../http'
import { createEndpoint } from '../endpoint'
import { centered } from '../ui/Container'
import { Button } from '../ui/Button'
import { Error } from '../ui/Error'
import { useTitle } from 'react-use'
import { useDispatch } from 'react-redux'
import { useInput, Input } from '../ui/Input'
import { useForm } from '../ui/useForm'
import { session } from '../session'
import { routes } from '../route'
import { Link } from '../ui/Link'
import {
  ifBlank,
  ifShorterThan,
  rule,
  fail,
  ifContains,
  ifRegexFails
} from '../ui/validate'

enum ValidationError {
  UsernameMissing = 'Please enter username',
  InvalidUsername = 'Please enter a valid username, the only special characters allowed are - and _',
  EmailMissing = 'Please enter email',
  InvalidEmail = 'Please enter a valid email',
  PasswordMissing = 'Please enter password',
  UsernameTooShort = 'Username must be at least 4 characters long',
  PasswordTooShort = 'Password must be at least 6 characters long',
  None = ''
}

enum RegisterResponseError {
  UserAlreadyExists = 'UserAlreadyExists',
  InvalidRequest = 'InvalidRequest',
  PasswordHashingFailed = 'PasswordHashingFailed',
  ValidationFailed = 'ValidationFailed'
}

interface RegisterSuccessResponse {
  jwt: string
}

interface RegisterFailResponse extends FailedRequest {
  error: RegisterResponseError
}

const registerEndpoint = createEndpoint<RegisterSuccessResponse>('/register')

const RegisterForm = styled.form`
  ${centered};
  height: 66%;
`

const Title = styled.h1`
  font-size: 3rem;
`

const SubmitButton = styled(Button).attrs({ type: 'submit' })`
  margin-top: 10px;
  margin-bottom: 15px;
`

export const Register = () => {
  useTitle('Register - Listeo')

  const dispatch = useDispatch()
  const http = useHttp()
  const [registerResponse, setRegisterResponse] = useState<
    RemoteData<RegisterSuccessResponse, RegisterFailResponse>
  >(remoteData.notAsked)

  const registerForm = useForm({
    onSubmit: () => {
      if (usernameInput.isValid && passwordInput.isValid) {
        setRegisterResponse(remoteData.loading)

        http
          .post(registerEndpoint, {
            username: usernameInput.value,
            email: emailInput.value,
            password: passwordInput.value
          })
          .then((response: RegisterSuccessResponse) => {
            dispatch(session.effects.authSuccess(response.jwt))
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
      rule(ifRegexFails(/([A-Za-z0-9_-]+)/), ValidationError.EmailMissing)
    ],
    shouldShowError: _ => registerForm.submitted
  })

  const emailInput = useInput({
    trim: true,
    validations: [
      rule(ifBlank, ValidationError.EmailMissing),
      rule(fail(ifContains('@')), ValidationError.InvalidEmail)
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

  const registerRequestErrorText = (() => {
    switch (registerResponse.status) {
      case DataStatus.Fail: {
        switch (registerResponse.error) {
          case RegisterResponseError.UserAlreadyExists:
            return 'User already exists'

          default:
            return 'Something went wrong'
        }
      }

      default:
        return ''
    }
  })()

  const isSubmitButtonDisabled =
    registerResponse.status === DataStatus.Loading ||
    !usernameInput.isValid ||
    !passwordInput.isValid

  return (
    <RegisterForm {...registerForm}>
      <Title>Register</Title>
      <Input {...usernameInput} placeholder="Username" />
      <Input {...emailInput} placeholder="Email" />
      <Input {...passwordInput} placeholder="Password" type="password" />
      <SubmitButton disabled={isSubmitButtonDisabled}>Beam me up!</SubmitButton>
      <Error visible={!!registerRequestErrorText}>
        {registerRequestErrorText}
      </Error>
      <Link to={routes.login}>Already have an account?</Link>
    </RegisterForm>
  )
}
