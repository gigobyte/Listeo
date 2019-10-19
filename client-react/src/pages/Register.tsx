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

enum ValidationError {
  UsernameMissing = 'Please enter username',
  PasswordMissing = 'Please enter password',
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

const RegisterForm = styled(centered(styled.form))`
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
    validate: value =>
      !value ? ValidationError.UsernameMissing : ValidationError.None,
    shouldShowError: _ => registerForm.submitted
  })

  const passwordInput = useInput({
    trim: true,
    validate: value =>
      !value ? ValidationError.PasswordMissing : ValidationError.None,
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
      <Input {...passwordInput} placeholder="Password" type="password" />
      <SubmitButton disabled={isSubmitButtonDisabled}>Beam me up!</SubmitButton>
      <Error visible={!!registerRequestErrorText}>
        {registerRequestErrorText}
      </Error>
      <Link to={routes.login}>Already have an account?</Link>
    </RegisterForm>
  )
}
