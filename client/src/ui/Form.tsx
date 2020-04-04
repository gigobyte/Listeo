import React, { useState } from 'react'

interface FormProps extends React.FormHTMLAttributes<HTMLFormElement> {
  onSubmitCustom: (
    cb?: (e: React.FormEvent<HTMLFormElement>) => void
  ) => (e: React.FormEvent<HTMLFormElement>) => void
}

export const Form: React.FC<FormProps> = ({
  onSubmitCustom,
  ...otherProps
}) => <form {...otherProps} onSubmit={onSubmitCustom(otherProps.onSubmit)} />

export const useForm = () => {
  const [submitted, setSubmitted] = useState(false)

  const onSubmit = (cb?: (e: React.FormEvent<HTMLFormElement>) => void) => (
    e: React.FormEvent<HTMLFormElement>
  ) => {
    e.preventDefault()

    if (cb) {
      cb(e)
    }

    setSubmitted(true)
  }

  return { onSubmitCustom: onSubmit, submitted }
}
