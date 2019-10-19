import { useState } from 'react'

export const useForm = (options: { onSubmit: () => void }) => {
  const [submitted, setSubmitted] = useState(false)

  const onSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    options.onSubmit()
    setSubmitted(true)
  }

  return { onSubmit, submitted }
}
