import React from 'react'
import { ModalProps, Modal } from '../../ui/Modal'
import { useInput, Input } from '../../ui/Input'

export const AddVideoModal: React.FC<ModalProps> = ({ onClose }) => {
  const urlInput = useInput({
    validations: [],
    trim: false,
    shouldShowError: _ => false
  })

  return (
    <Modal onClose={onClose}>
      Add Video
      <Input {...urlInput} />
    </Modal>
  )
}
