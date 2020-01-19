import React from 'react'
import { ModalProps, Modal } from '../../ui/Modal'
import { useInput, Input } from '../../ui/Input'
import { DefaultButton } from '../../ui/Button'
import { http } from '../../http'
import { createEndpoint } from '../../endpoint'
import { useForm } from '../../ui/useForm'

interface AddVideoModalProps extends ModalProps {
  playlistId: string
  onRefetchPlaylist: () => void
}

const addVideo = (
  playlistId: string,
  request: { url: string }
): Promise<void> =>
  http.post(createEndpoint<void>('/playlist/' + playlistId + '/video'), request)

export const AddVideoModal: React.FC<AddVideoModalProps> = ({
  onClose,
  onRefetchPlaylist,
  playlistId
}) => {
  const videoForm = useForm({
    onSubmit: () => {
      addVideo(playlistId, { url: urlInput.value }).then(() => {
        onClose()
        onRefetchPlaylist()
      })
    }
  })

  const urlInput = useInput({
    validations: [],
    trim: false,
    shouldShowError: _ => false
  })

  return (
    <Modal onClose={onClose}>
      <form {...videoForm}>
        Add Video
        <Input {...urlInput} placeholder="Url" />
        <DefaultButton type="submit">Add</DefaultButton>
      </form>
    </Modal>
  )
}
