import React from 'react'
import styled from 'styled-components'
import { ModalProps, Modal } from '../../ui/Modal'
import { useInput, Input } from '../../ui/Input'
import { DefaultButton } from '../../ui/Button'
import { http } from '../../infrastructure/http'
import { createEndpoint } from '../../infrastructure/endpoint'
import { useForm } from '../../ui/useForm'
import { useTagInput, TagInput } from '../../ui/TagInput'
import { useTextarea, Textarea } from '../../ui/Textarea'
import { centered } from '../../ui/Container'
import { Playlist } from '../playlist/Playlist'
import { Id } from '../../infrastructure/id'

interface AddVideoRequest {
  url: string
  note: string
  tags: string[]
}

interface AddVideoModalProps extends ModalProps {
  playlistId: Id<Playlist>
  onRefetchPlaylist: () => void
}

const Title = styled.h1`
  font-size: 2rem;
`

const AddVideoForm = styled.form`
  ${centered};
`

const addVideo = (
  playlistId: Id<Playlist>,
  request: AddVideoRequest
): Promise<void> =>
  http.post(createEndpoint<void>('/playlist/' + playlistId + '/video'), request)

export const AddVideoModal: React.FC<AddVideoModalProps> = ({
  onClose,
  onRefetchPlaylist,
  playlistId
}) => {
  const videoForm = useForm({
    onSubmit: () => {
      addVideo(playlistId, {
        url: urlInput.value,
        note: noteInput.value,
        tags: tagsInput.tags
      }).then(() => {
        onClose()
        onRefetchPlaylist()
      })
    }
  })

  const tagsInput = useTagInput()

  const urlInput = useInput({
    validations: [],
    trim: false,
    shouldShowError: _ => false
  })

  const noteInput = useTextarea({
    trim: false,
    validations: [],
    shouldShowError: _ => false
  })

  return (
    <Modal onClose={onClose}>
      <AddVideoForm {...videoForm}>
        <Title>Add Video</Title>
        <Input {...urlInput} placeholder="Url" />
        <Textarea {...noteInput} placeholder="Note (optional)" />
        <TagInput {...tagsInput} placeholder="Tags (optional)" />
        <DefaultButton type="submit">Add</DefaultButton>
      </AddVideoForm>
    </Modal>
  )
}
