import React from 'react'
import styled from 'styled-components'
import { centered } from '../../ui/Container'
import { useTitle } from 'react-use'
import { useInput, Input } from '../../ui/Input'
import { ifBlank, rule } from '../../ui/validate'
import { useForm } from '../../ui/useForm'
import { TagInput, useTagInput } from '../../ui/TagInput'
import { Textarea, useTextarea } from '../../ui/Textarea'
import { RadioButton, useRadioButtons } from '../../ui/RadioButton'
import { Button } from '../../ui/Button'
import { useHttp, FailedRequest } from '../../http'
import { createEndpoint } from '../../endpoint'
import { session } from '../../session'
import { useDispatch } from 'react-redux'
import { routes } from '../../route'

enum ValidationError {
  PlaylistNameMissing = 'Please enter the name of the playlist'
}

enum PlaylistPrivacy {
  Public = 'Public',
  Private = 'Private'
}

enum PlaylistStyle {
  Ranked = 'Ranked',
  Unordered = 'Unordered'
}

enum CreatePlaylistResponseError {
  InvalidRequest = 'InvalidRequest',
  ValidationFailed = 'ValidationFailed'
}

interface CreatePlaylistSuccessResponse {
  playlistId: string
}

interface CreatePlaylistFailResponse extends FailedRequest {
  error: CreatePlaylistResponseError
}

const createPlaylistEndpoint = createEndpoint<CreatePlaylistSuccessResponse>(
  '/playlist'
)

const Container = styled(centered(styled.div))`
  height: 66%;
`

const Title = styled.h1`
  font-size: 2rem;
`

const SettingsColumn = styled.div`
  display: flex;
  flex-direction: column;
  padding-right: 15px;
  &:last-child {
    padding-right: 0;
  }
`

const SettingLabel = styled.span`
  font-weight: bold;
`

const Settings = styled.div`
  display: flex;
  padding: 10px 0;
`

const Separator = styled.div`
  height: 5px;
`

export const CreatePlaylist = () => {
  useTitle('Create Playlist - Listeo')

  const http = useHttp()
  const dispatch = useDispatch()

  const createPlaylistForm = useForm({
    onSubmit: () => {
      if (playlistNameInput.isValid) {
        http
          .post(createPlaylistEndpoint, {
            name: playlistNameInput.value,
            description: descriptionInput.value,
            tags: tagsInput.tags,
            privacy: playlistPrivacy.value,
            style: playlistStyle.value
          })
          .then(response => {
            dispatch(
              session.effects.redirect(routes.viewPlaylist(response.playlistId))
            )
          })
      }
    }
  })

  const playlistNameInput = useInput({
    trim: false,
    validations: [rule(ifBlank, ValidationError.PlaylistNameMissing)],
    shouldShowError: _ => createPlaylistForm.submitted
  })

  const tagsInput = useTagInput()

  const descriptionInput = useTextarea({
    trim: false,
    validations: [],
    shouldShowError: _ => false
  })

  const playlistPrivacy = useRadioButtons({
    initialValue: PlaylistPrivacy.Public,
    values: [PlaylistPrivacy.Public, PlaylistPrivacy.Private]
  })

  const [publicRadioButton, privateRadioButton] = playlistPrivacy.radioButtons

  const playlistStyle = useRadioButtons({
    initialValue: PlaylistStyle.Unordered,
    values: [PlaylistStyle.Ranked, PlaylistStyle.Unordered]
  })

  const [rankedRadioButton, unorderedRadioButton] = playlistStyle.radioButtons

  return (
    <Container>
      <Title>Create a new playlist</Title>
      <Input {...playlistNameInput} placeholder="Name of list" />
      <TagInput {...tagsInput} placeholder="Tags (optional)" />
      <Textarea {...descriptionInput} placeholder="Description (optional)" />
      <Settings>
        <SettingsColumn>
          <SettingLabel>Privacy</SettingLabel>
          <Separator />
          <SettingLabel>Style</SettingLabel>
        </SettingsColumn>
        <SettingsColumn>
          <RadioButton {...publicRadioButton} label="Public" />
          <RadioButton {...rankedRadioButton} label="Ranked" />
        </SettingsColumn>
        <SettingsColumn>
          <RadioButton {...privateRadioButton} label="Private" />
          <RadioButton {...unorderedRadioButton} label="Unordered" />
        </SettingsColumn>
      </Settings>
      <Button onClick={createPlaylistForm.onSubmit}>Create</Button>
    </Container>
  )
}
