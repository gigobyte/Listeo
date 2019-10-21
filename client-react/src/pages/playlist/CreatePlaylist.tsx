import React from 'react'
import styled from 'styled-components'
import { centered } from '../../ui/Container'
import { useTitle } from 'react-use'
import { useInput, Input } from '../../ui/Input'
import { ifBlank, rule } from '../../ui/validate'
import { useForm } from '../../ui/useForm'

enum ValidationError {
  PlaylistNameMissing = 'PlaylistNameMissing'
}

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

  const createPlaylistForm = useForm({
    onSubmit: () => {}
  })

  const playlistNameInput = useInput({
    trim: false,
    validations: [rule(ifBlank, ValidationError.PlaylistNameMissing)],
    shouldShowError: _ => createPlaylistForm.submitted
  })

  return (
    <Container>
      <Title>Create a new playlist</Title>
      <Input {...playlistNameInput} placeholder="Name of list" />
    </Container>
  )
}
