import React from 'react'
import styled from 'styled-components'
import { colors } from '../../ui/color'
import { Icons } from '../../ui/Icon'
import { DefaultButton } from '../../ui/Button'
import { redirect } from '../../session'
import { routes } from '../../route'
import { Modal, ModalProps } from '../../ui/Modal'

const Container = styled.div`
  height: 100%;
  display: flex;
  flex-direction: column;
`

const OptionsContainer = styled.div`
  display: flex;
  padding-top: 5%;
  height: 100%;
  justify-content: center;
`

const Header = styled.div`
  text-align: center;
`

const Title = styled.span`
  text-transform: uppercase;
`

const Subtitle = styled.span`
  font-size: 1.4rem;
`

const OptionCard = styled.div`
  flex-basis: 25%;
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 0 10px;
  &:hover i {
    transform: scale(1.1);
  }
`

const optionIconStyle = `
  text-align: center;
  font-size: 5rem;
  color: ${colors.blue200};
  transition: transform 300ms;
`

const NewPlaylistIcon = styled(Icons.folderPlus)`
  ${optionIconStyle}
`

const ImportPlaylistIcon = styled(Icons.cloudDownload)`
  ${optionIconStyle}
`

const OptionDescription = styled.div`
  margin-top: 20px;
  margin-bottom: 25px;
  text-align: center;
`

export const AddPlaylistModal: React.FC<ModalProps> = ({ onClose }) => {
  return (
    <Modal onClose={onClose}>
      <Container>
        <Header>
          <div>
            <Title>New playlist</Title>
          </div>
          <div>
            <Subtitle>Choose a starting point</Subtitle>
          </div>
        </Header>
        <OptionsContainer>
          <OptionCard>
            <NewPlaylistIcon />
            <OptionDescription>Start with an empty playlist.</OptionDescription>
            <DefaultButton
              data-test="add-playlist--create"
              onClick={() => {
                onClose()
                redirect(routes.createPlaylist)
              }}
            >
              Create new
            </DefaultButton>
          </OptionCard>
          <OptionCard>
            <ImportPlaylistIcon />
            <OptionDescription>
              Import your existing playlist.
            </OptionDescription>
            <DefaultButton disabled>Import</DefaultButton>
          </OptionCard>
        </OptionsContainer>
      </Container>
    </Modal>
  )
}
