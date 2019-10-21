import React from 'react'
import styled from 'styled-components'
import { fullHeight } from '../../ui/Container'
import { colors } from '../../ui/color'
import { Icons } from '../../ui/Icon'
import { Button } from '../../ui/Button'
import { useDispatch } from 'react-redux'
import { session } from '../../session'
import { routes } from '../../route'
import { Modal } from '../../ui/Modal'

const Container = styled(fullHeight(styled.div))`
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

interface AddPlaylistModalProps {
  onClose: () => void
}

export const AddPlaylistModal: React.FC<AddPlaylistModalProps> = ({
  onClose
}) => {
  const dispatch = useDispatch()

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
            <Button
              onClick={() =>
                dispatch(session.effects.redirect(routes.createPlaylist))
              }
            >
              Create new
            </Button>
          </OptionCard>
          <OptionCard>
            <ImportPlaylistIcon />
            <OptionDescription>
              Import your existing playlist.
            </OptionDescription>
            <Button disabled>Import</Button>
          </OptionCard>
        </OptionsContainer>
      </Container>
    </Modal>
  )
}
