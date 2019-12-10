import React from 'react'
import styled from 'styled-components'
import { colors } from './color'
import { Icons } from './Icon'

interface ModalProps {
  onClose: () => void
}

const Overlay = styled.div`
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  z-index: 999;
  overflow: hidden;
`

const Container = styled.div`
  @keyframes fadeIn {
    0% {
      opacity: 0;
    }

    100% {
      opacity: 1;
    }
  }

  position: relative;
  width: 50%;
  margin: auto;
  top: 20%;
  background-color: ${colors.white};
  padding: 40px 0;
  border-radius: 5px;
  animation-name: fadeIn;
  animation-duration: 200ms;
`

const Backdrop = styled.div`
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  opacity: 0.3;
  background-color: ${colors.black};
`

const CloseIcon = styled(Icons.times)`
  position: absolute;
  top: 10px;
  right: 10px;
  text-align: right;
  color: ${colors.blue200};
  cursor: pointer;
`

export const Modal: React.FC<ModalProps> = ({ onClose, children }) => (
  <>
    <Overlay>
      <Container>
        <CloseIcon onClick={onClose} />
        {children}
      </Container>
    </Overlay>
    <Backdrop />
  </>
)
