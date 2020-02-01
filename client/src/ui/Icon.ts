import styled from 'styled-components'

interface IconProps {
  clickable?: boolean
}

const Icon = styled.i<IconProps>`
  cursor: ${props => (props.clickable ? 'pointer' : 'initial')};
`

export const Icons = {
  plusCircle: styled(Icon).attrs({ className: 'fas fa-plus-circle' })``,
  folderPlus: styled(Icon).attrs({ className: 'fas fa-folder-plus' })``,
  cloudDownload: styled(Icon).attrs({
    className: 'fas fa-cloud-download-alt'
  })``,
  times: styled(Icon).attrs({ className: 'fas fa-times' })``,
  eye: styled(Icon).attrs({ className: 'fas fa-eye' })``,
  eyeSlash: styled(Icon).attrs({ className: 'fas fa-eye-slash' })``,
  edit: styled(Icon).attrs({ className: 'fas fa-edit' })``,
  play: styled(Icon).attrs({ className: 'fas fa-play' })``,
  trash: styled(Icon).attrs({ className: 'fas fa-trash-alt' })``
}
