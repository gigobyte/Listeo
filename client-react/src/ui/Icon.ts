import styled from 'styled-components'

const Icon = styled.i`
  width: 100%;
`

export const Icons = {
  plusCircle: styled(Icon).attrs({ className: 'fas fa-plus-circle' })``,
  folderPlus: styled(Icon).attrs({ className: 'fas fa-folder-plus' })``,
  cloudDownload: styled(Icon).attrs({
    className: 'fas fa-cloud-download-alt'
  })``,
  times: styled(Icon).attrs({ className: 'fas fa-times' })``
}
