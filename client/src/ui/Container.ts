import { ThemedStyledFunction } from 'styled-components'

export const fullHeight = (el: ThemedStyledFunction<any, any>) =>
  el`
    height: 100%
    `

export const centered = (el: ThemedStyledFunction<any, any>) =>
  el`
    display: flex;
    justify-content: center;
    align-items: center;
    flex-direction: column;
    `
