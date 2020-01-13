import 'whatwg-fetch'
import React from 'react'
import ReactDOM from 'react-dom'
import { App } from './App'

ReactDOM.render(<App />, document.querySelector('main'))

if (process.env.NODE_ENV === 'production') {
  window.onerror = () => window.location.assign('/error')
}
