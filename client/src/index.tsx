import 'whatwg-fetch'
import React from 'react'
import ReactDOM from 'react-dom'
import { App } from './App'

ReactDOM.render(<App />, document.querySelector('main'))

window.onerror = () => window.location.assign('/error')
