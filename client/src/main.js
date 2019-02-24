const { Elm } = require('./Main.elm')

const app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: {
    jwt: localStorage.getItem('jwt-token'),
    apiRoot: 'http://localhost:8081'
  }
})

app.ports.storeJwt.subscribe(jwt => {
  localStorage.setItem('jwt-token', jwt)
})

app.ports.removeJwt.subscribe(() => {
  localStorage.removeItem('jwt-token')
})
