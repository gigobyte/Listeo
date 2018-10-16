const { Elm } = require('./Main.elm');

Elm.Main.init({
  node: document.querySelector('main'),
  flags: {
    jwt: localStorage.getItem('jwt-token')
  }
});