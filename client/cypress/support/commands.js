const uuid = require('uuid/v4')

// ***********************************************
// This example commands.js shows you how to
// create various custom commands and overwrite
// existing commands.
//
// For more comprehensive examples of custom
// commands please read more here:
// https://on.cypress.io/custom-commands
// ***********************************************
//
//
// -- This is a parent command --
// Cypress.Commands.add("login", (email, password) => { ... })
//
//
// -- This is a child command --
// Cypress.Commands.add("drag", { prevSubject: 'element'}, (subject, options) => { ... })
//
//
// -- This is a dual command --
// Cypress.Commands.add("dismiss", { prevSubject: 'optional'}, (subject, options) => { ... })
//
//
// -- This will overwrite an existing command --
// Cypress.Commands.overwrite("visit", (originalFn, url, options) => { ... })

Cypress.Commands.add('dataTest', value => cy.get(`[data-test=${value}]`))

Cypress.Commands.add('registerUser', () => {
  const username = `e2e-${uuid()}`

  return cy
    .request({
      method: 'POST',
      url: 'http://localhost:8081/register',
      body: {
        username,
        email: `${username}@gmail.com`,
        password: username
      }
    })
    .then(() => ({ username, password: username }))
})

Cypress.Commands.add('login', ({ username, password }) => {
  cy.request({
    method: 'POST',
    url: 'http://localhost:8081/login',
    body: {
      username,
      password
    }
  }).then(res => {
    window.localStorage.setItem('jwt', res.body.jwt)
  })
})

Cypress.Commands.add('deleteCurrentUser', () =>
  cy
    .request({
      method: 'POST',
      url: 'http://localhost:8081/delete-me',
      headers: {
        Authorization: 'Bearer ' + localStorage.getItem('jwt')
      }
    })
    .then(() => {
      window.localStorage.removeItem('jwt')
    })
)
