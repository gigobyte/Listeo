const uuid = require('uuid/v4')

describe('Login', () => {
  const USERNAME = 'login--username'
  const PASSWORD = 'login--password'
  const SUBMIT = 'login--submit'
  const API_ERROR = 'login--api-error'
  const ERROR = '-error'

  beforeEach(() => {
    cy.visit('/login')

    Cypress.on('window:before:load', win => {
      delete win.fetch
    })
  })

  describe('Username field', () => {
    it('works', () => {
      cy.dataTest(USERNAME)
        .type('v')
        .should('have.value', 'v')
    })

    it('triggers submit on enter', () => {
      cy.dataTest(USERNAME).type('{enter}')
      cy.dataTest(USERNAME + ERROR)
        .should('be.visible')
        .should('have.text', 'Please enter username')
    })

    it('does not show error if its value is not empty', () => {
      cy.dataTest(USERNAME).type('v{enter}')
      cy.dataTest(USERNAME + ERROR).should('not.be.visible')
    })

    it('should trim input', () => {
      cy.dataTest(USERNAME)
        .type('    ')
        .should('have.value', '')
    })

    it('should only show validation error if the form is submitted', () => {
      cy.dataTest(USERNAME)
        .focus()
        .blur()
      cy.dataTest(USERNAME + ERROR).should('not.be.visible')
    })
  })

  describe('Password field', () => {
    it('works', () => {
      cy.dataTest(PASSWORD)
        .type('v')
        .should('have.value', 'v')
    })

    it('triggers submit on enter', () => {
      cy.dataTest(PASSWORD).type('{enter}')
      cy.dataTest(PASSWORD + ERROR)
        .should('be.visible')
        .should('have.text', 'Please enter password')
    })

    it('does not show error if its value is not empty', () => {
      cy.dataTest(PASSWORD).type('v{enter}')
      cy.dataTest(PASSWORD + ERROR).should('not.be.visible')
    })

    it('should trim input', () => {
      cy.dataTest(PASSWORD)
        .type('    ')
        .should('have.value', '')
    })

    it('should only show validation error if the form is submitted', () => {
      cy.dataTest(PASSWORD)
        .focus()
        .blur()
      cy.dataTest(PASSWORD + ERROR).should('not.be.visible')
    })
  })

  describe('Submit button', () => {
    it('triggers validation on click', () => {
      cy.dataTest(SUBMIT).click()
      cy.dataTest(USERNAME + ERROR).should('be.visible')
      cy.dataTest(PASSWORD + ERROR).should('be.visible')
    })

    it('becomes disabled if the username is showing an error', () => {
      cy.dataTest(USERNAME).type('{enter}')
      cy.dataTest(PASSWORD).type('v')
      cy.dataTest(SUBMIT).should('be.disabled')
    })

    it('becomes disabled if the password is showing an error', () => {
      cy.dataTest(USERNAME).type('v')
      cy.dataTest(PASSWORD).type('{enter}')
      cy.dataTest(SUBMIT).should('be.disabled')
    })

    it('becomes disabled if the login request is pending', () => {
      cy.server()
      cy.route({
        method: 'POST',
        url: 'http://localhost:8081/login',
        status: 200,
        response: {},
        delay: 500
      }).as('login')

      cy.dataTest(USERNAME).type('v')
      cy.dataTest(PASSWORD).type('v')
      cy.dataTest(SUBMIT).click()
      cy.dataTest(SUBMIT).should('be.disabled')

      cy.wait('@login').then(() => {
        cy.dataTest(SUBMIT).should('not.be.disabled')
      })
    })

    it('should show error if credentials are not valid on submit', () => {
      cy.server()
      cy.route('POST', 'http://localhost:8081/login').as('login')
      cy.dataTest(USERNAME).type(uuid())
      cy.dataTest(PASSWORD).type(uuid())
      cy.dataTest(SUBMIT).click()

      cy.wait('@login').then(() => {
        cy.dataTest(API_ERROR).should('have.text', 'User not found')
      })
    })

    it('should redirect to home after successful login', () => {
      cy.registerUser().then(({ username, password }) => {
        cy.server()
        cy.route('POST', 'http://localhost:8081/login').as('login')

        cy.dataTest(USERNAME).type(username)
        cy.dataTest(PASSWORD).type(password)
        cy.dataTest(SUBMIT).click()

        cy.wait('@login').then(() => {
          cy.url().should('eq', Cypress.config().baseUrl + '/')
          cy.deleteCurrentUser()
        })
      })
    })
  })
})
