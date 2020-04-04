const { v4: uuidv4 } = require('uuid')

describe('Register', () => {
  const USERNAME = 'register--username'
  const EMAIL = 'register--email'
  const PASSWORD = 'register--password'
  const SUBMIT = 'register--submit'
  const API_ERROR = 'register--api-error'
  const ERROR = '-error'

  beforeEach(() => {
    cy.visit('/register')
  })

  describe('Username field', () => {
    it('works', () => {
      cy.dataTest(USERNAME).type('v').should('have.value', 'v')
    })

    it('triggers submit on enter', () => {
      cy.dataTest(USERNAME).type('{enter}')
      cy.dataTest(USERNAME + ERROR)
        .should('be.visible')
        .should('have.text', 'Please enter username')
    })

    it('does not accept usernames with special characters', () => {
      cy.dataTest(USERNAME).type('!@$$!{enter}')
      cy.dataTest(USERNAME + ERROR)
        .should('be.visible')
        .should(
          'have.text',
          'Please enter a valid username, the only special characters allowed are - and _'
        )
    })

    it('does not accept usernames with less than 4 characters', () => {
      cy.dataTest(USERNAME).type('v{enter}')
      cy.dataTest(USERNAME + ERROR)
        .should('be.visible')
        .should('have.text', 'Username must be at least 4 characters long')
    })

    it('does not accept usernames with more than 99 characters', () => {
      cy.dataTest(USERNAME).type(`${'v'.repeat(100)}{enter}`)
      cy.dataTest(USERNAME + ERROR)
        .should('be.visible')
        .should('have.text', 'Username is too long')
    })

    it('does not show error if its value is not empty', () => {
      cy.dataTest(USERNAME).type('sam-_le{enter}')
      cy.dataTest(USERNAME + ERROR).should('not.be.visible')
    })

    it('should only show validation error if the form is submitted', () => {
      cy.dataTest(USERNAME).focus().blur()
      cy.dataTest(USERNAME + ERROR).should('not.be.visible')
    })

    it('should trim input', () => {
      cy.dataTest(USERNAME).type('    ').should('have.value', '')
    })
  })

  describe('Email field', () => {
    it('works', () => {
      cy.dataTest(EMAIL).type('v').should('have.value', 'v')
    })

    it('triggers submit on enter', () => {
      cy.dataTest(EMAIL).type('{enter}')
      cy.dataTest(EMAIL + ERROR)
        .should('be.visible')
        .should('have.text', 'Please enter email')
    })

    it('does not accept emails without @', () => {
      cy.dataTest(EMAIL).type('v{enter}')
      cy.dataTest(EMAIL + ERROR)
        .should('be.visible')
        .should('have.text', 'Please enter a valid email')
    })

    it('does not show error if its value is not empty', () => {
      cy.dataTest(EMAIL).type('t@t{enter}')
      cy.dataTest(EMAIL + ERROR).should('not.be.visible')
    })

    it('should only show validation error if the form is submitted', () => {
      cy.dataTest(EMAIL).focus().blur()
      cy.dataTest(EMAIL + ERROR).should('not.be.visible')
    })

    it('should trim input', () => {
      cy.dataTest(EMAIL).type('    ').should('have.value', '')
    })
  })

  describe('Password field', () => {
    it('works', () => {
      cy.dataTest(PASSWORD).type('v').should('have.value', 'v')
    })

    it('triggers submit on enter', () => {
      cy.dataTest(PASSWORD).type('{enter}')
      cy.dataTest(PASSWORD + ERROR)
        .should('be.visible')
        .should('have.text', 'Please enter password')
    })

    it('does not accept passwords with less than 6 characters', () => {
      cy.dataTest(PASSWORD).type('v{enter}')
      cy.dataTest(PASSWORD + ERROR)
        .should('be.visible')
        .should('have.text', 'Password must be at least 6 characters long')
    })

    it('does not show error if its value is not empty', () => {
      cy.dataTest(PASSWORD).type('sample{enter}')
      cy.dataTest(PASSWORD + ERROR).should('not.be.visible')
    })

    it('should only show validation error if the form is submitted', () => {
      cy.dataTest(PASSWORD).focus().blur()
      cy.dataTest(PASSWORD + ERROR).should('not.be.visible')
    })

    it('should trim input', () => {
      cy.dataTest(PASSWORD).type('    ').should('have.value', '')
    })
  })

  describe('Submit button', () => {
    it('triggers validation on click', () => {
      cy.dataTest(SUBMIT).click()
      cy.dataTest(USERNAME + ERROR).should('be.visible')
      cy.dataTest(EMAIL + ERROR).should('be.visible')
      cy.dataTest(PASSWORD + ERROR).should('be.visible')
    })

    it('becomes disabled if the username is showing an error', () => {
      cy.dataTest(USERNAME).type('{enter}')
      cy.dataTest(PASSWORD).type('validnow')
      cy.dataTest(EMAIL).type('v@n')
      cy.dataTest(SUBMIT).should('be.disabled')
    })

    it('becomes disabled if the password is showing an error', () => {
      cy.dataTest(USERNAME).type('validnow')
      cy.dataTest(PASSWORD).type('{enter}')
      cy.dataTest(EMAIL).type('v@n')
      cy.dataTest(SUBMIT).should('be.disabled')
    })

    it('becomes disabled if the email is showing an error', () => {
      cy.dataTest(USERNAME).type('validnow')
      cy.dataTest(PASSWORD).type('validnow')
      cy.dataTest(EMAIL).type('{enter}')
      cy.dataTest(SUBMIT).should('be.disabled')
    })

    it('becomes disabled if the register request is pending', () => {
      cy.server()
      cy.route({
        method: 'POST',
        url: 'http://localhost:8081/register',
        status: 200,
        response: {},
        delay: 500
      }).as('register')

      cy.dataTest(USERNAME).type('validnow')
      cy.dataTest(PASSWORD).type('validnow')
      cy.dataTest(EMAIL).type('v@n')
      cy.dataTest(SUBMIT).click()
      cy.dataTest(SUBMIT).should('be.disabled')

      cy.wait('@register').then(() => {
        cy.dataTest(SUBMIT).should('not.be.disabled')
      })
    })

    it('should show error if user already exists on submit', () => {
      cy.registerUser().then(({ username, password }) => {
        cy.server()
        cy.route('POST', 'http://localhost:8081/register').as('register')

        cy.dataTest(USERNAME).type(username)
        cy.dataTest(PASSWORD).type(password)
        cy.dataTest(EMAIL).type('v@n')
        cy.dataTest(SUBMIT).click()

        cy.wait('@register').then(() => {
          cy.dataTest(API_ERROR).should('have.text', 'User already exists')
          cy.deleteCurrentUser()
        })
      })
    })

    it('should redirect to home after successful registration', () => {
      cy.server()
      cy.route('POST', 'http://localhost:8081/register').as('register')

      cy.dataTest(USERNAME).type(uuidv4())
      cy.dataTest(PASSWORD).type(uuidv4())
      cy.dataTest(EMAIL).type(`${uuidv4()}@gmail.com`)
      cy.dataTest(SUBMIT).click()

      cy.wait('@register').then(() => {
        cy.url().should('eq', Cypress.config().baseUrl + '/')
        cy.deleteCurrentUser()
      })
    })
  })
})
