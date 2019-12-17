describe('Login', () => {
  const USERNAME = 'login--username'
  const PASSWORD = 'login--password'
  const SUBMIT = 'login--submit'
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
        .type('sample')
        .should('have.value', 'sample')
    })

    it('triggers submit on enter', () => {
      cy.dataTest(USERNAME).type('{enter}')
      cy.dataTest(USERNAME + ERROR)
        .should('be.visible')
        .should('have.text', 'Please enter username')
    })

    it('does not show error if its value is not empty', () => {
      cy.dataTest(USERNAME).type('sample{enter}')
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
        .type('sample')
        .should('have.value', 'sample')
    })

    it('triggers submit on enter', () => {
      cy.dataTest(PASSWORD).type('{enter}')
      cy.dataTest(PASSWORD + ERROR)
        .should('be.visible')
        .should('have.text', 'Please enter password')
    })

    it('does not show error if its value is not empty', () => {
      cy.dataTest(PASSWORD).type('sample{enter}')
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
      cy.dataTest(PASSWORD).type('validnow')
      cy.dataTest(SUBMIT).should('be.disabled')
    })

    it('becomes disabled if the password is showing an error', () => {
      cy.dataTest(USERNAME).type('validnow')
      cy.dataTest(PASSWORD).type('{enter}')
      cy.dataTest(SUBMIT).should('be.disabled')
    })

    it('becomes disabled if the login request is pending', () => {
      cy.server({ delay: 5000, status: 500 })
      cy.clock()
      cy.route('POST', 'http://localhost:8081/login', {})

      cy.dataTest(USERNAME).type('validnow')
      cy.dataTest(PASSWORD).type('validnow')
      cy.dataTest(SUBMIT).click()
      cy.dataTest(SUBMIT).should('be.disabled')
      cy.tick(5000)
      cy.dataTest(SUBMIT).should('not.be.disabled')
    })
  })
})
