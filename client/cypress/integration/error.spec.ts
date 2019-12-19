describe('Default error handling', () => {
  beforeEach(() => {
    Cypress.on('window:before:load', win => {
      delete win.fetch
    })
  })

  it('should redirect to /error if any of the request fail with 500', () => {
    cy.server()
    cy.route({
      method: 'GET',
      url: 'http://localhost:8081/me',
      response: '',
      status: 500
    })
    cy.visit('/')

    cy.url().should('eq', Cypress.config().baseUrl + '/error')
  })

  it('works on other pages as well (user is handled differently)', () => {
    cy.server()
    cy.route({
      method: 'POST',
      url: 'http://localhost:8081/login',
      response: '',
      status: 500
    })
    cy.visit('/login')
    cy.dataTest('login--username').type('validnow')
    cy.dataTest('login--password').type('validnow')
    cy.dataTest('login--submit').click()

    cy.url().should('eq', Cypress.config().baseUrl + '/error')
  })
})
