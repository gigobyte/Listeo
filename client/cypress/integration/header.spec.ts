describe('Header', () => {
  describe('Logged in', () => {
    const USERNAME = 'header--username'
    const LOGOUT = 'header--logout'
    const ADD_PLAYLIST = 'header--add-playlist'

    beforeEach(() => {
      cy.login()
      cy.visit('/')
    })

    it('shows the username', () => {
      cy.dataTest(USERNAME).should('have.text', 'testuser')
    })

    it('shows a logout button', () => {
      cy.dataTest(LOGOUT).should('be.visible')
    })

    it("shows an add playlist button when you're not on that page", () => {
      cy.dataTest(ADD_PLAYLIST).should('be.visible')
    })

    it("does not show an add playlist button when you're on that page", () => {
      cy.visit('/create-playlist')
      cy.dataTest(ADD_PLAYLIST).should('not.be.visible')
    })
  })

  describe('Logged out', () => {
    const LOGIN = 'header--login'
    const REGISTER = 'header--register'

    beforeEach(() => {
      cy.visit('/')
    })

    it('shows a login button', () => {
      cy.dataTest(LOGIN).should('be.visible')
    })

    it('shows a register button', () => {
      cy.dataTest(REGISTER).should('be.visible')
    })
  })
})
