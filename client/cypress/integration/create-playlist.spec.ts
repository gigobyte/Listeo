describe('Create playlist', () => {
  beforeEach(() => {
    cy.login()
  })

  describe('Entering the page', () => {
    const ADD_PLAYLIST = 'header--add-playlist'
    const CREATE_PLAYLIST = 'add-playlist--create'

    beforeEach(() => {
      cy.visit('/')
    })

    it('there should be a button for creating a playlist', () => {
      cy.dataTest(ADD_PLAYLIST).click()
      cy.dataTest(CREATE_PLAYLIST).should('be.visible')
    })

    it('clicking the create playlist button should redirect to that page', () => {
      cy.dataTest(ADD_PLAYLIST).click()
      cy.dataTest(CREATE_PLAYLIST).click()
      cy.url().should('eq', Cypress.config().baseUrl + '/create-playlist')
    })
  })

  describe.only('Inside the page', () => {
    const NAME = 'create-playlist--name'
    const TAGS = 'create-playlist--tags'
    const DESCRIPTION = 'create-playlist--description'
    const PRIVACY_PUBLIC = 'create-playlist--privacy-public'
    const PRIVACY_PRIVATE = 'create-playlist--privacy-private'
    const STYLE_RANKED = 'create-playlist--style-ranked'
    const STYLE_UNORDERED = 'create-playlist--style-unordered'
    const SUBMIT = 'create-playlist--submit'
    const ERROR = '-error'
    const INPUT = '-input'
    const TAG = i => `-tag-${i}`
    const DELETE = '-delete'

    beforeEach(() => {
      cy.visit('/create-playlist')
    })

    describe('Name field', () => {
      it('works', () => {
        cy.dataTest(NAME)
          .type('v')
          .should('have.value', 'v')
      })

      it('does not trigger submit on enter', () => {
        cy.dataTest(NAME).type('{enter}')
        cy.dataTest(NAME + ERROR).should('not.be.visible')
      })

      it('does not accept empty input', () => {
        cy.dataTest(SUBMIT).click()
        cy.dataTest(NAME + ERROR)
          .should('be.visible')
          .should('have.text', 'Please enter the name of the playlist')
      })

      it('does not show error if its value is not empty', () => {
        cy.dataTest(NAME).type('v{enter}')
        cy.dataTest(NAME + ERROR).should('not.be.visible')
      })

      it('should only show validation error if the form is submitted', () => {
        cy.dataTest(NAME)
          .focus()
          .blur()
        cy.dataTest(NAME + ERROR).should('not.be.visible')
      })
    })

    describe('Tags field', () => {
      it('works', () => {
        cy.dataTest(TAGS + INPUT)
          .type('v')
          .should('have.value', 'v')
      })

      it('creates a tag underneath the input on enter', () => {
        cy.dataTest(TAGS + INPUT).type('v{enter}')
        cy.dataTest(TAGS + TAG(1))
          .should('exist')
          .should('have.text', 'v')
      })

      it('should create no tag if the input is empty', () => {
        cy.dataTest(TAGS + INPUT).type('{enter}')
        cy.dataTest(TAGS + TAG(1)).should('not.exist')
      })

      it('should not allow addition of the same tag twice', () => {
        cy.dataTest(TAGS + INPUT)
          .type('v{enter}')
          .type('v{enter}')
        cy.dataTest(TAGS + TAG(1)).should('exist')
        cy.dataTest(TAGS + TAG(2)).should('not.exist')
      })

      it('should clear input after a tag is added', () => {
        cy.dataTest(TAGS + INPUT)
          .type('v{enter}')
          .should('have.value', '')
      })

      it('should allow you to delete a tag', () => {
        cy.dataTest(TAGS + INPUT).type('v{enter}')
        cy.dataTest(TAGS + TAG(1) + DELETE).click()
        cy.dataTest(TAGS + TAG(1)).should('not.exist')
      })
    })
  })
})
