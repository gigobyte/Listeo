describe('Create playlist', () => {
  beforeEach(() => {
    cy.login()

    Cypress.on('window:before:load', win => {
      delete win.fetch
    })
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

  describe('Inside the page', () => {
    const NAME = 'create-playlist--name'
    const TAGS = 'create-playlist--tags'
    const DESCRIPTION = 'create-playlist--description'
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

      it('does not accept name longer than 99 characters', () => {
        cy.dataTest(NAME).type(`${'v'.repeat(100)}`)
        cy.dataTest(SUBMIT).click()
        cy.dataTest(NAME + ERROR)
          .should('be.visible')
          .should('have.text', 'Please enter a shorter name')
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

      it('trims input', () => {
        cy.dataTest(TAGS + INPUT)
          .type(' v ')
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

      it('should show an error when you try to add an empty tag', () => {
        cy.dataTest(TAGS + INPUT).type('{enter}')
        cy.dataTest(TAGS + INPUT + ERROR)
          .should('be.visible')
          .should('have.text', 'Please enter a tag first')
      })

      it('should show an error when you try to a tag with more than 99 characters', () => {
        cy.dataTest(TAGS + INPUT).type(`${'v'.repeat(100)}{enter}`)
        cy.dataTest(TAGS + INPUT + ERROR)
          .should('be.visible')
          .should('have.text', 'Please enter a shorter tag')
      })

      it('should show an error when you try to add more than 10 tags', () => {
        for (let i = 0; i < 10; i++) {
          cy.dataTest(TAGS + INPUT).type(`${i}{enter}`)
        }

        cy.dataTest(TAGS + INPUT).type(`v{enter}`)
        cy.dataTest(TAGS + INPUT + ERROR)
          .should('be.visible')
          .should('have.text', 'You can only add up to 10 tags')
      })

      it('should clear the error when the input changes', () => {
        cy.dataTest(TAGS + INPUT)
          .type('{enter}')
          .type('v')
        cy.dataTest(TAGS + INPUT + ERROR).should('not.be.visible')
      })
    })

    describe('Description field', () => {
      it('works', () => {
        cy.dataTest(DESCRIPTION)
          .type('v')
          .should('have.value', 'v')
      })
    })

    describe('Submit button', () => {
      it('triggers validation on click', () => {
        cy.dataTest(SUBMIT).click()
        cy.dataTest(NAME + ERROR).should('be.visible')
      })

      it('becomes disabled if the name is showing an error', () => {
        cy.dataTest(SUBMIT).click()
        cy.dataTest(SUBMIT).should('be.disabled')
      })

      it('becomes disabled if the create playlist request is pending', () => {
        cy.server()
        cy.route({
          method: 'POST',
          url: 'http://localhost:8081/playlist',
          status: 500,
          response: {},
          delay: 500
        }).as('createPlaylist')

        cy.dataTest(NAME).type('v')
        cy.dataTest(SUBMIT).click()
        cy.dataTest(SUBMIT).should('be.disabled')

        cy.wait('@createPlaylist').then(() => {
          cy.dataTest(SUBMIT).should('not.be.disabled')
        })
      })

      it('should redirect to the playlist page after success', () => {
        cy.server()
        cy.route({
          method: 'POST',
          url: 'http://localhost:8081/playlist',
          status: 200,
          response: { playlistId: 1337 },
          delay: 500
        }).as('createPlaylist')

        cy.dataTest(NAME).type('v')
        cy.dataTest(SUBMIT).click()

        cy.wait('@createPlaylist').then(() => {
          cy.url().should('eq', Cypress.config().baseUrl + '/playlist/1337')
        })
      })
    })
  })
})
