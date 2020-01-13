declare namespace Cypress {
  interface Chainable {
    dataTest(value: string): Chainable<Element>
    registerUser(): Chainable<{ username: string; password: string }>
    deleteCurrentUser(): Chainable<void>
    login(credentials: { username: string; password: string }): void
  }
}
