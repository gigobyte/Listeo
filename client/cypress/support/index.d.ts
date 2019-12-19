declare namespace Cypress {
  interface Chainable {
    dataTest(value: string): Chainable<Element>
    login(): void
  }
}
