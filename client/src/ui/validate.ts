type Validation = [(x: string) => boolean, string]

export const rule = (
  validationFn: (x: string) => boolean,
  error: string
): Validation => [validationFn, error]

export const validate = (
  validations: Validation[],
  value: string
): string | undefined =>
  validations.map(([f, err]) => (f(value) ? '' : err)).find(x => !!x)

/* Combinators */

export const fail = (validation: (value: string) => boolean) => (
  value: string
): boolean => !validation(value)

export const pass = (validation: (value: string) => boolean) => (
  value: string
): boolean => validation(value)

/* Rules */

export const ifBlank = (x: string): boolean => !!x

export const ifShorterThan = (minLenght: number) => (x: string): boolean =>
  x.length >= minLenght

export const ifLongerThan = (maxLength: number) => (x: string): boolean =>
  x.length <= maxLength

export const ifContains = (char: string) => (x: string): boolean =>
  x.includes(char)

export const ifRegexFails = (regex: RegExp) => (x: string): boolean =>
  regex.test(x)
