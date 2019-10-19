export const rule = (
  validationFn: (x: string) => boolean,
  error: string
): [(x: string) => boolean, string] => [validationFn, error]

export const ifBlank = (x: string): boolean => !!x

export const ifShorterThan = (maxLenght: number) => (x: string): boolean =>
  x.length >= maxLenght
