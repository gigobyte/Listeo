export type Id<T> = number & { __brand: 'Id' & T }
