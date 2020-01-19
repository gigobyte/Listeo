export type Id<T> = unknown & { __brand: 'Id' & T }
