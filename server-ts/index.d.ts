export * from 'express'

declare global {
  interface Env {
    user: import('./src/features/user/User').User
    pool: import('pg').Pool
  }
}

declare module 'express' {
  interface Request {
    env: Env
  }
}

declare module 'express-serve-static-core' {
  interface Request {
    env: Env
  }
}
