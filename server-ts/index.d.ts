export * from 'express'

declare module 'express' {
  interface Request {
    env: {
      user: import('./src/features/user/User').User
      pool: import('pg').Pool
    }
  }
}

declare module 'express-serve-static-core' {
  interface Request {
    env: {
      user: import('./src/features/user/User').User
      pool: import('pg').Pool
    }
  }
}
