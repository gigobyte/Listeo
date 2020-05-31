import express from 'express'
import morgan from 'morgan'
import cors from 'cors'
import { createDbPool } from './infrastructure/DB'
import { userRoutes } from './features/user/UserRouter'
import { requireUser } from './features/user/UserMiddleware'
import { loginRoutes } from './features/login/LoginRouter'

async function main() {
  const pool = await createDbPool()
  const app = express()

  app.use(morgan('dev'))
  app.use(cors())
  app.use(express.json())
  app.use((req, _, next) => {
    req.env = {} as any
    req.env.pool = pool
    next()
  })

  app.get('/me', requireUser, userRoutes.me).post('/login', loginRoutes.login)

  app.listen(process.env.PORT || 8081, () => {
    console.log('App started successfully!')
  })
}

main()
