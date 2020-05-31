import { Request, Response } from 'express'
import { login, LoginError } from './LoginService'

export const loginRoutes = {
  login(req: Request, res: Response) {
    login(req.env, req.body)
      .run()
      .then(response => {
        response
          .ifLeft(error => {
            switch (error) {
              case LoginError.UserNotFound:
                return res.status(400).json({ error })

              case LoginError.InvalidRequest:
                return res.status(500).json({ error })
            }
          })
          .ifRight(jwt => {
            res.json({ jwt })
          })
      })
  }
}
