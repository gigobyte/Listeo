import jwt from 'jsonwebtoken'
import { Request, Response, NextFunction } from 'express'
import { Maybe } from 'purify-ts/Maybe'
import { MaybeAsync } from 'purify-ts/MaybeAsync'
import { jwtSecret } from '../../infrastructure/Secrets'
import { findUserByUsername } from './UserRepo'

export const optionalUser = (req: Request, _: Response, next: NextFunction) => {
  MaybeAsync(async ({ liftMaybe, fromPromise }) => {
    const authHeader = await liftMaybe(
      Maybe.fromNullable(req.header('Authorization'))
    )

    const username = await liftMaybe(
      Maybe.encase(() => jwt.verify(authHeader, jwtSecret))
    )

    const user = await fromPromise(
      findUserByUsername(username as string, req.env.pool)
    )

    return user
  })
    .run()
    .then(maybeUser => {
      if (maybeUser.isJust()) {
        req.env.user = maybeUser.extract()
      }

      next()
    })
}

export const requireUser = (
  req: Request,
  res: Response,
  next: NextFunction
) => {
  optionalUser(req, res, () => {
    if (!req.env.user) {
      res.status(401).send()
    } else {
      next()
    }
  })
}
