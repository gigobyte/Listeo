import { Request, Response } from 'express'
import { toPublicUser } from './User'

export const me = (req: Request, res: Response) => {
  res.json(toPublicUser(req.env.user))
}
