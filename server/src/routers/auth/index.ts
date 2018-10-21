import { Router } from 'express'
import * as passport from 'passport'

const authRouter = Router()

authRouter.route('/me').get(
    passport.authenticate('jwt', { session: false }),
    (req: any, res) => {
        res.status(200).json(req.getUser())
    }
)

export { authRouter }