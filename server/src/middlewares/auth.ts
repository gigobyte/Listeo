import * as passport from 'passport'
import * as jwt from 'passport-jwt'
import { JWT_SECRET } from '../env'
import { User } from '../models/User'
import { Maybe } from 'purify-ts/adts/Maybe'

passport.use(new jwt.Strategy({
    jwtFromRequest: jwt.ExtractJwt.fromHeader('authorization'), secretOrKey: JWT_SECRET
}, async (payload, done) => {
    const getUser = () =>
        User.findById(payload.sub)
            .then(user => Maybe.fromNullable(user).caseOf({
                Just: x => done(null, x),
                Nothing: () => done(null, false)
            }))
            .catch(err => done(err, false))

    done(null, { getUser })
}))