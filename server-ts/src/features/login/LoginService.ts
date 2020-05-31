import jwt from 'jsonwebtoken'
import { EitherAsync } from 'purify-ts/EitherAsync'
import { Codec, string, GetInterface } from 'purify-ts/Codec'
import { findUserByUsername } from '../user/UserRepo'
import { Pool } from 'pg'
import { User } from '../user/User'
import { compareSync } from 'bcrypt'
import { Either } from 'purify-ts/Either'
import { jwtSecret } from '../../infrastructure/Secrets'

export enum LoginError {
  UserNotFound = 'UserNotFound',
  InvalidRequest = 'InvalidRequest'
}

const LoginBody = Codec.interface({
  username: string,
  password: string
})

type LoginBody = GetInterface<typeof LoginBody>

export const login = (
  env: Env,
  rawBody: unknown
): EitherAsync<LoginError, string> =>
  EitherAsync(async ({ liftEither, fromPromise }) => {
    const body = await liftEither(parseBody(rawBody))
    const user = await fromPromise(findUserByCredentials(env.pool, body))
    return generateJwt(user.username)
  })

const parseBody = (rawBody: unknown): Either<LoginError, LoginBody> =>
  LoginBody.decode(rawBody).mapLeft(_ => LoginError.InvalidRequest)

const findUserByCredentials = (
  pool: Pool,
  body: LoginBody
): Promise<Either<LoginError, User>> => {
  const isPasswordValid = (user: User): boolean =>
    compareSync(user.password, body.password)

  return findUserByUsername(body.username, pool).then(maybeUser =>
    maybeUser.filter(isPasswordValid).toEither(LoginError.UserNotFound)
  )
}

const generateJwt = (username: string): string =>
  jwt.sign(username, jwtSecret, { algorithm: 'RS256', issuer: 'listeo' })
