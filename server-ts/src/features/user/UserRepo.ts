import { Pool } from 'pg'
import { withConn } from '../../infrastructure/DB'
import { Maybe } from 'purify-ts/Maybe'
import { User } from './User'

export const findUserByUsername = (
  username: string,
  pool: Pool
): Promise<Maybe<User>> =>
  withConn(pool, conn =>
    conn
      .query('SELECT * FROM users WHERE username = $1 LIMIT 1', [username])
      .then(res => Maybe.fromNullable(res.rows[0]))
  )
