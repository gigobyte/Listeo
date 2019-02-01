module Feature.Login.LoginService
  ( login
  )
where

import Protolude
import Control.Monad (mfilter)
import Control.Monad.Except (liftEither)
import Feature.User.User (User)
import Feature.User.UserRepoClass (UserRepo(..))
import Infrastructure.AppError
import Feature.Login.LoginError (LoginError(..))
import Feature.Login.LoginBody (LoginBody)
import qualified Feature.Login.LoginBody as LoginBody
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Feature.User.User as User
import qualified Infrastructure.Utils.Crypto as Crypto
import qualified Infrastructure.Secrets as Secrets
import qualified Web.JWT as JWT

login :: UserRepo m => LByteString -> m (Either (AppError LoginError) Text)
login rawBody = runExceptT $ do
  body     <- liftEither $ parseBody rawBody
  user     <- ExceptT $ findUserByCredentials body
  jwtToken <- return $ generateJwtToken user

  return jwtToken

parseBody :: LByteString -> Either (AppError LoginError) LoginBody
parseBody rawBody = maybeToRight InvalidRequest $ Aeson.decode rawBody

findUserByCredentials
  :: UserRepo m => LoginBody -> m (Either (AppError LoginError) User)
findUserByCredentials req = do
  userInDb <- findUser (LoginBody.username req)

  return
    $ maybeToRight (DomainError UserNotFound)
    $ mfilter isPasswordValid
    $ userInDb
 where
  isPasswordValid :: User -> Bool
  isPasswordValid user =
    Crypto.validate (User.password user) (LoginBody.password req)

generateJwtToken :: User -> Text
generateJwtToken user =
  let
    cs = JWT.def
      { JWT.iss                = JWT.stringOrURI "listeo"
      , JWT.sub                = JWT.stringOrURI (User.username user)
      , JWT.unregisteredClaims = Map.fromList
        [("http://localhost:1234", (Aeson.Bool True))]
      }
    key = JWT.secret Secrets.jwtSecret
  in JWT.encodeSigned JWT.HS256 key cs
