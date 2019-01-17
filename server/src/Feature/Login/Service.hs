module Feature.Login.Service
  ( login
  , LoginBody
  , LoginError
  )
where

import Protolude
import Control.Monad (mfilter)
import Control.Monad.Except (liftEither)
import Feature.User.Service
import Feature.User.DB (User)
import Infrastructure.AppError
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Feature.User.DB as User
import qualified Infrastructure.Crypto as Crypto
import qualified Infrastructure.Secrets as Secrets
import qualified Web.JWT as JWT

instance Aeson.FromJSON LoginBody
data LoginBody = LoginBody
    { username :: Text
    , password :: Text
    } deriving Generic

instance Aeson.ToJSON LoginError where
  toJSON err = Aeson.genericToJSON (Aeson.defaultOptions { Aeson.tagSingleConstructors = True }) err

data LoginError
    = UserNotFound
    deriving Generic

login :: UserRepo m => LByteString -> m (Either (AppError LoginError) Text)
login rawBody = runExceptT $ do
  body     <- liftEither $ parseBody rawBody
  user     <- ExceptT $ findUserByCredentials body
  jwtToken <- return $ generateJwtToken user

  return jwtToken

parseBody :: LByteString -> Either (AppError LoginError) LoginBody
parseBody rawBody = maybeToRight ValidationFailed $ Aeson.decode rawBody

findUserByCredentials
  :: (UserRepo m) => LoginBody -> m (Either (AppError LoginError) User)
findUserByCredentials req = do
  userInDb <- findUser (username req)

  return
    $ maybeToRight (DomainError UserNotFound)
    $ mfilter isPasswordValid
    $ userInDb
 where
  isPasswordValid :: User -> Bool
  isPasswordValid user = Crypto.validate (User.password user) (password req)

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
