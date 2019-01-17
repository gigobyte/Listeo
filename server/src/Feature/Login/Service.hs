module Feature.Login.Service
  ( login
  , LoginBody
  , LoginError
  )
where

import Protolude
import Control.Monad (mfilter)
import Control.Monad.Except (liftEither)
import Feature.User.DB (User)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Database.MongoDB as DB
import qualified Feature.User.DB as DB
import qualified Feature.User.DB as User
import qualified Infrastructure.Crypto as Crypto
import qualified Infrastructure.Secrets as Secrets
import qualified Web.JWT as JWT

instance Aeson.FromJSON LoginBody
data LoginBody = LoginBody
    { loginBodyUsername :: Text
    , loginBodyPassword :: Text
    } deriving Generic

instance Aeson.ToJSON LoginError
data LoginError
    = ValidationFailed
    | UserNotFound
    | ServerError
    deriving Generic

login :: DB.Pipe -> LByteString -> IO (Either LoginError Text)
login pipe rawBody = runExceptT $ do
  body     <- liftEither $ parseBody rawBody
  user     <- ExceptT $ findUserByCredentials pipe body
  jwtToken <- return $ generateJwtToken user

  return jwtToken

parseBody :: LByteString -> Either LoginError LoginBody
parseBody rawBody = maybeToRight ValidationFailed $ Aeson.decode rawBody

findUserByCredentials :: DB.Pipe -> LoginBody -> IO (Either LoginError User)
findUserByCredentials pipe req = do
  userInDb <- DB.findUser pipe (loginBodyUsername req)

  return $ maybeToRight UserNotFound $ mfilter isPasswordValid $ userInDb
 where
  isPasswordValid :: User -> Bool
  isPasswordValid user =
    Crypto.validate (User.password user) (loginBodyPassword req)

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
