module Feature.Login.LoginService
  ( login
  , generateJwtToken
  )
where

import Protolude
import Data.Aeson
import Control.Monad (mfilter)
import Control.Monad.Except (liftEither)
import Feature.User.User (User(..))
import Feature.User.UserRepoClass (UserRepo(..))
import Feature.Login.LoginResult (LoginError(..))
import qualified Data.Aeson as Aeson
import qualified Infrastructure.Utils.Crypto as Crypto
import qualified Infrastructure.Secrets as Secrets
import qualified Web.JWT as JWT

data Login = Login
  { loginUsername :: Text
  , loginPassword :: Text
  } deriving Generic

instance FromJSON Login where
  parseJSON =
    withObject "login" $ \o -> Login <$> o .: "username" <*> o .: "password"

login :: UserRepo m => LByteString -> m (Either LoginError Text)
login rawBody = runExceptT $ do
  body <- liftEither $ parseBody rawBody
  user <- ExceptT $ findUserByCredentials body
  return $ generateJwtToken (userUsername user)

parseBody :: LByteString -> Either LoginError Login
parseBody rawBody = maybeToRight InvalidRequest $ Aeson.decode rawBody

findUserByCredentials :: UserRepo m => Login -> m (Either LoginError User)
findUserByCredentials req = do
  userInDb <- mfilter isPasswordValid <$> findUser (loginUsername req)

  return $ maybeToRight UserNotFound $ userInDb
 where
  isPasswordValid :: User -> Bool
  isPasswordValid user =
    Crypto.validate (userPassword user) (loginPassword req)

generateJwtToken :: Text -> Text
generateJwtToken username = JWT.encodeSigned key mempty cs
 where
  cs = mempty
    { JWT.iss = JWT.stringOrURI "listeo"
    , JWT.sub = JWT.stringOrURI username
    }
  key = JWT.hmacSecret Secrets.jwtSecret
