module Feature.Register.Service
  ( insertUser
  , mkUser
  )
where

import Protolude
import Flow
import Feature.Register.Models.RegisterBody (RegisterBody(..))
import Feature.Register.Models.RegisterResponse (RegisterError(..))
import Feature.User.Models.User (User(..))
import qualified Data.Bson as Bson
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import qualified Database.MongoDB as MongoDB
import qualified Feature.User.DB as DB
import qualified Feature.User.Models.User as User
import qualified Feature.Register.Models.RegisterBody as RegisterBody
import qualified Infrastructure.Crypto as Crypto

insertUser :: User -> MongoDB.Action IO (Either RegisterError ())
insertUser user = do
  userInDB <- DB.findUser (User.username user)

  case userInDB of
    Just _  -> pure (Left UserAlreadyExists)
    Nothing -> do
      updatedUser <- liftIO (hashPasswordInUser user)
      DB.insertUser <$> updatedUser |> maybeToRight ServerError |> sequence

mkUser :: Time.UTCTime -> RegisterBody -> Either RegisterError User
mkUser dateNow req =
  User
    <$> (pure <| Bson.Oid 0 0)
    <*> (validateUsername <| RegisterBody.username req)
    <*> (validatePassword <| RegisterBody.password req)
    <*> pure dateNow
    |>  maybeToRight ValidationFailed

validateUsername :: Text -> Maybe Text
validateUsername str
  | T.length str < 4 = Nothing
  | otherwise        = Just (T.strip str)

validatePassword :: Text -> Maybe Text
validatePassword str
  | T.length str < 6 = Nothing
  | otherwise        = Just (T.strip str)

hashPasswordInUser :: User -> IO (Maybe User)
hashPasswordInUser user@User { password } = do
  hashedPassword <- password |> encodeUtf8 |> Crypto.hash
  pure (setHashedPassword <$> decodeUtf8 <$> hashedPassword)
 where
  setHashedPassword :: Text -> User
  setHashedPassword p = user { User.password = p }
