module Feature.Login.Service
    ( areCredentialsValid
    , generateJwtToken
    ) where

import           Data.Aeson             (Value (..))
import           Data.Map               as M
import           Database.MongoDB       (Action)
import           Feature.Login.Types    (LoginBody (..), LoginError (..))
import qualified Feature.User.DB        as DB
import qualified Infrastructure.Crypto  as Crypto
import           Infrastructure.Secrets (jwtSecret)
import           Protolude
import           Web.JWT

areCredentialsValid :: LoginBody -> Action IO (Either LoginError Bool)
areCredentialsValid req = do
    hashedPassword <- liftIO $ hashPasswordAttempt $ password req

    case hashedPassword of
        Just password  -> do
            user <- DB.findUser (username req) password

            pure undefined

        Nothing -> pure $ Left ServerError

generateJwtToken :: Int -> Text
generateJwtToken userId =
    let
        cs = def
            { iss = stringOrURI "listeo"
            , sub = stringOrURI (show userId)
            , unregisteredClaims = M.fromList [("http://localhost:1234", (Bool True))]
            }
        key = secret jwtSecret
    in encodeSigned HS256 key cs

hashPasswordAttempt :: Text -> IO (Maybe Text)
hashPasswordAttempt attempt = do
    maybeHashed <- Crypto.hash $ encodeUtf8 attempt
    pure $ decodeUtf8 <$> maybeHashed
