module Infrastructure.Middleware.Auth where

import           Data.Text.Lazy            (drop)
import qualified Database.MongoDB          as DB
import qualified Feature.User.DB           as DB
import           Feature.User.Types        as User
import qualified Infrastructure.DB         as DB
import           Network.HTTP.Types.Status (status401)
import           Protolude                 hiding (drop)
import           Web.JWT                   (claims, decode, stringOrURIToText,
                                            sub)
import           Web.Scotty                (ActionM, header, status)

headerToUsername :: Text -> Maybe Text
headerToUsername authHeader =
    stringOrURIToText <$> (sub =<< claims <$> decode authHeader)

dbUserToPublicUser :: User.User -> PublicUser
dbUserToPublicUser user =
    PublicUser
        { username = User.username (user :: User)
        , createdOn = User.createdOn (user :: User)
        }

auth :: (DB.Pipe -> PublicUser -> ActionM ()) -> DB.Pipe -> ActionM ()
auth endpoint pipe = do
    authHeader <- header "Authorization"

    let maybeUsername = headerToUsername =<< toStrict <$> drop 7 <$> authHeader

    case maybeUsername of
        Just username  -> do
            user <- liftIO $ DB.runQuery pipe $ DB.findUser username

            case user of
                Just u  -> endpoint pipe $ dbUserToPublicUser u
                Nothing -> status status401
        Nothing -> status status401

    pure undefined
