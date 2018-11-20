module Feature.Login.Service
    ( findUserByCredentials
    , generateJwtToken
    ) where

import           Control.Monad          (mfilter)
import           Data.Aeson             (Value (..))
import           Data.Map               as M
import           Database.MongoDB       (Action)
import           Feature.Login.Types    (LoginBody (..), LoginError (..))
import qualified Feature.User.DB        as DB
import qualified Feature.User.Types     as User
import qualified Infrastructure.Crypto  as Crypto
import           Infrastructure.Secrets (jwtSecret)
import           Protolude
import           Web.JWT

findUserByCredentials :: LoginBody -> Action IO (Either LoginError User.User)
findUserByCredentials req = do
    user <- DB.findUser (username req)

    pure $ maybeToRight UserNotFound $ mfilter isPasswordValid user
        where
            isPasswordValid :: User.User -> Bool
            isPasswordValid u = Crypto.validate (User.password u) (password req)

generateJwtToken :: User.User -> Text
generateJwtToken user =
    let
        cs = def
            { iss = stringOrURI "listeo"
            , sub = stringOrURI (show $ User.username (user :: User.User))
            , unregisteredClaims = M.fromList [("http://localhost:1234", (Bool True))]
            }
        key = secret jwtSecret
    in encodeSigned HS256 key cs
