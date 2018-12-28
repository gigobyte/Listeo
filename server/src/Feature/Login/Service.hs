module Feature.Login.Service
  ( findUserByCredentials
  , generateJwtToken
  )
where

import Protolude
import Flow
import Control.Monad (mfilter)
import Feature.Login.Models.LoginBody (LoginBody)
import Feature.Login.Models.LoginResponse (LoginError(..))
import Feature.User.Models.User (User)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Database.MongoDB as MongoDB
import qualified Feature.Login.Models.LoginBody as LoginBody
import qualified Feature.User.DB as DB
import qualified Feature.User.Models.User as User
import qualified Infrastructure.Crypto as Crypto
import qualified Infrastructure.Secrets as Secrets
import qualified Web.JWT as JWT

findUserByCredentials :: LoginBody -> MongoDB.Action IO (Either LoginError User)
findUserByCredentials req = do
  userInDb <- DB.findUser (LoginBody.username req)

  mfilter isPasswordValid userInDb |> maybeToRight UserNotFound |> pure
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
