module Infrastructure.Utils.JWT
  ( subjectFromHeader
  )
where

import Protolude hiding (drop)
import Data.Text.Lazy (drop)
import qualified Web.JWT as JWT

subjectFromHeader :: LText -> Maybe Text
subjectFromHeader header = do
  unverifiedJwt <- JWT.decode $ toStrict $ drop 7 $ header
  subject       <- JWT.sub $ JWT.claims unverifiedJwt

  return $ JWT.stringOrURIToText subject

