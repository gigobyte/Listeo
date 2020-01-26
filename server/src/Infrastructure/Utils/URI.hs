module Infrastructure.Utils.URI where

import Protolude
import qualified Data.Text as T

parseQueryString :: Text -> [(Text, Text)]
parseQueryString query =
  let
    queryWithoutQuestionMark =
      if T.isPrefixOf "?" query then T.drop 1 query else query
    params = T.splitOn "&" queryWithoutQuestionMark
  in (\(k, v) -> (k, T.drop 1 v)) <$> T.breakOn "=" <$> params
