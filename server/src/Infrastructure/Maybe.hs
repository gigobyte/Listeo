module Infrastructure.Maybe
    ( maybeToEither
    ) where

import           Protolude hiding (maybeToEither)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a
