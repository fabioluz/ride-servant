module Prelude (module ClassyPrelude) where

-- | TODO: investigate conflicts for catchIO and \\
import ClassyPrelude hiding (catchIO, Handler, (\\))