
module MVTO.Lens
  ( is
  ) where


import           Protolude

import           Control.Lens

is :: APrism s t a b -> s -> Bool
is k s = not (isn't k s)
{-# INLINE is #-}

