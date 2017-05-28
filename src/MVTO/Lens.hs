
module MVTO.Lens
  ( is
  , ($.)
  ) where


import           Protolude

import           Control.Lens

is :: APrism s t a b -> s -> Bool
is k s = not (isn't k s)
{-# INLINE is #-}

infixl 8 $.
($.) :: Functor f => f s -> Getting a s a -> f a
s $. l = fmap (view l) s
{-# INLINE ($.) #-}

