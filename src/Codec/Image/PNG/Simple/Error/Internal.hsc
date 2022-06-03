{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , UndecidableInstances #-}

{-# OPTIONS_HADDOCK not_home #-}

module Codec.Image.PNG.Simple.Error.Internal where

import           Codec.Image.PNG.Simple.Internal

import           Control.Exception
import           Control.Monad (when)
import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe
import           Data.Int
import           Foreign.Ptr
import           System.IO.Unsafe

#include "spng.h"

-- | Exception thrown by functions in this library.
--   The 'ByteString' is the corresponding 'spng_strerror'.
data SpngError = SpngError SpngErrNo ByteString
                 deriving Show

instance Exception SpngError



{-# NOINLINE spng_strerror #-}
spng_strerror :: SpngErrNo -> ByteString
spng_strerror (SpngErrNo err) = unsafePerformIO $ do ver <- spng_strerror' err
                                                     unsafePackCString $ castPtr ver


class Wrap f a | f -> a where
  wrap :: (#{type int} -> IO ()) -> f -> a

instance Wrap (IO #{type int}) (IO ()) where
  wrap g f = g =<< f

instance Wrap b c => Wrap (a -> b) (a -> c) where
  wrap g f a = wrap g $ f a



-- | Consumes the returned integer from an 'IO' action and
--   throws a 'SpngError' if it's not equal to 'SPNG_OK'. Lifts all the arguments.
wrapError :: Wrap f a => f -> a
wrapError = wrap $ \raw -> let res = SpngErrNo raw
                           in when (res /= SPNG_OK) $
                                throw $ SpngError res (spng_strerror res)
