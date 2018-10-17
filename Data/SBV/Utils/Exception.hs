-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Utils.Exception
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Various helpers for working with exceptions
-----------------------------------------------------------------------------

module Data.SBV.Utils.Exception
  ( handleAsync
  ) where

import qualified Control.Exception as C

import Data.Maybe (isJust)

-- We should not be catching/processing asynchronous exceptions.
-- See http://github.com/LeventErkok/sbv/issues/410
handleAsync :: C.SomeException -> IO a -> IO a
handleAsync e cont
  | isAsynchronous = C.throwIO e
  | True           = cont
  where -- Stealing this definition from the asynchronous exceptions package to reduce dependencies
        isAsynchronous :: Bool
        isAsynchronous = isJust (C.fromException e :: Maybe C.AsyncException) || isJust (C.fromException e :: Maybe C.SomeAsyncException)
