-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Processes.WebSocket.Types
-- Copyright   :  (c) Brian Schroeder
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Types shared between WebSocket client and server implementations
-----------------------------------------------------------------------------

module Data.SBV.Processes.WebSocket.Types where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.WebSockets         as WS

import Safe        (readMay)
import System.Exit (ExitCode(..))

data ToClient = StdOut String
              | StdErr String
              | Exit ExitCode
              | UnexpectedFromServer String

data ToServer = StdIn String
              | CloseIn
              | UnexpectedFromClient String

instance WS.WebSocketsData ToClient where
  fromDataMessage (WS.Text lbs _) = WS.fromLazyByteString lbs
  fromDataMessage (WS.Binary lbs) = WS.fromLazyByteString lbs

  fromLazyByteString lbs = case LBS.unpack lbs of
                               'o':'u':'t':',':body     -> StdOut body
                               'e':'r':'r':',':body     -> StdErr body
                               'e':'x':'i':'t':',':body -> case readMay body of
                                                               Just 0    -> Exit ExitSuccess
                                                               Just code -> Exit $ ExitFailure code
                                                               Nothing   -> Exit $ ExitFailure (-50)
                               unexpected               -> UnexpectedFromServer unexpected

  toLazyByteString (StdOut body)            = LBS.pack $ "out," ++ body
  toLazyByteString (StdErr body)            = LBS.pack $ "err," ++ body
  toLazyByteString (Exit code)              = LBS.pack $ "exit," ++ show code
  toLazyByteString (UnexpectedFromServer _) = error "impossible: encoding UnexpectedFromServer"

instance WS.WebSocketsData ToServer where
  fromDataMessage (WS.Text lbs _) = WS.fromLazyByteString lbs
  fromDataMessage (WS.Binary lbs) = WS.fromLazyByteString lbs

  fromLazyByteString lbs = case LBS.unpack lbs of
                               'i':'n':',':body -> StdIn body
                               "close,"         -> CloseIn
                               unexpected       -> UnexpectedFromClient unexpected

  toLazyByteString (StdIn body)             = LBS.pack $ "in," ++ body
  toLazyByteString CloseIn                  = LBS.pack "close,"
  toLazyByteString (UnexpectedFromClient _) = error "impossible: encoding UnexpectedFromClient"

