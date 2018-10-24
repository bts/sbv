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

data ToClient = WriteOut String

              --
              -- TODO: combine? would involve making "exit" parsing more involved.
              --
              | WriteErr String
              | Exit ExitCode

              | UnexpectedFromServer String
    deriving Show

data ToServer = ReadOut
              | WriteIn String
              | CloseIn
              | UnexpectedFromClient String
    deriving Show

instance WS.WebSocketsData ToClient where
  fromDataMessage (WS.Text lbs _) = WS.fromLazyByteString lbs
  fromDataMessage (WS.Binary lbs) = WS.fromLazyByteString lbs

  fromLazyByteString lbs = case LBS.unpack lbs of
                               'o':'u':'t':',':body           -> WriteOut body
                               'e':'r':'r':',':body           -> WriteErr body
                               msg@('e':'x':'i':'t':',':body) -> case readMay body of
                                                                     Just 0    -> Exit ExitSuccess
                                                                     Just code -> Exit $ ExitFailure code
                                                                     Nothing   -> UnexpectedFromServer msg
                               unexpected                     -> UnexpectedFromServer unexpected

  toLazyByteString (WriteOut body)          = LBS.pack $ "out," ++ body
  toLazyByteString (WriteErr body)          = LBS.pack $ "err," ++ body
  toLazyByteString (Exit code)              = LBS.pack $ "exit," ++ case code of
                                                                      ExitFailure num -> show num
                                                                      ExitSuccess     -> "0"
  toLazyByteString (UnexpectedFromServer _) = error "impossible: encoding UnexpectedFromServer"

instance WS.WebSocketsData ToServer where
  fromDataMessage (WS.Text lbs _) = WS.fromLazyByteString lbs
  fromDataMessage (WS.Binary lbs) = WS.fromLazyByteString lbs

  fromLazyByteString lbs = case LBS.unpack lbs of
                               "read,"          -> ReadOut
                               'i':'n':',':body -> WriteIn body
                               "close,"         -> CloseIn
                               unexpected       -> UnexpectedFromClient unexpected

  toLazyByteString ReadOut                  = LBS.pack "read,"
  toLazyByteString (WriteIn body)           = LBS.pack $ "in," ++ body
  toLazyByteString CloseIn                  = LBS.pack "close,"
  toLazyByteString (UnexpectedFromClient _) = error "impossible: encoding UnexpectedFromClient"

