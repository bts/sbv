-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Processes.WebSocket.Client
-- Copyright   :  (c) Brian Schroeder
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Client side of WebSocket-based solver process
-----------------------------------------------------------------------------

module Data.SBV.Processes.WebSocket.Client
  ( startClient
  ) where

import qualified Control.Exception          as C
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.WebSockets         as WS

import Control.Concurrent      (forkFinally, forkIO, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Monad           (forever, void)
import Control.Monad.Trans     (liftIO)
import Network.Socket          (withSocketsDo)
import Safe                    (readMay)
import System.Exit             (ExitCode(..))

import Data.SBV.Core.Symbolic (SolverProcess(..), SMTConfig)
import Data.SBV.SMT.Utils     (SBVException(..))

data Receive = StdOut String
             | StdErr String
             | Exit ExitCode
             | Unexpected String

data Send = StdIn String
          | CloseIn

data Termination = ExitedCleanly String ExitCode
                 | Aborted C.SomeException

instance WS.WebSocketsData Receive where
  fromDataMessage (WS.Text lbs _) = WS.fromLazyByteString lbs
  fromDataMessage (WS.Binary lbs) = WS.fromLazyByteString lbs

  fromLazyByteString lbs = case LBS.unpack lbs of
                               'o':'u':'t':',':body     -> StdOut body
                               'e':'r':'r':',':body     -> StdErr body
                               'e':'x':'i':'t':',':body -> case readMay body of
                                                               Just 0    -> Exit ExitSuccess
                                                               Just code -> Exit $ ExitFailure code
                                                               Nothing   -> Exit $ ExitFailure (-50)
                               unexpected               -> Unexpected unexpected

  toLazyByteString _ = LBS.empty -- NB: we never encode "received" messages

instance WS.WebSocketsData Send where
  fromDataMessage (WS.Text lbs _) = WS.fromLazyByteString lbs
  fromDataMessage (WS.Binary lbs) = WS.fromLazyByteString lbs

  fromLazyByteString _ = StdIn "" -- NB: we never decode "sent" messages

  toLazyByteString (StdIn body) = LBS.pack $ "in," ++ body
  toLazyByteString CloseIn      = LBS.pack "close,"

startClient :: SMTConfig -> String -> Int -> String -> Int -> IO SolverProcess
startClient cfg host port path pingTimeout = do
    closeRequest   <- newEmptyMVar :: IO (MVar ())          -- only fulfilled once
    termination    <- newEmptyMVar :: IO (MVar Termination) -- only fulfilled once
    inputBuffer    <- newEmptyMVar :: IO (MVar String)      -- single-place buffer
    outputBuffer   <- newEmptyMVar :: IO (MVar String)      -- single-place buffer
    accumulatedErr <- newMVar []   :: IO (MVar [String])    -- reversed list

    let run :: IO ()
        run = withSocketsDo $ WS.runClient host port path $ \conn -> do
                  let receiveData = do
                          msg <- WS.receiveData conn
                          case msg of
                              StdOut line        -> do liftIO $ putMVar outputBuffer line
                                                       receiveData

                              StdErr line        -> do liftIO $ modifyMVar_ accumulatedErr $ pure . (line :)
                                                       receiveData

                              Exit code          -> do err <- modifyMVar accumulatedErr $ \acc -> pure ([], acc)
                                                       putMVar termination $ ExitedCleanly (unlines (reverse err)) code

                              Unexpected payload -> C.throwIO SBVException { sbvExceptionDescription = "WebSocket client received unexpected data"
                                                                           , sbvExceptionSent        = Nothing
                                                                           , sbvExceptionExpected    = Nothing
                                                                           , sbvExceptionReceived    = Just payload
                                                                           , sbvExceptionStdOut      = Nothing
                                                                           , sbvExceptionStdErr      = Nothing
                                                                           , sbvExceptionExitCode    = Nothing
                                                                           , sbvExceptionConfig      = cfg
                                                                           , sbvExceptionReason      = Nothing
                                                                           , sbvExceptionHint        = Nothing
                                                                           }

                  _ <- forkIO receiveData

                  void $ forkIO $ forever $ do
                      line <- readMVar inputBuffer
                      WS.sendTextData conn $ StdIn line

                  void $ readMVar closeRequest
                  WS.sendTextData conn $ CloseIn
                  void $ readMVar termination

        awaitCleanExit :: IO (String, String, ExitCode)
        awaitCleanExit = do
            term <- readMVar termination
            case term of
              ExitedCleanly err code -> pure ("", err, code)
              Aborted e              -> C.throwIO SBVException { sbvExceptionDescription = "WebSocket solver aborted while waiting for close: " ++ show e
                                                               , sbvExceptionSent        = Nothing
                                                               , sbvExceptionExpected    = Nothing
                                                               , sbvExceptionReceived    = Nothing
                                                               , sbvExceptionStdOut      = Nothing
                                                               , sbvExceptionStdErr      = Nothing
                                                               , sbvExceptionExitCode    = Nothing
                                                               , sbvExceptionConfig      = cfg
                                                               , sbvExceptionReason      = Nothing
                                                               , sbvExceptionHint        = Nothing
                                                               }

    tid <- forkFinally run $ \eResult -> case eResult of
                                           Left e -> void $ tryPutMVar termination $ Aborted e
                                           Right _ -> pure ()

    pure $ SolverProcess
        { writeLine = putMVar inputBuffer
        , readLine  = takeMVar outputBuffer
        , close     = do res <- tryPutMVar closeRequest ()
                         case res of
                           False -> C.throwIO SBVException { sbvExceptionDescription = "Tried to close solver twice"
                                                           , sbvExceptionSent        = Nothing
                                                           , sbvExceptionExpected    = Nothing
                                                           , sbvExceptionReceived    = Nothing
                                                           , sbvExceptionStdOut      = Nothing
                                                           , sbvExceptionStdErr      = Nothing
                                                           , sbvExceptionExitCode    = Nothing
                                                           , sbvExceptionConfig      = cfg
                                                           , sbvExceptionReason      = Nothing
                                                           , sbvExceptionHint        = Nothing
                                                           }
                           True  -> awaitCleanExit
        , terminate = killThread tid
        , await     = (\(_,_,code) -> code) <$> awaitCleanExit
        }
