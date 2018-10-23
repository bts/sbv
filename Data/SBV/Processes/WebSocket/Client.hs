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

{-# LANGUAGE LambdaCase #-}

module Data.SBV.Processes.WebSocket.Client
  ( startClient
  ) where

import qualified Control.Exception  as C
import qualified Network.WebSockets as WS

import Control.Concurrent      (forkFinally, forkIO, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Monad           (forever, void)
import Control.Monad.Trans     (liftIO)
import Network.Socket          (withSocketsDo)
import System.Exit             (ExitCode(..))

import Data.SBV.Core.Symbolic (SolverProcess(..), SMTConfig)
import Data.SBV.SMT.Utils     (SBVException(..))

import Data.SBV.Processes.WebSocket.Types


data Termination = ExitedCleanly String ExitCode
                 | Aborted C.SomeException

startClient :: String -> Int -> String -> SMTConfig -> IO SolverProcess
startClient host port path cfg = do
    readRequest    <- newEmptyMVar :: IO (MVar ())          -- fulfilled many times
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
                              WriteOut line                -> do liftIO $ putMVar outputBuffer line
                                                                 receiveData

                              WriteErr line                -> do liftIO $ modifyMVar_ accumulatedErr $ pure . (line :)
                                                                 receiveData

                              Exit code                    -> do err <- modifyMVar accumulatedErr $ \acc -> pure ([], acc)
                                                                 putMVar termination $ ExitedCleanly (unlines (reverse err)) code

                              UnexpectedFromServer payload -> C.throwIO SBVException { sbvExceptionDescription = "WebSocket client received unexpected data from server"
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
                      line <- takeMVar inputBuffer
                      WS.sendTextData conn $ WriteIn line

                  void $ forkIO $ forever $ do
                      () <- takeMVar readRequest
                      WS.sendTextData conn ReadOut

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

    tid <- forkFinally run $ \case
               Left e  -> void $ tryPutMVar termination $ Aborted e
               Right _ -> pure ()

    pure $ SolverProcess
        { writeLine = putMVar inputBuffer

        , readLine  = do putMVar readRequest ()
                         takeMVar outputBuffer

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
