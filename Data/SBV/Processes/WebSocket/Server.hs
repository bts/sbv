-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Processes.WebSocket.Server
-- Copyright   :  (c) Brian Schroeder
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Server side of WebSocket-based solver process
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.SBV.Processes.WebSocket.Server
  ( runServer
  , mkWaiApp
  , mkWebSocketApp
  ) where

import qualified Control.Exception              as C
import qualified Network.HTTP.Types             as Web
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS

import Control.Concurrent (MVar, newMVar, modifyMVar_)
import Control.Exception  (finally)

import Data.SBV.Core.Symbolic (SolverProcess(..), SMTConfig)
import Data.SBV.SMT.Utils     (SBVException(..))

import Data.SBV.Processes.WebSocket.Types

-- | Convenience function for starting a local server
runServer :: String -> Int -> SMTConfig -> SolverProcess -> IO ()
runServer host port cfg process = WS.runServer host port =<< mkWebSocketApp cfg process

mkWaiApp :: SMTConfig -> SolverProcess -> IO Wai.Application
mkWaiApp cfg process = mkWaiApp' cfg process <$> mkServerState

mkWebSocketApp :: SMTConfig -> SolverProcess -> IO WS.ServerApp
mkWebSocketApp cfg process = mkWebSocketApp' cfg process <$> mkServerState

--

newtype ServerState = ServerState Int

mkServerState :: IO (MVar ServerState)
mkServerState = newMVar $ ServerState 0

mkWaiApp' :: SMTConfig -> SolverProcess -> MVar ServerState -> Wai.Application
mkWaiApp' cfg process state = WaiWS.websocketsOr WS.defaultConnectionOptions wsApp fallbackWaiApp
    where wsApp :: WS.ServerApp
          wsApp = mkWebSocketApp' cfg process state

          fallbackWaiApp :: Wai.Application
          fallbackWaiApp _req respond = respond $ Wai.responseLBS Web.status400 [] "Not a WebSocket request"

mkWebSocketApp' :: SMTConfig -> SolverProcess -> MVar ServerState -> WS.ServerApp
mkWebSocketApp' cfg process state = \pending -> do

    -- TODO: check the path and headers provided by the pending request.
    -- TODO: check against max number of connections

    modifyMVar_ state $ pure . \(ServerState i) -> ServerState (succ i)

    let handleException = do
            modifyMVar_ state $ pure . \(ServerState i) -> ServerState (pred i)
            terminate process

    flip finally handleException $ do
        conn <- WS.acceptRequest pending
        WS.forkPingThread conn 30

        let receiveData = do
                msg <- WS.receiveData conn
                case msg of
                    ReadOut                      -> do line <- readLine process
                                                       WS.sendTextData conn $ WriteOut line
                                                       receiveData

                    WriteIn body                 -> do writeLine process body
                                                       receiveData

                    CloseIn                      -> do (out, err, code) <- close process
                                                       WS.sendTextData conn $ WriteOut out
                                                       WS.sendTextData conn $ WriteErr err
                                                       WS.sendTextData conn $ Exit code

                    UnexpectedFromClient payload -> C.throwIO SBVException { sbvExceptionDescription = "WebSocket server received unexpected data from client"
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

        receiveData
