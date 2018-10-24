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

  , test -- TODO: move into test code

  ) where

--
-- TODO
--
-- [ ] commit pending stuff
-- [ ] add tests for serverside-only WS interactions.
--        decide whether to use dummy or real z3 backend
-- [ ] potentially consolidate WriteErr and Exit (might simplify possible reuse for ghcjs)
-- [ ] write ghcjs SolverProcess
-- [ ] test ghcjs SolverProcess
-- [ ] move stuff into new package(s)
--

import qualified Control.Exception              as C
import qualified Network.HTTP.Types             as Web
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS
import qualified Data.SBV.Processes.Local       as LocalProcess
import qualified Data.SBV.Provers.Prover        as Prover

import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_)
import Control.Exception  (finally)

import Data.SBV.Core.Symbolic (SolverProcess(..), SMTConfig)
import Data.SBV.SMT.Utils     (SBVException(..))

import Data.SBV.Processes.WebSocket.Types

-- TODO: move this to test code
test :: IO ()
test = runServer "127.0.0.1" 3000 10 cfg =<< LocalProcess.start cfg
  where
    cfg = Prover.z3

-- | Convenience function for starting a local server
runServer :: String -> Int -> Int -> SMTConfig -> SolverProcess -> IO ()
runServer host port maxConns cfg process = WS.runServer host port =<< mkWebSocketApp maxConns cfg process

mkWaiApp :: Int -> SMTConfig -> SolverProcess -> IO Wai.Application
mkWaiApp maxConns cfg process = mkWaiApp' maxConns cfg process <$> mkServerState

mkWebSocketApp :: Int -> SMTConfig -> SolverProcess -> IO WS.ServerApp
mkWebSocketApp maxConns cfg process = mkWebSocketApp' maxConns cfg process <$> mkServerState

--

newtype ServerState = ServerState Int

mkServerState :: IO (MVar ServerState)
mkServerState = newMVar $ ServerState 0

mkWaiApp' :: Int -> SMTConfig -> SolverProcess -> MVar ServerState -> Wai.Application
mkWaiApp' maxConns cfg process state = WaiWS.websocketsOr WS.defaultConnectionOptions wsApp fallbackWaiApp
    where wsApp :: WS.ServerApp
          wsApp = mkWebSocketApp' maxConns cfg process state

          fallbackWaiApp :: Wai.Application
          fallbackWaiApp _req respond = respond $ Wai.responseLBS Web.status400 [] "Not a WebSocket request"

mkWebSocketApp' :: Int -> SMTConfig -> SolverProcess -> MVar ServerState -> WS.ServerApp
mkWebSocketApp' maxConns cfg process state = \pending -> do
    permitted <- modifyMVar state $ pure . \(ServerState numClients) ->
      if numClients >= maxConns
      then (ServerState numClients, False)
      else (ServerState (succ numClients), True)

    if not permitted
    then do
      WS.rejectRequestWith pending $
          WS.defaultRejectRequest { WS.rejectCode    = 503
                                  , WS.rejectMessage = "Service Unavailable"
                                  }
    else do
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
