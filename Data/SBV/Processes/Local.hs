-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Processes.Local
-- Copyright   :  (c) Brian Schroeder, Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Local SMT solver process creation
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Data.SBV.Processes.Local
  ( start
  ) where

import qualified Control.Exception as C

import Control.Concurrent (newEmptyMVar, takeMVar, putMVar, forkIO)

import System.Directory   (findExecutable)
import System.Exit        (ExitCode(..))
import System.IO          (hClose, hFlush, hPutStrLn, hGetContents, hGetLine)
import System.Process     (runInteractiveProcess, waitForProcess, terminateProcess)

import Data.SBV.Core.Data
import Data.SBV.Core.Symbolic (SolverProcess(..))
import Data.SBV.Utils.Exception (handleAsync)

-- | Starts a 'SolverProcess' that finds the named executable on the PATH, and runs the solver locally.
start :: SMTConfig -> IO SolverProcess
start cfg = do
    let smtSolver  = solver cfg
        solverName = show (name smtSolver)
        execName   = executable smtSolver
        opts       = options smtSolver cfg

    mbExecPath <- findExecutable execName
    case mbExecPath of
        Nothing   -> error $ unlines [ "Unable to locate executable for " ++ solverName
                                     , "Executable specified: " ++ show execName
                                     ]

        Just path -> do (inh, outh, errh, pid) <- runInteractiveProcess path opts Nothing Nothing
                        pure $ SolverProcess
                            { writeLine = \s -> do hPutStrLn inh s
                                                   hFlush inh
                            , readLine  = hGetLine outh
                            , close     = do hClose inh
                                             outMVar <- newEmptyMVar
                                             out <- hGetContents outh `C.catch`  (\(e :: C.SomeException) -> handleAsync e (return (show e)))
                                             _ <- forkIO $ C.evaluate (length out) >> putMVar outMVar ()
                                             err <- hGetContents errh `C.catch`  (\(e :: C.SomeException) -> handleAsync e (return (show e)))
                                             _ <- forkIO $ C.evaluate (length err) >> putMVar outMVar ()
                                             takeMVar outMVar
                                             takeMVar outMVar
                                             hClose outh `C.catch`  (\(e :: C.SomeException) -> handleAsync e (return ()))
                                             hClose errh `C.catch`  (\(e :: C.SomeException) -> handleAsync e (return ()))
                                             ex <- waitForProcess pid `C.catch` (\(e :: C.SomeException) -> handleAsync e (return (ExitFailure (-999))))
                                             return (out, err, ex)
                            , terminate = terminateProcess pid
                            , await     = waitForProcess pid
                            }
