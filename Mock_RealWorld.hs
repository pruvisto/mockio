{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Mock.IO.RealWorld (
    RealWorld (RealWorld, handles, files, workDir, isPermitted, nextHandle),
    Direction (In, Out), HandleHook, ConsoleHook,
    newWorld, emptyWorld, setUser, addServer, removeServer, listServers, runIO, evalIO, tryRunIO, tryEvalIO,
    dumpHandle, getOpenHandles, wait,
    registerWriteHook, hookConsole, readConsoleHook, showConsoleHook, hookHandle, readHandleHook, showHandleHook,
    tryIO, catchIO
  ) where

import System.Mock.IO.Internal

