{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Mock.IO.RealWorld (
    RealWorld (RealWorld, handles, files, workDir, isPermitted, nextHandle),
    Direction (In, Out),
    newWorld, emptyWorld, setUser, addServer, removeServer, listServers, runIO, evalIO,
    dumpHandle, getOpenHandles, wait,
    registerWriteHook, hookConsole, readConsoleHook, showConsoleHook, hookHandle, readHandleHook, showHandleHook
  ) where

import System.Mock.IO.Internal

