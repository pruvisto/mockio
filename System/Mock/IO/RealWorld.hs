{-# LANGUAGE NoImplicitPrelude #-}
module System.Mock.IO.RealWorld (
    RealWorld (RealWorld, handles, files, workDir, isPermitted, nextHandle),
    User, Server, Direction (In, Out),
    newWorld, emptyWorld, mkUser, setUser, mkServer, addServer, removeServer, listServers, runIO, evalIO,
    dumpHandle, usrStdin, usrStdout, usrStderr, reverseHandle, getOpenHandles
  ) where

import System.Mock.IO.Internal
