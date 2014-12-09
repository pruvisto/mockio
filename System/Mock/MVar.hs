{-# LANGUAGE NoImplicitPrelude #-}
module System.Mock.MVar (
    MVar, newMVar, newEmptyMVar, isEmptyMVar, tryTakeMVar, takeMVar, tryPutMVar, tryReadMVar, 
    readMVar, putMVar, swapMVar, modifyMVar, modifyMVar_,
  ) where
  
import System.Mock.IO.Internal

