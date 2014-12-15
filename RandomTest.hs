{-# LANGUAGE PackageImports #-}
import System.Random
import qualified "random" System.Random as GHCR
import qualified "base" System.IO as GHCIO
import System.Mock.IO.RealWorld

io :: IO [Int]
io =
  do g <- newStdGen
     return (take 10 (randomRs (0,100) g))

main = 
  do g <- GHCR.newStdGen
     let x = evalIO (setStdGen g >> io) emptyWorld
     GHCIO.print x

