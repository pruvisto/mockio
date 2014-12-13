{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PackageImports #-}

module Network (
  N.HostName
, N.PortNumber
, N.PortID(..)
, connectTo
, withSocketsDo
) where

import qualified "network" Network as N
import System.Mock.IO.Internal
