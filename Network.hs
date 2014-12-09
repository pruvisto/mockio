{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

module Network (
  N.withSocketsDo
, N.HostName
, N.PortID(..)
, N.PortNumber
, System.Mock.IO.Internal.connectTo
) where


import qualified "network" Network as N
import System.Mock.IO.Prelude
import System.Mock.IO.Internal

type HostName = N.HostName
type PortID = N.PortID
type PortNumber = N.PortNumber

