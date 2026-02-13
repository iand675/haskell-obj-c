{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Caching options for a Unix Domain Socket.
--
-- This object configures Rosetta to communicate with the Rosetta daemon using a Unix Domain Socket.
--
-- See: VZLinuxRosettaCachingOptions
--
-- Generated bindings for @VZLinuxRosettaUnixSocketCachingOptions@.
module ObjC.Virtualization.VZLinuxRosettaUnixSocketCachingOptions
  ( VZLinuxRosettaUnixSocketCachingOptions
  , IsVZLinuxRosettaUnixSocketCachingOptions(..)
  , initWithPath_error
  , init_
  , path
  , maximumPathLength
  , initSelector
  , initWithPath_errorSelector
  , maximumPathLengthSelector
  , pathSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize options to be set on a VZLinuxRosettaDirectoryShare.
--
-- @path@ — The path of the Unix Domain Socket to be used to communicate with the Rosetta translation daemon. This cannot exceed maximumPathLength UTF-8 bytes long.
--
-- @error@ — If not nil, assigned with the error if the initialization failed.
--
-- Rosetta can be optionally configured to use cached translations from the Rosetta translation daemon communicating through a Unix Domain Socket.    If path exceeds maximumPathLength UTF-8 bytes, nil is returned and the error is set.    The guest operating system must have a directory at path created in order for translation caching to operate correctly.
--
-- ObjC selector: @- initWithPath:error:@
initWithPath_error :: (IsVZLinuxRosettaUnixSocketCachingOptions vzLinuxRosettaUnixSocketCachingOptions, IsNSString path, IsNSError error_) => vzLinuxRosettaUnixSocketCachingOptions -> path -> error_ -> IO (Id VZLinuxRosettaUnixSocketCachingOptions)
initWithPath_error vzLinuxRosettaUnixSocketCachingOptions path error_ =
  sendOwnedMessage vzLinuxRosettaUnixSocketCachingOptions initWithPath_errorSelector (toNSString path) (toNSError error_)

-- | Initialize default options to be set on a VZLinuxRosettaDirectoryShare.
--
-- The default translation caching configuration uses a Unix Domain Socket at /run/rosettad/rosetta.sock.
--
-- ObjC selector: @- init@
init_ :: IsVZLinuxRosettaUnixSocketCachingOptions vzLinuxRosettaUnixSocketCachingOptions => vzLinuxRosettaUnixSocketCachingOptions -> IO (Id VZLinuxRosettaUnixSocketCachingOptions)
init_ vzLinuxRosettaUnixSocketCachingOptions =
  sendOwnedMessage vzLinuxRosettaUnixSocketCachingOptions initSelector

-- | Path set by initWithPath.
--
-- This is the path of the Unix Domain Socket to be used by Rosetta.
--
-- ObjC selector: @- path@
path :: IsVZLinuxRosettaUnixSocketCachingOptions vzLinuxRosettaUnixSocketCachingOptions => vzLinuxRosettaUnixSocketCachingOptions -> IO (Id NSString)
path vzLinuxRosettaUnixSocketCachingOptions =
  sendMessage vzLinuxRosettaUnixSocketCachingOptions pathSelector

-- | The maximum allowed length of path, as defined by the sockaddr_un structure in Linux.
--
-- ObjC selector: @+ maximumPathLength@
maximumPathLength :: IO CULong
maximumPathLength  =
  do
    cls' <- getRequiredClass "VZLinuxRosettaUnixSocketCachingOptions"
    sendClassMessage cls' maximumPathLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPath:error:@
initWithPath_errorSelector :: Selector '[Id NSString, Id NSError] (Id VZLinuxRosettaUnixSocketCachingOptions)
initWithPath_errorSelector = mkSelector "initWithPath:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZLinuxRosettaUnixSocketCachingOptions)
initSelector = mkSelector "init"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSString)
pathSelector = mkSelector "path"

-- | @Selector@ for @maximumPathLength@
maximumPathLengthSelector :: Selector '[] CULong
maximumPathLengthSelector = mkSelector "maximumPathLength"

