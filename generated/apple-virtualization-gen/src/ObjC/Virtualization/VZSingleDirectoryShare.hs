{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Directory share for a single directory.
--
-- This directory share exposes a single directory from the host file system to the guest.
--
-- See: VZDirectorySharingDeviceConfiguration
--
-- See: VZSharedDirectory
--
-- Generated bindings for @VZSingleDirectoryShare@.
module ObjC.Virtualization.VZSingleDirectoryShare
  ( VZSingleDirectoryShare
  , IsVZSingleDirectoryShare(..)
  , initWithDirectory
  , directory
  , directorySelector
  , initWithDirectorySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the directory share with a directory on the host.
--
-- @directory@ — Directory to share.
--
-- ObjC selector: @- initWithDirectory:@
initWithDirectory :: (IsVZSingleDirectoryShare vzSingleDirectoryShare, IsVZSharedDirectory directory) => vzSingleDirectoryShare -> directory -> IO (Id VZSingleDirectoryShare)
initWithDirectory vzSingleDirectoryShare directory =
  sendOwnedMessage vzSingleDirectoryShare initWithDirectorySelector (toVZSharedDirectory directory)

-- | Directory on the host to share.
--
-- ObjC selector: @- directory@
directory :: IsVZSingleDirectoryShare vzSingleDirectoryShare => vzSingleDirectoryShare -> IO (Id VZSharedDirectory)
directory vzSingleDirectoryShare =
  sendMessage vzSingleDirectoryShare directorySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDirectory:@
initWithDirectorySelector :: Selector '[Id VZSharedDirectory] (Id VZSingleDirectoryShare)
initWithDirectorySelector = mkSelector "initWithDirectory:"

-- | @Selector@ for @directory@
directorySelector :: Selector '[] (Id VZSharedDirectory)
directorySelector = mkSelector "directory"

