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
  , initWithDirectorySelector
  , directorySelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
initWithDirectory vzSingleDirectoryShare  directory =
withObjCPtr directory $ \raw_directory ->
    sendMsg vzSingleDirectoryShare (mkSelector "initWithDirectory:") (retPtr retVoid) [argPtr (castPtr raw_directory :: Ptr ())] >>= ownedObject . castPtr

-- | Directory on the host to share.
--
-- ObjC selector: @- directory@
directory :: IsVZSingleDirectoryShare vzSingleDirectoryShare => vzSingleDirectoryShare -> IO (Id VZSharedDirectory)
directory vzSingleDirectoryShare  =
  sendMsg vzSingleDirectoryShare (mkSelector "directory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDirectory:@
initWithDirectorySelector :: Selector
initWithDirectorySelector = mkSelector "initWithDirectory:"

-- | @Selector@ for @directory@
directorySelector :: Selector
directorySelector = mkSelector "directory"

