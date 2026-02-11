{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a directory sharing device configuration.
--
-- VZDirectorySharingDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioFileSystemDeviceConfiguration should be used instead.
--
-- See: VZVirtioFileSystemDeviceConfiguration
--
-- Generated bindings for @VZDirectorySharingDeviceConfiguration@.
module ObjC.Virtualization.VZDirectorySharingDeviceConfiguration
  ( VZDirectorySharingDeviceConfiguration
  , IsVZDirectorySharingDeviceConfiguration(..)
  , new
  , init_
  , newSelector
  , initSelector


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

-- | @+ new@
new :: IO (Id VZDirectorySharingDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZDirectorySharingDeviceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZDirectorySharingDeviceConfiguration vzDirectorySharingDeviceConfiguration => vzDirectorySharingDeviceConfiguration -> IO (Id VZDirectorySharingDeviceConfiguration)
init_ vzDirectorySharingDeviceConfiguration  =
  sendMsg vzDirectorySharingDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

