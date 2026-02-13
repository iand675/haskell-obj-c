{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZDirectorySharingDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZDirectorySharingDeviceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZDirectorySharingDeviceConfiguration vzDirectorySharingDeviceConfiguration => vzDirectorySharingDeviceConfiguration -> IO (Id VZDirectorySharingDeviceConfiguration)
init_ vzDirectorySharingDeviceConfiguration =
  sendOwnedMessage vzDirectorySharingDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZDirectorySharingDeviceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZDirectorySharingDeviceConfiguration)
initSelector = mkSelector "init"

