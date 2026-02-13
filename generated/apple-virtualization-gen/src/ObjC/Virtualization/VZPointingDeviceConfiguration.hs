{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a pointing device configuration.
--
-- VZPointingDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZUSBScreenCoordinatePointingDeviceConfiguration or VZMacTrackpadConfiguration should be used instead.
--
-- See: VZUSBScreenCoordinatePointingDeviceConfiguration
--
-- See: VZMacTrackpadConfiguration
--
-- Generated bindings for @VZPointingDeviceConfiguration@.
module ObjC.Virtualization.VZPointingDeviceConfiguration
  ( VZPointingDeviceConfiguration
  , IsVZPointingDeviceConfiguration(..)
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
new :: IO (Id VZPointingDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZPointingDeviceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZPointingDeviceConfiguration vzPointingDeviceConfiguration => vzPointingDeviceConfiguration -> IO (Id VZPointingDeviceConfiguration)
init_ vzPointingDeviceConfiguration =
  sendOwnedMessage vzPointingDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZPointingDeviceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZPointingDeviceConfiguration)
initSelector = mkSelector "init"

