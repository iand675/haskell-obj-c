{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for an entropy device configuration.
--
-- VZEntropyDeviceConfiguration should not be instantiated directly.    The subclass VZVirtioEntropyDeviceConfiguration should be used instead.
--
-- See: VZVirtioEntropyDeviceConfiguration
--
-- Generated bindings for @VZEntropyDeviceConfiguration@.
module ObjC.Virtualization.VZEntropyDeviceConfiguration
  ( VZEntropyDeviceConfiguration
  , IsVZEntropyDeviceConfiguration(..)
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
new :: IO (Id VZEntropyDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZEntropyDeviceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZEntropyDeviceConfiguration vzEntropyDeviceConfiguration => vzEntropyDeviceConfiguration -> IO (Id VZEntropyDeviceConfiguration)
init_ vzEntropyDeviceConfiguration =
  sendOwnedMessage vzEntropyDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZEntropyDeviceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZEntropyDeviceConfiguration)
initSelector = mkSelector "init"

