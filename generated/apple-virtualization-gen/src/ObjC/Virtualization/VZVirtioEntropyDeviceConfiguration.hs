{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Entropy Device
--
-- The device exposes a source of entropy for the guest's random number generator.
--
-- Generated bindings for @VZVirtioEntropyDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioEntropyDeviceConfiguration
  ( VZVirtioEntropyDeviceConfiguration
  , IsVZVirtioEntropyDeviceConfiguration(..)
  , init_
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZVirtioEntropyDeviceConfiguration vzVirtioEntropyDeviceConfiguration => vzVirtioEntropyDeviceConfiguration -> IO (Id VZVirtioEntropyDeviceConfiguration)
init_ vzVirtioEntropyDeviceConfiguration =
  sendOwnedMessage vzVirtioEntropyDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioEntropyDeviceConfiguration)
initSelector = mkSelector "init"

