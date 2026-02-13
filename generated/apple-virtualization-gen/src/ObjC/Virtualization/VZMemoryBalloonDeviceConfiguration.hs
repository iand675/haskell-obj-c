{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a memory balloon device configuration.
--
-- VZMemoryBalloonDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioTraditionalMemoryBalloonDeviceConfiguration should be used instead.
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
--
-- Generated bindings for @VZMemoryBalloonDeviceConfiguration@.
module ObjC.Virtualization.VZMemoryBalloonDeviceConfiguration
  ( VZMemoryBalloonDeviceConfiguration
  , IsVZMemoryBalloonDeviceConfiguration(..)
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
new :: IO (Id VZMemoryBalloonDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZMemoryBalloonDeviceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZMemoryBalloonDeviceConfiguration vzMemoryBalloonDeviceConfiguration => vzMemoryBalloonDeviceConfiguration -> IO (Id VZMemoryBalloonDeviceConfiguration)
init_ vzMemoryBalloonDeviceConfiguration =
  sendOwnedMessage vzMemoryBalloonDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZMemoryBalloonDeviceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMemoryBalloonDeviceConfiguration)
initSelector = mkSelector "init"

