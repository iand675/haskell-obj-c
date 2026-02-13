{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a GPU compute device.
--
-- Generated bindings for @MLGPUComputeDevice@.
module ObjC.CoreML.MLGPUComputeDevice
  ( MLGPUComputeDevice
  , IsMLGPUComputeDevice(..)
  , init_
  , new
  , metalDevice
  , initSelector
  , metalDeviceSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLGPUComputeDevice mlgpuComputeDevice => mlgpuComputeDevice -> IO (Id MLGPUComputeDevice)
init_ mlgpuComputeDevice =
  sendOwnedMessage mlgpuComputeDevice initSelector

-- | @+ new@
new :: IO (Id MLGPUComputeDevice)
new  =
  do
    cls' <- getRequiredClass "MLGPUComputeDevice"
    sendOwnedClassMessage cls' newSelector

-- | The underlying metal device.
--
-- ObjC selector: @- metalDevice@
metalDevice :: IsMLGPUComputeDevice mlgpuComputeDevice => mlgpuComputeDevice -> IO RawId
metalDevice mlgpuComputeDevice =
  sendMessage mlgpuComputeDevice metalDeviceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLGPUComputeDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLGPUComputeDevice)
newSelector = mkSelector "new"

-- | @Selector@ for @metalDevice@
metalDeviceSelector :: Selector '[] RawId
metalDeviceSelector = mkSelector "metalDevice"

