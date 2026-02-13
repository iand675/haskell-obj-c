{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The anticipated compute devices that would be used for executing a layer/operation.
--
-- Generated bindings for @MLComputePlanDeviceUsage@.
module ObjC.CoreML.MLComputePlanDeviceUsage
  ( MLComputePlanDeviceUsage
  , IsMLComputePlanDeviceUsage(..)
  , init_
  , new
  , supportedComputeDevices
  , preferredComputeDevice
  , initSelector
  , newSelector
  , preferredComputeDeviceSelector
  , supportedComputeDevicesSelector


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
init_ :: IsMLComputePlanDeviceUsage mlComputePlanDeviceUsage => mlComputePlanDeviceUsage -> IO (Id MLComputePlanDeviceUsage)
init_ mlComputePlanDeviceUsage =
  sendOwnedMessage mlComputePlanDeviceUsage initSelector

-- | @+ new@
new :: IO (Id MLComputePlanDeviceUsage)
new  =
  do
    cls' <- getRequiredClass "MLComputePlanDeviceUsage"
    sendOwnedClassMessage cls' newSelector

-- | The compute devices that can execute the layer/operation.
--
-- ObjC selector: @- supportedComputeDevices@
supportedComputeDevices :: IsMLComputePlanDeviceUsage mlComputePlanDeviceUsage => mlComputePlanDeviceUsage -> IO (Id NSArray)
supportedComputeDevices mlComputePlanDeviceUsage =
  sendMessage mlComputePlanDeviceUsage supportedComputeDevicesSelector

-- | The compute device that the framework prefers to execute the layer/operation.
--
-- ObjC selector: @- preferredComputeDevice@
preferredComputeDevice :: IsMLComputePlanDeviceUsage mlComputePlanDeviceUsage => mlComputePlanDeviceUsage -> IO RawId
preferredComputeDevice mlComputePlanDeviceUsage =
  sendMessage mlComputePlanDeviceUsage preferredComputeDeviceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLComputePlanDeviceUsage)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLComputePlanDeviceUsage)
newSelector = mkSelector "new"

-- | @Selector@ for @supportedComputeDevices@
supportedComputeDevicesSelector :: Selector '[] (Id NSArray)
supportedComputeDevicesSelector = mkSelector "supportedComputeDevices"

-- | @Selector@ for @preferredComputeDevice@
preferredComputeDeviceSelector :: Selector '[] RawId
preferredComputeDeviceSelector = mkSelector "preferredComputeDevice"

