{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCDevice
--
-- A device that will be used to execute a neural network.                If a MLCdevice is created with multiple devices using the [devicesWithType:selectMultipleDvices], on configurations                where multiple GPUs are available such as on the Mac Pro, the framework may transparently schedule the execution                across multiple GPUs.  There are some requirements for a MLCDevice with multiple devices such as there can only be                one training and/or inference graph associated with this device.  If multiple graphs are used, they must be compiled using                MLCGraphCompilationOptionsLinkGraphs specified in compileOptions and the multiple graphs should be linked together                with linkWithGraphs.
--
-- Generated bindings for @MLCDevice@.
module ObjC.MLCompute.MLCDevice
  ( MLCDevice
  , IsMLCDevice(..)
  , cpuDevice
  , gpuDevice
  , aneDevice
  , deviceWithType
  , deviceWithType_selectsMultipleComputeDevices
  , deviceWithGPUDevices
  , type_
  , actualDeviceType
  , gpuDevices
  , actualDeviceTypeSelector
  , aneDeviceSelector
  , cpuDeviceSelector
  , deviceWithGPUDevicesSelector
  , deviceWithTypeSelector
  , deviceWithType_selectsMultipleComputeDevicesSelector
  , gpuDeviceSelector
  , gpuDevicesSelector
  , typeSelector

  -- * Enum types
  , MLCDeviceType(MLCDeviceType)
  , pattern MLCDeviceTypeCPU
  , pattern MLCDeviceTypeGPU
  , pattern MLCDeviceTypeAny
  , pattern MLCDeviceTypeANE
  , pattern MLCDeviceTypeCount

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a device which uses the CPU.
--
-- Returns: A new device.
--
-- ObjC selector: @+ cpuDevice@
cpuDevice :: IO (Id MLCDevice)
cpuDevice  =
  do
    cls' <- getRequiredClass "MLCDevice"
    sendClassMessage cls' cpuDeviceSelector

-- | Creates a device which uses a GPU, if any.
--
-- Returns: A new device, or @nil@ if no GPU exists.
--
-- ObjC selector: @+ gpuDevice@
gpuDevice :: IO (Id MLCDevice)
gpuDevice  =
  do
    cls' <- getRequiredClass "MLCDevice"
    sendClassMessage cls' gpuDeviceSelector

-- | Creates a device which uses the Apple Neural Engine, if any.
--
-- Returns: A new device, or @nil@ if no ANE exists.
--
-- ObjC selector: @+ aneDevice@
aneDevice :: IO (Id MLCDevice)
aneDevice  =
  do
    cls' <- getRequiredClass "MLCDevice"
    sendClassMessage cls' aneDeviceSelector

-- | Create a MLCDevice object
--
-- @type@ — A device type
--
-- Returns: A new device object
--
-- ObjC selector: @+ deviceWithType:@
deviceWithType :: MLCDeviceType -> IO (Id MLCDevice)
deviceWithType type_ =
  do
    cls' <- getRequiredClass "MLCDevice"
    sendClassMessage cls' deviceWithTypeSelector type_

-- | Create a MLCDevice object that uses multiple devices if available
--
-- @type@ — A device type
--
-- @selectsMultipleComputeDevices@ — A boolean to indicate whether to select multiple compute devices
--
-- Returns: A new device object
--
-- ObjC selector: @+ deviceWithType:selectsMultipleComputeDevices:@
deviceWithType_selectsMultipleComputeDevices :: MLCDeviceType -> Bool -> IO (Id MLCDevice)
deviceWithType_selectsMultipleComputeDevices type_ selectsMultipleComputeDevices =
  do
    cls' <- getRequiredClass "MLCDevice"
    sendClassMessage cls' deviceWithType_selectsMultipleComputeDevicesSelector type_ selectsMultipleComputeDevices

-- | Create a MLCDevice object
--
-- This method can be used by developers to select specific GPUs
--
-- @gpus@ — List of Metal devices
--
-- Returns: A new device object
--
-- ObjC selector: @+ deviceWithGPUDevices:@
deviceWithGPUDevices :: IsNSArray gpus => gpus -> IO (Id MLCDevice)
deviceWithGPUDevices gpus =
  do
    cls' <- getRequiredClass "MLCDevice"
    sendClassMessage cls' deviceWithGPUDevicesSelector (toNSArray gpus)

-- | type
--
-- The type specified when the device is created
--
-- Recommend that developers use MLCDeviceTypeAny as the device type.                This will ensure that MLCompute will select the best device to execute the neural network.                If developers want to be able to control device selection, they can select CPU or GPU and                for the GPU, they can also select a specific Metal device.
--
-- ObjC selector: @- type@
type_ :: IsMLCDevice mlcDevice => mlcDevice -> IO MLCDeviceType
type_ mlcDevice =
  sendMessage mlcDevice typeSelector

-- | actualDeviceType
--
-- The specific device selected.
--
-- This can be CPU, GPU or ANE.  If type is MLCDeviceTypeAny, this property                can be used to find out the specific device type that is selected.
--
-- ObjC selector: @- actualDeviceType@
actualDeviceType :: IsMLCDevice mlcDevice => mlcDevice -> IO MLCDeviceType
actualDeviceType mlcDevice =
  sendMessage mlcDevice actualDeviceTypeSelector

-- | @- gpuDevices@
gpuDevices :: IsMLCDevice mlcDevice => mlcDevice -> IO (Id NSArray)
gpuDevices mlcDevice =
  sendMessage mlcDevice gpuDevicesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cpuDevice@
cpuDeviceSelector :: Selector '[] (Id MLCDevice)
cpuDeviceSelector = mkSelector "cpuDevice"

-- | @Selector@ for @gpuDevice@
gpuDeviceSelector :: Selector '[] (Id MLCDevice)
gpuDeviceSelector = mkSelector "gpuDevice"

-- | @Selector@ for @aneDevice@
aneDeviceSelector :: Selector '[] (Id MLCDevice)
aneDeviceSelector = mkSelector "aneDevice"

-- | @Selector@ for @deviceWithType:@
deviceWithTypeSelector :: Selector '[MLCDeviceType] (Id MLCDevice)
deviceWithTypeSelector = mkSelector "deviceWithType:"

-- | @Selector@ for @deviceWithType:selectsMultipleComputeDevices:@
deviceWithType_selectsMultipleComputeDevicesSelector :: Selector '[MLCDeviceType, Bool] (Id MLCDevice)
deviceWithType_selectsMultipleComputeDevicesSelector = mkSelector "deviceWithType:selectsMultipleComputeDevices:"

-- | @Selector@ for @deviceWithGPUDevices:@
deviceWithGPUDevicesSelector :: Selector '[Id NSArray] (Id MLCDevice)
deviceWithGPUDevicesSelector = mkSelector "deviceWithGPUDevices:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MLCDeviceType
typeSelector = mkSelector "type"

-- | @Selector@ for @actualDeviceType@
actualDeviceTypeSelector :: Selector '[] MLCDeviceType
actualDeviceTypeSelector = mkSelector "actualDeviceType"

-- | @Selector@ for @gpuDevices@
gpuDevicesSelector :: Selector '[] (Id NSArray)
gpuDevicesSelector = mkSelector "gpuDevices"

