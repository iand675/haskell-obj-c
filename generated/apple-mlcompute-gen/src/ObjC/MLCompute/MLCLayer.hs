{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCLayer
--
-- The base class for all MLCompute layers
--
-- There are as many MLCLayer subclasses as there are MLCompute neural network layer objects. Make one of those.                This class defines an polymorphic interface for them.
--
-- Generated bindings for @MLCLayer@.
module ObjC.MLCompute.MLCLayer
  ( MLCLayer
  , IsMLCLayer(..)
  , supportsDataType_onDevice
  , new
  , init_
  , layerID
  , label
  , setLabel
  , isDebuggingEnabled
  , setIsDebuggingEnabled
  , deviceType
  , deviceTypeSelector
  , initSelector
  , isDebuggingEnabledSelector
  , labelSelector
  , layerIDSelector
  , newSelector
  , setIsDebuggingEnabledSelector
  , setLabelSelector
  , supportsDataType_onDeviceSelector

  -- * Enum types
  , MLCDataType(MLCDataType)
  , pattern MLCDataTypeInvalid
  , pattern MLCDataTypeFloat32
  , pattern MLCDataTypeFloat16
  , pattern MLCDataTypeBoolean
  , pattern MLCDataTypeInt64
  , pattern MLCDataTypeInt32
  , pattern MLCDataTypeInt8
  , pattern MLCDataTypeUInt8
  , pattern MLCDataTypeCount
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

-- | Determine whether instances of this layer accept source tensors of the given data type on the given device.
--
-- @dataType@ — A data type of a possible input tensor to the layer
--
-- @device@ — A device
--
-- Returns: A boolean indicating whether the data type is supported
--
-- ObjC selector: @+ supportsDataType:onDevice:@
supportsDataType_onDevice :: IsMLCDevice device => MLCDataType -> device -> IO Bool
supportsDataType_onDevice dataType device =
  do
    cls' <- getRequiredClass "MLCLayer"
    sendClassMessage cls' supportsDataType_onDeviceSelector dataType (toMLCDevice device)

-- | @+ new@
new :: IO (Id MLCLayer)
new  =
  do
    cls' <- getRequiredClass "MLCLayer"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCLayer mlcLayer => mlcLayer -> IO (Id MLCLayer)
init_ mlcLayer =
  sendOwnedMessage mlcLayer initSelector

-- | layerID
--
-- The layer ID
--
-- A unique number to identify each layer.  Assigned when the layer is created.
--
-- ObjC selector: @- layerID@
layerID :: IsMLCLayer mlcLayer => mlcLayer -> IO CULong
layerID mlcLayer =
  sendMessage mlcLayer layerIDSelector

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- label@
label :: IsMLCLayer mlcLayer => mlcLayer -> IO (Id NSString)
label mlcLayer =
  sendMessage mlcLayer labelSelector

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMLCLayer mlcLayer, IsNSString value) => mlcLayer -> value -> IO ()
setLabel mlcLayer value =
  sendMessage mlcLayer setLabelSelector (toNSString value)

-- | isDebuggingEnabled
--
-- A flag to identify if we want to debug this layer when executing a graph that includes this layer
--
-- If this is set, we will make sure that the result tensor and gradient tensors are available for reading on CPU                The default is NO.  If isDebuggingEnabled is set to YES,  make sure to set options to enable debugging when                compiling the graph.  Otherwise this property may be ignored.
--
-- ObjC selector: @- isDebuggingEnabled@
isDebuggingEnabled :: IsMLCLayer mlcLayer => mlcLayer -> IO Bool
isDebuggingEnabled mlcLayer =
  sendMessage mlcLayer isDebuggingEnabledSelector

-- | isDebuggingEnabled
--
-- A flag to identify if we want to debug this layer when executing a graph that includes this layer
--
-- If this is set, we will make sure that the result tensor and gradient tensors are available for reading on CPU                The default is NO.  If isDebuggingEnabled is set to YES,  make sure to set options to enable debugging when                compiling the graph.  Otherwise this property may be ignored.
--
-- ObjC selector: @- setIsDebuggingEnabled:@
setIsDebuggingEnabled :: IsMLCLayer mlcLayer => mlcLayer -> Bool -> IO ()
setIsDebuggingEnabled mlcLayer value =
  sendMessage mlcLayer setIsDebuggingEnabledSelector value

-- | deviceType
--
-- The device type where this layer will be executed
--
-- Typically the MLCDevice passed to compileWithOptions will be the device used to execute layers in the graph.                If MLCDeviceTypeANE is selected, it is possible that some of the layers of the graph may not be executed on the ANE                but instead on the CPU or GPU.  This property can be used to determine which device type the layer will be executed on.
--
-- ObjC selector: @- deviceType@
deviceType :: IsMLCLayer mlcLayer => mlcLayer -> IO MLCDeviceType
deviceType mlcLayer =
  sendMessage mlcLayer deviceTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportsDataType:onDevice:@
supportsDataType_onDeviceSelector :: Selector '[MLCDataType, Id MLCDevice] Bool
supportsDataType_onDeviceSelector = mkSelector "supportsDataType:onDevice:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCLayer)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCLayer)
initSelector = mkSelector "init"

-- | @Selector@ for @layerID@
layerIDSelector :: Selector '[] CULong
layerIDSelector = mkSelector "layerID"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @isDebuggingEnabled@
isDebuggingEnabledSelector :: Selector '[] Bool
isDebuggingEnabledSelector = mkSelector "isDebuggingEnabled"

-- | @Selector@ for @setIsDebuggingEnabled:@
setIsDebuggingEnabledSelector :: Selector '[Bool] ()
setIsDebuggingEnabledSelector = mkSelector "setIsDebuggingEnabled:"

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector '[] MLCDeviceType
deviceTypeSelector = mkSelector "deviceType"

