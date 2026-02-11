{-# LANGUAGE PatternSynonyms #-}
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
  , supportsDataType_onDeviceSelector
  , newSelector
  , initSelector
  , layerIDSelector
  , labelSelector
  , setLabelSelector
  , isDebuggingEnabledSelector
  , setIsDebuggingEnabledSelector
  , deviceTypeSelector

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
    withObjCPtr device $ \raw_device ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsDataType:onDevice:") retCULong [argCInt (coerce dataType), argPtr (castPtr raw_device :: Ptr ())]

-- | @+ new@
new :: IO (Id MLCLayer)
new  =
  do
    cls' <- getRequiredClass "MLCLayer"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCLayer mlcLayer => mlcLayer -> IO (Id MLCLayer)
init_ mlcLayer  =
  sendMsg mlcLayer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | layerID
--
-- The layer ID
--
-- A unique number to identify each layer.  Assigned when the layer is created.
--
-- ObjC selector: @- layerID@
layerID :: IsMLCLayer mlcLayer => mlcLayer -> IO CULong
layerID mlcLayer  =
  sendMsg mlcLayer (mkSelector "layerID") retCULong []

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- label@
label :: IsMLCLayer mlcLayer => mlcLayer -> IO (Id NSString)
label mlcLayer  =
  sendMsg mlcLayer (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMLCLayer mlcLayer, IsNSString value) => mlcLayer -> value -> IO ()
setLabel mlcLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg mlcLayer (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | isDebuggingEnabled
--
-- A flag to identify if we want to debug this layer when executing a graph that includes this layer
--
-- If this is set, we will make sure that the result tensor and gradient tensors are available for reading on CPU                The default is NO.  If isDebuggingEnabled is set to YES,  make sure to set options to enable debugging when                compiling the graph.  Otherwise this property may be ignored.
--
-- ObjC selector: @- isDebuggingEnabled@
isDebuggingEnabled :: IsMLCLayer mlcLayer => mlcLayer -> IO Bool
isDebuggingEnabled mlcLayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcLayer (mkSelector "isDebuggingEnabled") retCULong []

-- | isDebuggingEnabled
--
-- A flag to identify if we want to debug this layer when executing a graph that includes this layer
--
-- If this is set, we will make sure that the result tensor and gradient tensors are available for reading on CPU                The default is NO.  If isDebuggingEnabled is set to YES,  make sure to set options to enable debugging when                compiling the graph.  Otherwise this property may be ignored.
--
-- ObjC selector: @- setIsDebuggingEnabled:@
setIsDebuggingEnabled :: IsMLCLayer mlcLayer => mlcLayer -> Bool -> IO ()
setIsDebuggingEnabled mlcLayer  value =
  sendMsg mlcLayer (mkSelector "setIsDebuggingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | deviceType
--
-- The device type where this layer will be executed
--
-- Typically the MLCDevice passed to compileWithOptions will be the device used to execute layers in the graph.                If MLCDeviceTypeANE is selected, it is possible that some of the layers of the graph may not be executed on the ANE                but instead on the CPU or GPU.  This property can be used to determine which device type the layer will be executed on.
--
-- ObjC selector: @- deviceType@
deviceType :: IsMLCLayer mlcLayer => mlcLayer -> IO MLCDeviceType
deviceType mlcLayer  =
  fmap (coerce :: CInt -> MLCDeviceType) $ sendMsg mlcLayer (mkSelector "deviceType") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportsDataType:onDevice:@
supportsDataType_onDeviceSelector :: Selector
supportsDataType_onDeviceSelector = mkSelector "supportsDataType:onDevice:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @layerID@
layerIDSelector :: Selector
layerIDSelector = mkSelector "layerID"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @isDebuggingEnabled@
isDebuggingEnabledSelector :: Selector
isDebuggingEnabledSelector = mkSelector "isDebuggingEnabled"

-- | @Selector@ for @setIsDebuggingEnabled:@
setIsDebuggingEnabledSelector :: Selector
setIsDebuggingEnabledSelector = mkSelector "setIsDebuggingEnabled:"

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector
deviceTypeSelector = mkSelector "deviceType"

