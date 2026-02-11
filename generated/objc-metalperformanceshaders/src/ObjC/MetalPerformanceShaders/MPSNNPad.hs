{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNNPad@.
module ObjC.MetalPerformanceShaders.MPSNNPad
  ( MPSNNPad
  , IsMPSNNPad(..)
  , initWithDevice
  , initWithDevice_paddingSizeBefore_paddingSizeAfter
  , initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArray
  , initWithCoder_device
  , paddingSizeBefore
  , setPaddingSizeBefore
  , paddingSizeAfter
  , setPaddingSizeAfter
  , fillValue
  , setFillValue
  , initWithDeviceSelector
  , initWithDevice_paddingSizeBefore_paddingSizeAfterSelector
  , initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArraySelector
  , initWithCoder_deviceSelector
  , paddingSizeBeforeSelector
  , setPaddingSizeBeforeSelector
  , paddingSizeAfterSelector
  , setPaddingSizeAfterSelector
  , fillValueSelector
  , setFillValueSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Initialize a MPSNNPad kernel
--
-- @device@ — The device the filter will run on.
--
-- Returns: A valid MPSNNPad object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNPad mpsnnPad => mpsnnPad -> RawId -> IO (Id MPSNNPad)
initWithDevice mpsnnPad  device =
  sendMsg mpsnnPad (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize a MPSNNPad kernel
--
-- @device@ — The device the filter will run on
--
-- @paddingSizeBefore@ — The amount of padding to add before the source image - see details above.
--
-- @paddingSizeAfter@ — The amount of padding to add after the source image - see details above.
--
-- Returns: A valid MPSNNPad object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:paddingSizeBefore:paddingSizeAfter:@
initWithDevice_paddingSizeBefore_paddingSizeAfter :: IsMPSNNPad mpsnnPad => mpsnnPad -> RawId -> MPSImageCoordinate -> MPSImageCoordinate -> IO (Id MPSNNPad)
initWithDevice_paddingSizeBefore_paddingSizeAfter mpsnnPad  device paddingSizeBefore paddingSizeAfter =
  sendMsg mpsnnPad (mkSelector "initWithDevice:paddingSizeBefore:paddingSizeAfter:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argMPSImageCoordinate paddingSizeBefore, argMPSImageCoordinate paddingSizeAfter] >>= ownedObject . castPtr

-- | Initialize a MPSNNPad kernel
--
-- @device@ — The device the filter will run on
--
-- @paddingSizeBefore@ — The amount of padding to add before the source image - see details above.
--
-- @paddingSizeAfter@ — The amount of padding to add after the source image - see details above.
--
-- @fillValueArray@ — A NSData containing a float array to use with MPSImageEdgeModeConstant.                                  The first value of the array will correspond to the first feature channel                                  written out to the destination image and the number of float values in the                                  data must be at least as large as the number of feature channels written onto                                  the destination by the filter. Failing to pass a large enough array will                                  result in undefined behavior. Passing in nil is fine.
--
-- Returns: A valid MPSNNPad object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:paddingSizeBefore:paddingSizeAfter:fillValueArray:@
initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArray :: (IsMPSNNPad mpsnnPad, IsNSData fillValueArray) => mpsnnPad -> RawId -> MPSImageCoordinate -> MPSImageCoordinate -> fillValueArray -> IO (Id MPSNNPad)
initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArray mpsnnPad  device paddingSizeBefore paddingSizeAfter fillValueArray =
withObjCPtr fillValueArray $ \raw_fillValueArray ->
    sendMsg mpsnnPad (mkSelector "initWithDevice:paddingSizeBefore:paddingSizeAfter:fillValueArray:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argMPSImageCoordinate paddingSizeBefore, argMPSImageCoordinate paddingSizeAfter, argPtr (castPtr raw_fillValueArray :: Ptr ())] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSNNPad
--
-- @device@ — The MTLDevice on which to make the MPSNNPad
--
-- Returns: A new MPSNNPad object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNPad mpsnnPad, IsNSCoder aDecoder) => mpsnnPad -> aDecoder -> RawId -> IO (Id MPSNNPad)
initWithCoder_device mpsnnPad  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnPad (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | paddingSizeBefore
--
-- This property is used for automatically sizing the destination image              for the function destinationImageDescriptorForSourceImages:sourceStates:. Defines              how much padding to assign on the left, top and smaller feature channel indices              of the image. NOTE: the x and y coordinates of this property are only              used through destinationImageDescriptorForSourceImages:sourceStates:, since              the clipRect and offset together define the padding sizes in those directions, but              the 'channel' size defines the amount of padding to be applied in the feature              channel dimension, before the feature channels starting from feature channel              index sourceFeatureChannelOffset.              Default: { 0, 0, 0 }
--
-- ObjC selector: @- paddingSizeBefore@
paddingSizeBefore :: IsMPSNNPad mpsnnPad => mpsnnPad -> IO MPSImageCoordinate
paddingSizeBefore mpsnnPad  =
  sendMsgStret mpsnnPad (mkSelector "paddingSizeBefore") retMPSImageCoordinate []

-- | paddingSizeBefore
--
-- This property is used for automatically sizing the destination image              for the function destinationImageDescriptorForSourceImages:sourceStates:. Defines              how much padding to assign on the left, top and smaller feature channel indices              of the image. NOTE: the x and y coordinates of this property are only              used through destinationImageDescriptorForSourceImages:sourceStates:, since              the clipRect and offset together define the padding sizes in those directions, but              the 'channel' size defines the amount of padding to be applied in the feature              channel dimension, before the feature channels starting from feature channel              index sourceFeatureChannelOffset.              Default: { 0, 0, 0 }
--
-- ObjC selector: @- setPaddingSizeBefore:@
setPaddingSizeBefore :: IsMPSNNPad mpsnnPad => mpsnnPad -> MPSImageCoordinate -> IO ()
setPaddingSizeBefore mpsnnPad  value =
  sendMsg mpsnnPad (mkSelector "setPaddingSizeBefore:") retVoid [argMPSImageCoordinate value]

-- | paddingSizeAfter
--
-- This property is used for automatically sizing the destination image              for the function destinationImageDescriptorForSourceImages:sourceStates:. Defines              how much padding to assign on the right, bottom and higher feature channel indices              of the image. NOTE: the x and y coordinates of this property are only              used through destinationImageDescriptorForSourceImages:sourceStates:, since              the clipRect and offset together define the padding sizes in those directions, but              the 'channel' size defines the amount of padding to be applied in the feature              channel dimension after source feature channel index determined by the sum of              sourceFeatureChannelOffset and sourceFeatureChannelMaxCount, naturally              clipped to fit the feature channels in the provided source image.              Default: { 0, 0, 0 }
--
-- ObjC selector: @- paddingSizeAfter@
paddingSizeAfter :: IsMPSNNPad mpsnnPad => mpsnnPad -> IO MPSImageCoordinate
paddingSizeAfter mpsnnPad  =
  sendMsgStret mpsnnPad (mkSelector "paddingSizeAfter") retMPSImageCoordinate []

-- | paddingSizeAfter
--
-- This property is used for automatically sizing the destination image              for the function destinationImageDescriptorForSourceImages:sourceStates:. Defines              how much padding to assign on the right, bottom and higher feature channel indices              of the image. NOTE: the x and y coordinates of this property are only              used through destinationImageDescriptorForSourceImages:sourceStates:, since              the clipRect and offset together define the padding sizes in those directions, but              the 'channel' size defines the amount of padding to be applied in the feature              channel dimension after source feature channel index determined by the sum of              sourceFeatureChannelOffset and sourceFeatureChannelMaxCount, naturally              clipped to fit the feature channels in the provided source image.              Default: { 0, 0, 0 }
--
-- ObjC selector: @- setPaddingSizeAfter:@
setPaddingSizeAfter :: IsMPSNNPad mpsnnPad => mpsnnPad -> MPSImageCoordinate -> IO ()
setPaddingSizeAfter mpsnnPad  value =
  sendMsg mpsnnPad (mkSelector "setPaddingSizeAfter:") retVoid [argMPSImageCoordinate value]

-- | fillValue
--
-- Determines the constant value to apply when using MPSImageEdgeModeConstant. Default: 0.0f.              NOTE: this value is ignored if the filter is initialized with a per-channel fill value              using initWithDevice:paddingSizeBefore:paddingSizeAfter:fillValueArray:.
--
-- ObjC selector: @- fillValue@
fillValue :: IsMPSNNPad mpsnnPad => mpsnnPad -> IO CFloat
fillValue mpsnnPad  =
  sendMsg mpsnnPad (mkSelector "fillValue") retCFloat []

-- | fillValue
--
-- Determines the constant value to apply when using MPSImageEdgeModeConstant. Default: 0.0f.              NOTE: this value is ignored if the filter is initialized with a per-channel fill value              using initWithDevice:paddingSizeBefore:paddingSizeAfter:fillValueArray:.
--
-- ObjC selector: @- setFillValue:@
setFillValue :: IsMPSNNPad mpsnnPad => mpsnnPad -> CFloat -> IO ()
setFillValue mpsnnPad  value =
  sendMsg mpsnnPad (mkSelector "setFillValue:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:paddingSizeBefore:paddingSizeAfter:@
initWithDevice_paddingSizeBefore_paddingSizeAfterSelector :: Selector
initWithDevice_paddingSizeBefore_paddingSizeAfterSelector = mkSelector "initWithDevice:paddingSizeBefore:paddingSizeAfter:"

-- | @Selector@ for @initWithDevice:paddingSizeBefore:paddingSizeAfter:fillValueArray:@
initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArraySelector :: Selector
initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArraySelector = mkSelector "initWithDevice:paddingSizeBefore:paddingSizeAfter:fillValueArray:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @paddingSizeBefore@
paddingSizeBeforeSelector :: Selector
paddingSizeBeforeSelector = mkSelector "paddingSizeBefore"

-- | @Selector@ for @setPaddingSizeBefore:@
setPaddingSizeBeforeSelector :: Selector
setPaddingSizeBeforeSelector = mkSelector "setPaddingSizeBefore:"

-- | @Selector@ for @paddingSizeAfter@
paddingSizeAfterSelector :: Selector
paddingSizeAfterSelector = mkSelector "paddingSizeAfter"

-- | @Selector@ for @setPaddingSizeAfter:@
setPaddingSizeAfterSelector :: Selector
setPaddingSizeAfterSelector = mkSelector "setPaddingSizeAfter:"

-- | @Selector@ for @fillValue@
fillValueSelector :: Selector
fillValueSelector = mkSelector "fillValue"

-- | @Selector@ for @setFillValue:@
setFillValueSelector :: Selector
setFillValueSelector = mkSelector "setFillValue:"

