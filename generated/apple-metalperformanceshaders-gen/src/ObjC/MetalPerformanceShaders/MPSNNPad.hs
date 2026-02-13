{-# LANGUAGE DataKinds #-}
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
  , fillValueSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_paddingSizeBefore_paddingSizeAfterSelector
  , initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArraySelector
  , paddingSizeAfterSelector
  , paddingSizeBeforeSelector
  , setFillValueSelector
  , setPaddingSizeAfterSelector
  , setPaddingSizeBeforeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithDevice mpsnnPad device =
  sendOwnedMessage mpsnnPad initWithDeviceSelector device

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
initWithDevice_paddingSizeBefore_paddingSizeAfter mpsnnPad device paddingSizeBefore paddingSizeAfter =
  sendOwnedMessage mpsnnPad initWithDevice_paddingSizeBefore_paddingSizeAfterSelector device paddingSizeBefore paddingSizeAfter

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
initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArray mpsnnPad device paddingSizeBefore paddingSizeAfter fillValueArray =
  sendOwnedMessage mpsnnPad initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArraySelector device paddingSizeBefore paddingSizeAfter (toNSData fillValueArray)

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
initWithCoder_device mpsnnPad aDecoder device =
  sendOwnedMessage mpsnnPad initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | paddingSizeBefore
--
-- This property is used for automatically sizing the destination image              for the function destinationImageDescriptorForSourceImages:sourceStates:. Defines              how much padding to assign on the left, top and smaller feature channel indices              of the image. NOTE: the x and y coordinates of this property are only              used through destinationImageDescriptorForSourceImages:sourceStates:, since              the clipRect and offset together define the padding sizes in those directions, but              the 'channel' size defines the amount of padding to be applied in the feature              channel dimension, before the feature channels starting from feature channel              index sourceFeatureChannelOffset.              Default: { 0, 0, 0 }
--
-- ObjC selector: @- paddingSizeBefore@
paddingSizeBefore :: IsMPSNNPad mpsnnPad => mpsnnPad -> IO MPSImageCoordinate
paddingSizeBefore mpsnnPad =
  sendMessage mpsnnPad paddingSizeBeforeSelector

-- | paddingSizeBefore
--
-- This property is used for automatically sizing the destination image              for the function destinationImageDescriptorForSourceImages:sourceStates:. Defines              how much padding to assign on the left, top and smaller feature channel indices              of the image. NOTE: the x and y coordinates of this property are only              used through destinationImageDescriptorForSourceImages:sourceStates:, since              the clipRect and offset together define the padding sizes in those directions, but              the 'channel' size defines the amount of padding to be applied in the feature              channel dimension, before the feature channels starting from feature channel              index sourceFeatureChannelOffset.              Default: { 0, 0, 0 }
--
-- ObjC selector: @- setPaddingSizeBefore:@
setPaddingSizeBefore :: IsMPSNNPad mpsnnPad => mpsnnPad -> MPSImageCoordinate -> IO ()
setPaddingSizeBefore mpsnnPad value =
  sendMessage mpsnnPad setPaddingSizeBeforeSelector value

-- | paddingSizeAfter
--
-- This property is used for automatically sizing the destination image              for the function destinationImageDescriptorForSourceImages:sourceStates:. Defines              how much padding to assign on the right, bottom and higher feature channel indices              of the image. NOTE: the x and y coordinates of this property are only              used through destinationImageDescriptorForSourceImages:sourceStates:, since              the clipRect and offset together define the padding sizes in those directions, but              the 'channel' size defines the amount of padding to be applied in the feature              channel dimension after source feature channel index determined by the sum of              sourceFeatureChannelOffset and sourceFeatureChannelMaxCount, naturally              clipped to fit the feature channels in the provided source image.              Default: { 0, 0, 0 }
--
-- ObjC selector: @- paddingSizeAfter@
paddingSizeAfter :: IsMPSNNPad mpsnnPad => mpsnnPad -> IO MPSImageCoordinate
paddingSizeAfter mpsnnPad =
  sendMessage mpsnnPad paddingSizeAfterSelector

-- | paddingSizeAfter
--
-- This property is used for automatically sizing the destination image              for the function destinationImageDescriptorForSourceImages:sourceStates:. Defines              how much padding to assign on the right, bottom and higher feature channel indices              of the image. NOTE: the x and y coordinates of this property are only              used through destinationImageDescriptorForSourceImages:sourceStates:, since              the clipRect and offset together define the padding sizes in those directions, but              the 'channel' size defines the amount of padding to be applied in the feature              channel dimension after source feature channel index determined by the sum of              sourceFeatureChannelOffset and sourceFeatureChannelMaxCount, naturally              clipped to fit the feature channels in the provided source image.              Default: { 0, 0, 0 }
--
-- ObjC selector: @- setPaddingSizeAfter:@
setPaddingSizeAfter :: IsMPSNNPad mpsnnPad => mpsnnPad -> MPSImageCoordinate -> IO ()
setPaddingSizeAfter mpsnnPad value =
  sendMessage mpsnnPad setPaddingSizeAfterSelector value

-- | fillValue
--
-- Determines the constant value to apply when using MPSImageEdgeModeConstant. Default: 0.0f.              NOTE: this value is ignored if the filter is initialized with a per-channel fill value              using initWithDevice:paddingSizeBefore:paddingSizeAfter:fillValueArray:.
--
-- ObjC selector: @- fillValue@
fillValue :: IsMPSNNPad mpsnnPad => mpsnnPad -> IO CFloat
fillValue mpsnnPad =
  sendMessage mpsnnPad fillValueSelector

-- | fillValue
--
-- Determines the constant value to apply when using MPSImageEdgeModeConstant. Default: 0.0f.              NOTE: this value is ignored if the filter is initialized with a per-channel fill value              using initWithDevice:paddingSizeBefore:paddingSizeAfter:fillValueArray:.
--
-- ObjC selector: @- setFillValue:@
setFillValue :: IsMPSNNPad mpsnnPad => mpsnnPad -> CFloat -> IO ()
setFillValue mpsnnPad value =
  sendMessage mpsnnPad setFillValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNPad)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:paddingSizeBefore:paddingSizeAfter:@
initWithDevice_paddingSizeBefore_paddingSizeAfterSelector :: Selector '[RawId, MPSImageCoordinate, MPSImageCoordinate] (Id MPSNNPad)
initWithDevice_paddingSizeBefore_paddingSizeAfterSelector = mkSelector "initWithDevice:paddingSizeBefore:paddingSizeAfter:"

-- | @Selector@ for @initWithDevice:paddingSizeBefore:paddingSizeAfter:fillValueArray:@
initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArraySelector :: Selector '[RawId, MPSImageCoordinate, MPSImageCoordinate, Id NSData] (Id MPSNNPad)
initWithDevice_paddingSizeBefore_paddingSizeAfter_fillValueArraySelector = mkSelector "initWithDevice:paddingSizeBefore:paddingSizeAfter:fillValueArray:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNPad)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @paddingSizeBefore@
paddingSizeBeforeSelector :: Selector '[] MPSImageCoordinate
paddingSizeBeforeSelector = mkSelector "paddingSizeBefore"

-- | @Selector@ for @setPaddingSizeBefore:@
setPaddingSizeBeforeSelector :: Selector '[MPSImageCoordinate] ()
setPaddingSizeBeforeSelector = mkSelector "setPaddingSizeBefore:"

-- | @Selector@ for @paddingSizeAfter@
paddingSizeAfterSelector :: Selector '[] MPSImageCoordinate
paddingSizeAfterSelector = mkSelector "paddingSizeAfter"

-- | @Selector@ for @setPaddingSizeAfter:@
setPaddingSizeAfterSelector :: Selector '[MPSImageCoordinate] ()
setPaddingSizeAfterSelector = mkSelector "setPaddingSizeAfter:"

-- | @Selector@ for @fillValue@
fillValueSelector :: Selector '[] CFloat
fillValueSelector = mkSelector "fillValue"

-- | @Selector@ for @setFillValue:@
setFillValueSelector :: Selector '[CFloat] ()
setFillValueSelector = mkSelector "setFillValue:"

