{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNLossGradient
--
-- This depends on Metal.framework.
--
-- The MPSNNLossGradient filter specifies the gradient filter for MPSNNForwardLoss.
--
-- Generated bindings for @MPSNNLossGradient@.
module ObjC.MetalPerformanceShaders.MPSNNLossGradient
  ( MPSNNLossGradient
  , IsMPSNNLossGradient(..)
  , initWithDevice
  , initWithDevice_lossDescriptor
  , initWithCoder_device
  , lossType
  , reductionType
  , reduceAcrossBatch
  , numberOfClasses
  , weight
  , setWeight
  , labelSmoothing
  , setLabelSmoothing
  , epsilon
  , setEpsilon
  , delta
  , setDelta
  , computeLabelGradients
  , setComputeLabelGradients
  , initWithDeviceSelector
  , initWithDevice_lossDescriptorSelector
  , initWithCoder_deviceSelector
  , lossTypeSelector
  , reductionTypeSelector
  , reduceAcrossBatchSelector
  , numberOfClassesSelector
  , weightSelector
  , setWeightSelector
  , labelSmoothingSelector
  , setLabelSmoothingSelector
  , epsilonSelector
  , setEpsilonSelector
  , deltaSelector
  , setDeltaSelector
  , computeLabelGradientsSelector
  , setComputeLabelGradientsSelector

  -- * Enum types
  , MPSCNNLossType(MPSCNNLossType)
  , pattern MPSCNNLossTypeMeanAbsoluteError
  , pattern MPSCNNLossTypeMeanSquaredError
  , pattern MPSCNNLossTypeSoftMaxCrossEntropy
  , pattern MPSCNNLossTypeSigmoidCrossEntropy
  , pattern MPSCNNLossTypeCategoricalCrossEntropy
  , pattern MPSCNNLossTypeHinge
  , pattern MPSCNNLossTypeHuber
  , pattern MPSCNNLossTypeCosineDistance
  , pattern MPSCNNLossTypeLog
  , pattern MPSCNNLossTypeKullbackLeiblerDivergence
  , pattern MPSCNNLossTypeCount
  , MPSCNNReductionType(MPSCNNReductionType)
  , pattern MPSCNNReductionTypeNone
  , pattern MPSCNNReductionTypeSum
  , pattern MPSCNNReductionTypeMean
  , pattern MPSCNNReductionTypeSumByNonZeroWeights
  , pattern MPSCNNReductionTypeCount

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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> RawId -> IO (Id MPSNNLossGradient)
initWithDevice mpsnnLossGradient  device =
  sendMsg mpsnnLossGradient (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize the loss gradient filter with a loss descriptor.
--
-- @device@ — The device the filter will run on.
--
-- @lossDescriptor@ — The loss descriptor.
--
-- Returns: A valid MPSNNLossGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptor :: (IsMPSNNLossGradient mpsnnLossGradient, IsMPSCNNLossDescriptor lossDescriptor) => mpsnnLossGradient -> RawId -> lossDescriptor -> IO (Id MPSNNLossGradient)
initWithDevice_lossDescriptor mpsnnLossGradient  device lossDescriptor =
withObjCPtr lossDescriptor $ \raw_lossDescriptor ->
    sendMsg mpsnnLossGradient (mkSelector "initWithDevice:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_lossDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNLossGradient mpsnnLossGradient, IsNSCoder aDecoder) => mpsnnLossGradient -> aDecoder -> RawId -> IO (Id MPSNNLossGradient)
initWithCoder_device mpsnnLossGradient  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnLossGradient (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | See MPSCNNLossDescriptor for information about the following properties.
--
-- ObjC selector: @- lossType@
lossType :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO MPSCNNLossType
lossType mpsnnLossGradient  =
  fmap (coerce :: CUInt -> MPSCNNLossType) $ sendMsg mpsnnLossGradient (mkSelector "lossType") retCUInt []

-- | @- reductionType@
reductionType :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO MPSCNNReductionType
reductionType mpsnnLossGradient  =
  fmap (coerce :: CInt -> MPSCNNReductionType) $ sendMsg mpsnnLossGradient (mkSelector "reductionType") retCInt []

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO Bool
reduceAcrossBatch mpsnnLossGradient  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnLossGradient (mkSelector "reduceAcrossBatch") retCULong []

-- | @- numberOfClasses@
numberOfClasses :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO CULong
numberOfClasses mpsnnLossGradient  =
  sendMsg mpsnnLossGradient (mkSelector "numberOfClasses") retCULong []

-- | @- weight@
weight :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO CFloat
weight mpsnnLossGradient  =
  sendMsg mpsnnLossGradient (mkSelector "weight") retCFloat []

-- | @- setWeight:@
setWeight :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> CFloat -> IO ()
setWeight mpsnnLossGradient  value =
  sendMsg mpsnnLossGradient (mkSelector "setWeight:") retVoid [argCFloat (fromIntegral value)]

-- | @- labelSmoothing@
labelSmoothing :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO CFloat
labelSmoothing mpsnnLossGradient  =
  sendMsg mpsnnLossGradient (mkSelector "labelSmoothing") retCFloat []

-- | @- setLabelSmoothing:@
setLabelSmoothing :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> CFloat -> IO ()
setLabelSmoothing mpsnnLossGradient  value =
  sendMsg mpsnnLossGradient (mkSelector "setLabelSmoothing:") retVoid [argCFloat (fromIntegral value)]

-- | @- epsilon@
epsilon :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO CFloat
epsilon mpsnnLossGradient  =
  sendMsg mpsnnLossGradient (mkSelector "epsilon") retCFloat []

-- | @- setEpsilon:@
setEpsilon :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> CFloat -> IO ()
setEpsilon mpsnnLossGradient  value =
  sendMsg mpsnnLossGradient (mkSelector "setEpsilon:") retVoid [argCFloat (fromIntegral value)]

-- | @- delta@
delta :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO CFloat
delta mpsnnLossGradient  =
  sendMsg mpsnnLossGradient (mkSelector "delta") retCFloat []

-- | @- setDelta:@
setDelta :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> CFloat -> IO ()
setDelta mpsnnLossGradient  value =
  sendMsg mpsnnLossGradient (mkSelector "setDelta:") retVoid [argCFloat (fromIntegral value)]

-- | computeLabelGradients
--
-- The computeLabelGradients property is used to control whether the loss gradient              filter computes gradients for the primary (predictions) or secondary (labels) source image from the forward pass.              Default: NO.
--
-- ObjC selector: @- computeLabelGradients@
computeLabelGradients :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO Bool
computeLabelGradients mpsnnLossGradient  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnLossGradient (mkSelector "computeLabelGradients") retCULong []

-- | computeLabelGradients
--
-- The computeLabelGradients property is used to control whether the loss gradient              filter computes gradients for the primary (predictions) or secondary (labels) source image from the forward pass.              Default: NO.
--
-- ObjC selector: @- setComputeLabelGradients:@
setComputeLabelGradients :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> Bool -> IO ()
setComputeLabelGradients mpsnnLossGradient  value =
  sendMsg mpsnnLossGradient (mkSelector "setComputeLabelGradients:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptorSelector :: Selector
initWithDevice_lossDescriptorSelector = mkSelector "initWithDevice:lossDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @lossType@
lossTypeSelector :: Selector
lossTypeSelector = mkSelector "lossType"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

-- | @Selector@ for @numberOfClasses@
numberOfClassesSelector :: Selector
numberOfClassesSelector = mkSelector "numberOfClasses"

-- | @Selector@ for @weight@
weightSelector :: Selector
weightSelector = mkSelector "weight"

-- | @Selector@ for @setWeight:@
setWeightSelector :: Selector
setWeightSelector = mkSelector "setWeight:"

-- | @Selector@ for @labelSmoothing@
labelSmoothingSelector :: Selector
labelSmoothingSelector = mkSelector "labelSmoothing"

-- | @Selector@ for @setLabelSmoothing:@
setLabelSmoothingSelector :: Selector
setLabelSmoothingSelector = mkSelector "setLabelSmoothing:"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @setEpsilon:@
setEpsilonSelector :: Selector
setEpsilonSelector = mkSelector "setEpsilon:"

-- | @Selector@ for @delta@
deltaSelector :: Selector
deltaSelector = mkSelector "delta"

-- | @Selector@ for @setDelta:@
setDeltaSelector :: Selector
setDeltaSelector = mkSelector "setDelta:"

-- | @Selector@ for @computeLabelGradients@
computeLabelGradientsSelector :: Selector
computeLabelGradientsSelector = mkSelector "computeLabelGradients"

-- | @Selector@ for @setComputeLabelGradients:@
setComputeLabelGradientsSelector :: Selector
setComputeLabelGradientsSelector = mkSelector "setComputeLabelGradients:"

