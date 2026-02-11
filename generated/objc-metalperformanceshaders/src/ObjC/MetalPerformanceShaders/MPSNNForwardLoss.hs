{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNForwardLoss
--
-- This depends on Metal.framework.
--
-- The MPSNNForwardLoss filter specifies a version of the loss filter which separates the forward              computation from the gradient computation. In order to compute gradients for the loss filter              use MPSNNLossGradient filter and in order to start the gradient computation of an arbitrary              image use the MPSNNInitialGradient filter.              NOTE: This filter does not support non-default offset or cliprects and setting them to other              than default values will result in undefined results.
--
-- Generated bindings for @MPSNNForwardLoss@.
module ObjC.MetalPerformanceShaders.MPSNNForwardLoss
  ( MPSNNForwardLoss
  , IsMPSNNForwardLoss(..)
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
initWithDevice :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> RawId -> IO (Id MPSNNForwardLoss)
initWithDevice mpsnnForwardLoss  device =
  sendMsg mpsnnForwardLoss (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize the loss forward pass filter with a loss descriptor.
--
-- @device@ — The device the filter will run on.
--
-- @lossDescriptor@ — The loss descriptor.
--
-- Returns: A valid MPSNNForwardLoss object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptor :: (IsMPSNNForwardLoss mpsnnForwardLoss, IsMPSCNNLossDescriptor lossDescriptor) => mpsnnForwardLoss -> RawId -> lossDescriptor -> IO (Id MPSNNForwardLoss)
initWithDevice_lossDescriptor mpsnnForwardLoss  device lossDescriptor =
withObjCPtr lossDescriptor $ \raw_lossDescriptor ->
    sendMsg mpsnnForwardLoss (mkSelector "initWithDevice:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_lossDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNForwardLoss mpsnnForwardLoss, IsNSCoder aDecoder) => mpsnnForwardLoss -> aDecoder -> RawId -> IO (Id MPSNNForwardLoss)
initWithCoder_device mpsnnForwardLoss  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnForwardLoss (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | See MPSCNNLossDescriptor for information about the following properties.
--
-- ObjC selector: @- lossType@
lossType :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO MPSCNNLossType
lossType mpsnnForwardLoss  =
  fmap (coerce :: CUInt -> MPSCNNLossType) $ sendMsg mpsnnForwardLoss (mkSelector "lossType") retCUInt []

-- | @- reductionType@
reductionType :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO MPSCNNReductionType
reductionType mpsnnForwardLoss  =
  fmap (coerce :: CInt -> MPSCNNReductionType) $ sendMsg mpsnnForwardLoss (mkSelector "reductionType") retCInt []

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO Bool
reduceAcrossBatch mpsnnForwardLoss  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnForwardLoss (mkSelector "reduceAcrossBatch") retCULong []

-- | @- numberOfClasses@
numberOfClasses :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO CULong
numberOfClasses mpsnnForwardLoss  =
  sendMsg mpsnnForwardLoss (mkSelector "numberOfClasses") retCULong []

-- | @- weight@
weight :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO CFloat
weight mpsnnForwardLoss  =
  sendMsg mpsnnForwardLoss (mkSelector "weight") retCFloat []

-- | @- setWeight:@
setWeight :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> CFloat -> IO ()
setWeight mpsnnForwardLoss  value =
  sendMsg mpsnnForwardLoss (mkSelector "setWeight:") retVoid [argCFloat (fromIntegral value)]

-- | @- labelSmoothing@
labelSmoothing :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO CFloat
labelSmoothing mpsnnForwardLoss  =
  sendMsg mpsnnForwardLoss (mkSelector "labelSmoothing") retCFloat []

-- | @- setLabelSmoothing:@
setLabelSmoothing :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> CFloat -> IO ()
setLabelSmoothing mpsnnForwardLoss  value =
  sendMsg mpsnnForwardLoss (mkSelector "setLabelSmoothing:") retVoid [argCFloat (fromIntegral value)]

-- | @- epsilon@
epsilon :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO CFloat
epsilon mpsnnForwardLoss  =
  sendMsg mpsnnForwardLoss (mkSelector "epsilon") retCFloat []

-- | @- setEpsilon:@
setEpsilon :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> CFloat -> IO ()
setEpsilon mpsnnForwardLoss  value =
  sendMsg mpsnnForwardLoss (mkSelector "setEpsilon:") retVoid [argCFloat (fromIntegral value)]

-- | @- delta@
delta :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO CFloat
delta mpsnnForwardLoss  =
  sendMsg mpsnnForwardLoss (mkSelector "delta") retCFloat []

-- | @- setDelta:@
setDelta :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> CFloat -> IO ()
setDelta mpsnnForwardLoss  value =
  sendMsg mpsnnForwardLoss (mkSelector "setDelta:") retVoid [argCFloat (fromIntegral value)]

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

