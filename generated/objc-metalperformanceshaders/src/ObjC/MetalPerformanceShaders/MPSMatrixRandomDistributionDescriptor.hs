{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixRandomDistributionDescriptor
--
-- This depends on Metal.framework
--
-- Decribes properties of a distribution of random values.
--
-- Generated bindings for @MPSMatrixRandomDistributionDescriptor@.
module ObjC.MetalPerformanceShaders.MPSMatrixRandomDistributionDescriptor
  ( MPSMatrixRandomDistributionDescriptor
  , IsMPSMatrixRandomDistributionDescriptor(..)
  , uniformDistributionDescriptorWithMinimum_maximum
  , normalDistributionDescriptorWithMean_standardDeviation
  , normalDistributionDescriptorWithMean_standardDeviation_minimum_maximum
  , defaultDistributionDescriptor
  , distributionType
  , setDistributionType
  , minimum_
  , setMinimum
  , maximum_
  , setMaximum
  , mean
  , setMean
  , standardDeviation
  , setStandardDeviation
  , uniformDistributionDescriptorWithMinimum_maximumSelector
  , normalDistributionDescriptorWithMean_standardDeviationSelector
  , normalDistributionDescriptorWithMean_standardDeviation_minimum_maximumSelector
  , defaultDistributionDescriptorSelector
  , distributionTypeSelector
  , setDistributionTypeSelector
  , minimumSelector
  , setMinimumSelector
  , maximumSelector
  , setMaximumSelector
  , meanSelector
  , setMeanSelector
  , standardDeviationSelector
  , setStandardDeviationSelector

  -- * Enum types
  , MPSMatrixRandomDistribution(MPSMatrixRandomDistribution)
  , pattern MPSMatrixRandomDistributionDefault
  , pattern MPSMatrixRandomDistributionUniform
  , pattern MPSMatrixRandomDistributionNormal

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

-- | Make a descriptor for a uniform distribution of floating point values in              the range [minimum, maximum).
--
-- @minimum@ — The lower bound of the range.
--
-- @maximum@ — The upper bound of the range.
--
-- Returns: A valid MPSMatrixRandomDistribution object or nil, if failure.
--
-- ObjC selector: @+ uniformDistributionDescriptorWithMinimum:maximum:@
uniformDistributionDescriptorWithMinimum_maximum :: CFloat -> CFloat -> IO (Id MPSMatrixRandomDistributionDescriptor)
uniformDistributionDescriptorWithMinimum_maximum minimum_ maximum_ =
  do
    cls' <- getRequiredClass "MPSMatrixRandomDistributionDescriptor"
    sendClassMsg cls' (mkSelector "uniformDistributionDescriptorWithMinimum:maximum:") (retPtr retVoid) [argCFloat (fromIntegral minimum_), argCFloat (fromIntegral maximum_)] >>= retainedObject . castPtr

-- | Make a descriptor for a normal distribution of floating point values.
--
-- @mean@ — The mean of the distribution
--
-- @standardDeviation@ — The standard deviation of the distribution.
--
-- Returns: A valid MPSMatrixRandomDistribution object or nil if failure.
--
-- ObjC selector: @+ normalDistributionDescriptorWithMean:standardDeviation:@
normalDistributionDescriptorWithMean_standardDeviation :: CFloat -> CFloat -> IO (Id MPSMatrixRandomDistributionDescriptor)
normalDistributionDescriptorWithMean_standardDeviation mean standardDeviation =
  do
    cls' <- getRequiredClass "MPSMatrixRandomDistributionDescriptor"
    sendClassMsg cls' (mkSelector "normalDistributionDescriptorWithMean:standardDeviation:") (retPtr retVoid) [argCFloat (fromIntegral mean), argCFloat (fromIntegral standardDeviation)] >>= retainedObject . castPtr

-- | Make a descriptor for a truncated normal distribution of floating point values.
--
-- @mean@ — The mean of the distribution
--
-- @standardDeviation@ — The standard deviation of the distribution.
--
-- @minimum@ — The lower bound of the distribution
--
-- @maximum@ — The upper bound of the distribution
--
-- Returns: A valid MPSMatrixRandomDistribution object or nil if failure.
--
-- ObjC selector: @+ normalDistributionDescriptorWithMean:standardDeviation:minimum:maximum:@
normalDistributionDescriptorWithMean_standardDeviation_minimum_maximum :: CFloat -> CFloat -> CFloat -> CFloat -> IO (Id MPSMatrixRandomDistributionDescriptor)
normalDistributionDescriptorWithMean_standardDeviation_minimum_maximum mean standardDeviation minimum_ maximum_ =
  do
    cls' <- getRequiredClass "MPSMatrixRandomDistributionDescriptor"
    sendClassMsg cls' (mkSelector "normalDistributionDescriptorWithMean:standardDeviation:minimum:maximum:") (retPtr retVoid) [argCFloat (fromIntegral mean), argCFloat (fromIntegral standardDeviation), argCFloat (fromIntegral minimum_), argCFloat (fromIntegral maximum_)] >>= retainedObject . castPtr

-- | Make a descriptor for a default distribution.
--
-- Returns: A valid MPSMatrixRandomDistribution object or nil, if failure.
--
-- ObjC selector: @+ defaultDistributionDescriptor@
defaultDistributionDescriptor :: IO (Id MPSMatrixRandomDistributionDescriptor)
defaultDistributionDescriptor  =
  do
    cls' <- getRequiredClass "MPSMatrixRandomDistributionDescriptor"
    sendClassMsg cls' (mkSelector "defaultDistributionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | distributionType
--
-- The type of distribution.
--
-- ObjC selector: @- distributionType@
distributionType :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> IO MPSMatrixRandomDistribution
distributionType mpsMatrixRandomDistributionDescriptor  =
  fmap (coerce :: CULong -> MPSMatrixRandomDistribution) $ sendMsg mpsMatrixRandomDistributionDescriptor (mkSelector "distributionType") retCULong []

-- | distributionType
--
-- The type of distribution.
--
-- ObjC selector: @- setDistributionType:@
setDistributionType :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> MPSMatrixRandomDistribution -> IO ()
setDistributionType mpsMatrixRandomDistributionDescriptor  value =
  sendMsg mpsMatrixRandomDistributionDescriptor (mkSelector "setDistributionType:") retVoid [argCULong (coerce value)]

-- | minimum
--
-- For distributions of values bounded below, this value describes the minimum.
--
-- ObjC selector: @- minimum@
minimum_ :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> IO CFloat
minimum_ mpsMatrixRandomDistributionDescriptor  =
  sendMsg mpsMatrixRandomDistributionDescriptor (mkSelector "minimum") retCFloat []

-- | minimum
--
-- For distributions of values bounded below, this value describes the minimum.
--
-- ObjC selector: @- setMinimum:@
setMinimum :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> CFloat -> IO ()
setMinimum mpsMatrixRandomDistributionDescriptor  value =
  sendMsg mpsMatrixRandomDistributionDescriptor (mkSelector "setMinimum:") retVoid [argCFloat (fromIntegral value)]

-- | maximum
--
-- For distributions of values bounded above, this value describes the maximum.
--
-- ObjC selector: @- maximum@
maximum_ :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> IO CFloat
maximum_ mpsMatrixRandomDistributionDescriptor  =
  sendMsg mpsMatrixRandomDistributionDescriptor (mkSelector "maximum") retCFloat []

-- | maximum
--
-- For distributions of values bounded above, this value describes the maximum.
--
-- ObjC selector: @- setMaximum:@
setMaximum :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> CFloat -> IO ()
setMaximum mpsMatrixRandomDistributionDescriptor  value =
  sendMsg mpsMatrixRandomDistributionDescriptor (mkSelector "setMaximum:") retVoid [argCFloat (fromIntegral value)]

-- | mean
--
-- The value to use for distributions described by their mean.
--
-- ObjC selector: @- mean@
mean :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> IO CFloat
mean mpsMatrixRandomDistributionDescriptor  =
  sendMsg mpsMatrixRandomDistributionDescriptor (mkSelector "mean") retCFloat []

-- | mean
--
-- The value to use for distributions described by their mean.
--
-- ObjC selector: @- setMean:@
setMean :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> CFloat -> IO ()
setMean mpsMatrixRandomDistributionDescriptor  value =
  sendMsg mpsMatrixRandomDistributionDescriptor (mkSelector "setMean:") retVoid [argCFloat (fromIntegral value)]

-- | standardDeviation
--
-- The value to use for distributions described by their standardDeviation.
--
-- ObjC selector: @- standardDeviation@
standardDeviation :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> IO CFloat
standardDeviation mpsMatrixRandomDistributionDescriptor  =
  sendMsg mpsMatrixRandomDistributionDescriptor (mkSelector "standardDeviation") retCFloat []

-- | standardDeviation
--
-- The value to use for distributions described by their standardDeviation.
--
-- ObjC selector: @- setStandardDeviation:@
setStandardDeviation :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> CFloat -> IO ()
setStandardDeviation mpsMatrixRandomDistributionDescriptor  value =
  sendMsg mpsMatrixRandomDistributionDescriptor (mkSelector "setStandardDeviation:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniformDistributionDescriptorWithMinimum:maximum:@
uniformDistributionDescriptorWithMinimum_maximumSelector :: Selector
uniformDistributionDescriptorWithMinimum_maximumSelector = mkSelector "uniformDistributionDescriptorWithMinimum:maximum:"

-- | @Selector@ for @normalDistributionDescriptorWithMean:standardDeviation:@
normalDistributionDescriptorWithMean_standardDeviationSelector :: Selector
normalDistributionDescriptorWithMean_standardDeviationSelector = mkSelector "normalDistributionDescriptorWithMean:standardDeviation:"

-- | @Selector@ for @normalDistributionDescriptorWithMean:standardDeviation:minimum:maximum:@
normalDistributionDescriptorWithMean_standardDeviation_minimum_maximumSelector :: Selector
normalDistributionDescriptorWithMean_standardDeviation_minimum_maximumSelector = mkSelector "normalDistributionDescriptorWithMean:standardDeviation:minimum:maximum:"

-- | @Selector@ for @defaultDistributionDescriptor@
defaultDistributionDescriptorSelector :: Selector
defaultDistributionDescriptorSelector = mkSelector "defaultDistributionDescriptor"

-- | @Selector@ for @distributionType@
distributionTypeSelector :: Selector
distributionTypeSelector = mkSelector "distributionType"

-- | @Selector@ for @setDistributionType:@
setDistributionTypeSelector :: Selector
setDistributionTypeSelector = mkSelector "setDistributionType:"

-- | @Selector@ for @minimum@
minimumSelector :: Selector
minimumSelector = mkSelector "minimum"

-- | @Selector@ for @setMinimum:@
setMinimumSelector :: Selector
setMinimumSelector = mkSelector "setMinimum:"

-- | @Selector@ for @maximum@
maximumSelector :: Selector
maximumSelector = mkSelector "maximum"

-- | @Selector@ for @setMaximum:@
setMaximumSelector :: Selector
setMaximumSelector = mkSelector "setMaximum:"

-- | @Selector@ for @mean@
meanSelector :: Selector
meanSelector = mkSelector "mean"

-- | @Selector@ for @setMean:@
setMeanSelector :: Selector
setMeanSelector = mkSelector "setMean:"

-- | @Selector@ for @standardDeviation@
standardDeviationSelector :: Selector
standardDeviationSelector = mkSelector "standardDeviation"

-- | @Selector@ for @setStandardDeviation:@
setStandardDeviationSelector :: Selector
setStandardDeviationSelector = mkSelector "setStandardDeviation:"

