{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , defaultDistributionDescriptorSelector
  , distributionTypeSelector
  , maximumSelector
  , meanSelector
  , minimumSelector
  , normalDistributionDescriptorWithMean_standardDeviationSelector
  , normalDistributionDescriptorWithMean_standardDeviation_minimum_maximumSelector
  , setDistributionTypeSelector
  , setMaximumSelector
  , setMeanSelector
  , setMinimumSelector
  , setStandardDeviationSelector
  , standardDeviationSelector
  , uniformDistributionDescriptorWithMinimum_maximumSelector

  -- * Enum types
  , MPSMatrixRandomDistribution(MPSMatrixRandomDistribution)
  , pattern MPSMatrixRandomDistributionDefault
  , pattern MPSMatrixRandomDistributionUniform
  , pattern MPSMatrixRandomDistributionNormal

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' uniformDistributionDescriptorWithMinimum_maximumSelector minimum_ maximum_

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
    sendClassMessage cls' normalDistributionDescriptorWithMean_standardDeviationSelector mean standardDeviation

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
    sendClassMessage cls' normalDistributionDescriptorWithMean_standardDeviation_minimum_maximumSelector mean standardDeviation minimum_ maximum_

-- | Make a descriptor for a default distribution.
--
-- Returns: A valid MPSMatrixRandomDistribution object or nil, if failure.
--
-- ObjC selector: @+ defaultDistributionDescriptor@
defaultDistributionDescriptor :: IO (Id MPSMatrixRandomDistributionDescriptor)
defaultDistributionDescriptor  =
  do
    cls' <- getRequiredClass "MPSMatrixRandomDistributionDescriptor"
    sendClassMessage cls' defaultDistributionDescriptorSelector

-- | distributionType
--
-- The type of distribution.
--
-- ObjC selector: @- distributionType@
distributionType :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> IO MPSMatrixRandomDistribution
distributionType mpsMatrixRandomDistributionDescriptor =
  sendMessage mpsMatrixRandomDistributionDescriptor distributionTypeSelector

-- | distributionType
--
-- The type of distribution.
--
-- ObjC selector: @- setDistributionType:@
setDistributionType :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> MPSMatrixRandomDistribution -> IO ()
setDistributionType mpsMatrixRandomDistributionDescriptor value =
  sendMessage mpsMatrixRandomDistributionDescriptor setDistributionTypeSelector value

-- | minimum
--
-- For distributions of values bounded below, this value describes the minimum.
--
-- ObjC selector: @- minimum@
minimum_ :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> IO CFloat
minimum_ mpsMatrixRandomDistributionDescriptor =
  sendMessage mpsMatrixRandomDistributionDescriptor minimumSelector

-- | minimum
--
-- For distributions of values bounded below, this value describes the minimum.
--
-- ObjC selector: @- setMinimum:@
setMinimum :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> CFloat -> IO ()
setMinimum mpsMatrixRandomDistributionDescriptor value =
  sendMessage mpsMatrixRandomDistributionDescriptor setMinimumSelector value

-- | maximum
--
-- For distributions of values bounded above, this value describes the maximum.
--
-- ObjC selector: @- maximum@
maximum_ :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> IO CFloat
maximum_ mpsMatrixRandomDistributionDescriptor =
  sendMessage mpsMatrixRandomDistributionDescriptor maximumSelector

-- | maximum
--
-- For distributions of values bounded above, this value describes the maximum.
--
-- ObjC selector: @- setMaximum:@
setMaximum :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> CFloat -> IO ()
setMaximum mpsMatrixRandomDistributionDescriptor value =
  sendMessage mpsMatrixRandomDistributionDescriptor setMaximumSelector value

-- | mean
--
-- The value to use for distributions described by their mean.
--
-- ObjC selector: @- mean@
mean :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> IO CFloat
mean mpsMatrixRandomDistributionDescriptor =
  sendMessage mpsMatrixRandomDistributionDescriptor meanSelector

-- | mean
--
-- The value to use for distributions described by their mean.
--
-- ObjC selector: @- setMean:@
setMean :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> CFloat -> IO ()
setMean mpsMatrixRandomDistributionDescriptor value =
  sendMessage mpsMatrixRandomDistributionDescriptor setMeanSelector value

-- | standardDeviation
--
-- The value to use for distributions described by their standardDeviation.
--
-- ObjC selector: @- standardDeviation@
standardDeviation :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> IO CFloat
standardDeviation mpsMatrixRandomDistributionDescriptor =
  sendMessage mpsMatrixRandomDistributionDescriptor standardDeviationSelector

-- | standardDeviation
--
-- The value to use for distributions described by their standardDeviation.
--
-- ObjC selector: @- setStandardDeviation:@
setStandardDeviation :: IsMPSMatrixRandomDistributionDescriptor mpsMatrixRandomDistributionDescriptor => mpsMatrixRandomDistributionDescriptor -> CFloat -> IO ()
setStandardDeviation mpsMatrixRandomDistributionDescriptor value =
  sendMessage mpsMatrixRandomDistributionDescriptor setStandardDeviationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniformDistributionDescriptorWithMinimum:maximum:@
uniformDistributionDescriptorWithMinimum_maximumSelector :: Selector '[CFloat, CFloat] (Id MPSMatrixRandomDistributionDescriptor)
uniformDistributionDescriptorWithMinimum_maximumSelector = mkSelector "uniformDistributionDescriptorWithMinimum:maximum:"

-- | @Selector@ for @normalDistributionDescriptorWithMean:standardDeviation:@
normalDistributionDescriptorWithMean_standardDeviationSelector :: Selector '[CFloat, CFloat] (Id MPSMatrixRandomDistributionDescriptor)
normalDistributionDescriptorWithMean_standardDeviationSelector = mkSelector "normalDistributionDescriptorWithMean:standardDeviation:"

-- | @Selector@ for @normalDistributionDescriptorWithMean:standardDeviation:minimum:maximum:@
normalDistributionDescriptorWithMean_standardDeviation_minimum_maximumSelector :: Selector '[CFloat, CFloat, CFloat, CFloat] (Id MPSMatrixRandomDistributionDescriptor)
normalDistributionDescriptorWithMean_standardDeviation_minimum_maximumSelector = mkSelector "normalDistributionDescriptorWithMean:standardDeviation:minimum:maximum:"

-- | @Selector@ for @defaultDistributionDescriptor@
defaultDistributionDescriptorSelector :: Selector '[] (Id MPSMatrixRandomDistributionDescriptor)
defaultDistributionDescriptorSelector = mkSelector "defaultDistributionDescriptor"

-- | @Selector@ for @distributionType@
distributionTypeSelector :: Selector '[] MPSMatrixRandomDistribution
distributionTypeSelector = mkSelector "distributionType"

-- | @Selector@ for @setDistributionType:@
setDistributionTypeSelector :: Selector '[MPSMatrixRandomDistribution] ()
setDistributionTypeSelector = mkSelector "setDistributionType:"

-- | @Selector@ for @minimum@
minimumSelector :: Selector '[] CFloat
minimumSelector = mkSelector "minimum"

-- | @Selector@ for @setMinimum:@
setMinimumSelector :: Selector '[CFloat] ()
setMinimumSelector = mkSelector "setMinimum:"

-- | @Selector@ for @maximum@
maximumSelector :: Selector '[] CFloat
maximumSelector = mkSelector "maximum"

-- | @Selector@ for @setMaximum:@
setMaximumSelector :: Selector '[CFloat] ()
setMaximumSelector = mkSelector "setMaximum:"

-- | @Selector@ for @mean@
meanSelector :: Selector '[] CFloat
meanSelector = mkSelector "mean"

-- | @Selector@ for @setMean:@
setMeanSelector :: Selector '[CFloat] ()
setMeanSelector = mkSelector "setMean:"

-- | @Selector@ for @standardDeviation@
standardDeviationSelector :: Selector '[] CFloat
standardDeviationSelector = mkSelector "standardDeviation"

-- | @Selector@ for @setStandardDeviation:@
setStandardDeviationSelector :: Selector '[CFloat] ()
setStandardDeviationSelector = mkSelector "setStandardDeviation:"

