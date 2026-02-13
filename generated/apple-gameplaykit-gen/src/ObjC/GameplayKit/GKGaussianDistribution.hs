{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A gaussian distribution is biased towards the mean value, the possible outcomes are spread out from the mean with decreasing probability. Values within 1 deviation of the mean make up 68.27% of the distribution, values within 2 deviations make up 95% and values within 3 deviations make up 99.7%.
--
-- Note that a gaussian distribution's unbounded behavior beyond 3 deviations is undesired, thus this distribution deviates nominally by modifying the bounds to 3 deviations. Thus values within 3 deviations actually make up 100% of the distribution.
--
-- Generated bindings for @GKGaussianDistribution@.
module ObjC.GameplayKit.GKGaussianDistribution
  ( GKGaussianDistribution
  , IsGKGaussianDistribution(..)
  , initWithRandomSource_lowestValue_highestValue
  , initWithRandomSource_mean_deviation
  , mean
  , deviation
  , deviationSelector
  , initWithRandomSource_lowestValue_highestValueSelector
  , initWithRandomSource_mean_deviationSelector
  , meanSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a Gaussian random distribution within the range [lowest, highest] using a source to grab input values from. This sets the gaussian parameters to:
--
-- mean = (highest + lowest) / 2 deviation = (highest - lowest) / 6
--
-- The mean and deviation will be floating point numbers even if the distribution is meant to produce integer values.
--
-- See: mean
--
-- See: deviation
--
-- ObjC selector: @- initWithRandomSource:lowestValue:highestValue:@
initWithRandomSource_lowestValue_highestValue :: IsGKGaussianDistribution gkGaussianDistribution => gkGaussianDistribution -> RawId -> CLong -> CLong -> IO (Id GKGaussianDistribution)
initWithRandomSource_lowestValue_highestValue gkGaussianDistribution source lowestInclusive highestInclusive =
  sendOwnedMessage gkGaussianDistribution initWithRandomSource_lowestValue_highestValueSelector source lowestInclusive highestInclusive

-- | Initializes a Gaussian random distribution within the range [mean - 3 * deviation, mean + 3 * deviation] using a source to grab input values from.
--
-- ObjC selector: @- initWithRandomSource:mean:deviation:@
initWithRandomSource_mean_deviation :: IsGKGaussianDistribution gkGaussianDistribution => gkGaussianDistribution -> RawId -> CFloat -> CFloat -> IO (Id GKGaussianDistribution)
initWithRandomSource_mean_deviation gkGaussianDistribution source mean deviation =
  sendOwnedMessage gkGaussianDistribution initWithRandomSource_mean_deviationSelector source mean deviation

-- | The mean, or expected, value of the distribution. Values are more probable the closer to the mean they are.
--
-- ObjC selector: @- mean@
mean :: IsGKGaussianDistribution gkGaussianDistribution => gkGaussianDistribution -> IO CFloat
mean gkGaussianDistribution =
  sendMessage gkGaussianDistribution meanSelector

-- | The deviation, often called 'sigma', is the deviation from the mean that would include roughly 68% of the distribution. The range of the distribution is [mean - 3 * deviation, mean + 3 * deviation]. Values beyond 3 deviations are considered so improbable that they are removed from the output set.
--
-- ObjC selector: @- deviation@
deviation :: IsGKGaussianDistribution gkGaussianDistribution => gkGaussianDistribution -> IO CFloat
deviation gkGaussianDistribution =
  sendMessage gkGaussianDistribution deviationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRandomSource:lowestValue:highestValue:@
initWithRandomSource_lowestValue_highestValueSelector :: Selector '[RawId, CLong, CLong] (Id GKGaussianDistribution)
initWithRandomSource_lowestValue_highestValueSelector = mkSelector "initWithRandomSource:lowestValue:highestValue:"

-- | @Selector@ for @initWithRandomSource:mean:deviation:@
initWithRandomSource_mean_deviationSelector :: Selector '[RawId, CFloat, CFloat] (Id GKGaussianDistribution)
initWithRandomSource_mean_deviationSelector = mkSelector "initWithRandomSource:mean:deviation:"

-- | @Selector@ for @mean@
meanSelector :: Selector '[] CFloat
meanSelector = mkSelector "mean"

-- | @Selector@ for @deviation@
deviationSelector :: Selector '[] CFloat
deviationSelector = mkSelector "deviation"

