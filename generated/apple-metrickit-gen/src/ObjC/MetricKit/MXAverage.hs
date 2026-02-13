{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXAverage
--
-- A class representing metric data that is averaged.
--
-- Generated bindings for @MXAverage@.
module ObjC.MetricKit.MXAverage
  ( MXAverage
  , IsMXAverage(..)
  , averageMeasurement
  , sampleCount
  , standardDeviation
  , averageMeasurementSelector
  , sampleCountSelector
  , standardDeviationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | averageMeasurement
--
-- An NSMeasurement that contains the average measurement.
--
-- ObjC selector: @- averageMeasurement@
averageMeasurement :: IsMXAverage mxAverage => mxAverage -> IO (Id NSMeasurement)
averageMeasurement mxAverage =
  sendMessage mxAverage averageMeasurementSelector

-- | sampleCount
--
-- An NSInteger representation of the number of samples in the distribution used to formulate the average.
--
-- This value is negative if an unknown number of samples was used to compute the average.
--
-- ObjC selector: @- sampleCount@
sampleCount :: IsMXAverage mxAverage => mxAverage -> IO CLong
sampleCount mxAverage =
  sendMessage mxAverage sampleCountSelector

-- | standardDeviation
--
-- An double representation of the standard deviation of the distribution.
--
-- This value is negative an unknown number of samples was used to compute the standard deviation.
--
-- ObjC selector: @- standardDeviation@
standardDeviation :: IsMXAverage mxAverage => mxAverage -> IO CDouble
standardDeviation mxAverage =
  sendMessage mxAverage standardDeviationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @averageMeasurement@
averageMeasurementSelector :: Selector '[] (Id NSMeasurement)
averageMeasurementSelector = mkSelector "averageMeasurement"

-- | @Selector@ for @sampleCount@
sampleCountSelector :: Selector '[] CLong
sampleCountSelector = mkSelector "sampleCount"

-- | @Selector@ for @standardDeviation@
standardDeviationSelector :: Selector '[] CDouble
standardDeviationSelector = mkSelector "standardDeviation"

