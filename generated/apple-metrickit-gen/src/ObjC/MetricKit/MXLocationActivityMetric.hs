{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXLocationActivityMetric
--
-- An MXMetric subclass that encapsulates location metrics
--
-- The metrics contained in this class describe properties of location activity. See MXAppRunTimeMetric for time spent performing location activities.
--
-- Generated bindings for @MXLocationActivityMetric@.
module ObjC.MetricKit.MXLocationActivityMetric
  ( MXLocationActivityMetric
  , IsMXLocationActivityMetric(..)
  , cumulativeBestAccuracyTime
  , cumulativeBestAccuracyForNavigationTime
  , cumulativeNearestTenMetersAccuracyTime
  , cumulativeHundredMetersAccuracyTime
  , cumulativeKilometerAccuracyTime
  , cumulativeThreeKilometersAccuracyTime
  , cumulativeBestAccuracyForNavigationTimeSelector
  , cumulativeBestAccuracyTimeSelector
  , cumulativeHundredMetersAccuracyTimeSelector
  , cumulativeKilometerAccuracyTimeSelector
  , cumulativeNearestTenMetersAccuracyTimeSelector
  , cumulativeThreeKilometersAccuracyTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | cumulativeBestAccuracyTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyBest.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeBestAccuracyTime@
cumulativeBestAccuracyTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeBestAccuracyTime mxLocationActivityMetric =
  sendMessage mxLocationActivityMetric cumulativeBestAccuracyTimeSelector

-- | cumulativeBestAccuracyForNavigationTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyBestForNavigation.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeBestAccuracyForNavigationTime@
cumulativeBestAccuracyForNavigationTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeBestAccuracyForNavigationTime mxLocationActivityMetric =
  sendMessage mxLocationActivityMetric cumulativeBestAccuracyForNavigationTimeSelector

-- | cumulativeNearestTenMetersAccuracyTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyNearestTenMeters.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeNearestTenMetersAccuracyTime@
cumulativeNearestTenMetersAccuracyTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeNearestTenMetersAccuracyTime mxLocationActivityMetric =
  sendMessage mxLocationActivityMetric cumulativeNearestTenMetersAccuracyTimeSelector

-- | cumulativeHundredMetersAccuracyTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyHundredMeters.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeHundredMetersAccuracyTime@
cumulativeHundredMetersAccuracyTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeHundredMetersAccuracyTime mxLocationActivityMetric =
  sendMessage mxLocationActivityMetric cumulativeHundredMetersAccuracyTimeSelector

-- | cumulativeKilometerAccuracyTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyKilometer.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeKilometerAccuracyTime@
cumulativeKilometerAccuracyTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeKilometerAccuracyTime mxLocationActivityMetric =
  sendMessage mxLocationActivityMetric cumulativeKilometerAccuracyTimeSelector

-- | cumulativeThreeKilometersAccuracyTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyThreeKilometers.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeThreeKilometersAccuracyTime@
cumulativeThreeKilometersAccuracyTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeThreeKilometersAccuracyTime mxLocationActivityMetric =
  sendMessage mxLocationActivityMetric cumulativeThreeKilometersAccuracyTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeBestAccuracyTime@
cumulativeBestAccuracyTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeBestAccuracyTimeSelector = mkSelector "cumulativeBestAccuracyTime"

-- | @Selector@ for @cumulativeBestAccuracyForNavigationTime@
cumulativeBestAccuracyForNavigationTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeBestAccuracyForNavigationTimeSelector = mkSelector "cumulativeBestAccuracyForNavigationTime"

-- | @Selector@ for @cumulativeNearestTenMetersAccuracyTime@
cumulativeNearestTenMetersAccuracyTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeNearestTenMetersAccuracyTimeSelector = mkSelector "cumulativeNearestTenMetersAccuracyTime"

-- | @Selector@ for @cumulativeHundredMetersAccuracyTime@
cumulativeHundredMetersAccuracyTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeHundredMetersAccuracyTimeSelector = mkSelector "cumulativeHundredMetersAccuracyTime"

-- | @Selector@ for @cumulativeKilometerAccuracyTime@
cumulativeKilometerAccuracyTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeKilometerAccuracyTimeSelector = mkSelector "cumulativeKilometerAccuracyTime"

-- | @Selector@ for @cumulativeThreeKilometersAccuracyTime@
cumulativeThreeKilometersAccuracyTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeThreeKilometersAccuracyTimeSelector = mkSelector "cumulativeThreeKilometersAccuracyTime"

