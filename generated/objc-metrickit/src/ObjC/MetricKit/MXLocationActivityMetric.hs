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
  , cumulativeBestAccuracyTimeSelector
  , cumulativeBestAccuracyForNavigationTimeSelector
  , cumulativeNearestTenMetersAccuracyTimeSelector
  , cumulativeHundredMetersAccuracyTimeSelector
  , cumulativeKilometerAccuracyTimeSelector
  , cumulativeThreeKilometersAccuracyTimeSelector


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
cumulativeBestAccuracyTime mxLocationActivityMetric  =
  sendMsg mxLocationActivityMetric (mkSelector "cumulativeBestAccuracyTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeBestAccuracyForNavigationTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyBestForNavigation.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeBestAccuracyForNavigationTime@
cumulativeBestAccuracyForNavigationTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeBestAccuracyForNavigationTime mxLocationActivityMetric  =
  sendMsg mxLocationActivityMetric (mkSelector "cumulativeBestAccuracyForNavigationTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeNearestTenMetersAccuracyTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyNearestTenMeters.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeNearestTenMetersAccuracyTime@
cumulativeNearestTenMetersAccuracyTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeNearestTenMetersAccuracyTime mxLocationActivityMetric  =
  sendMsg mxLocationActivityMetric (mkSelector "cumulativeNearestTenMetersAccuracyTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeHundredMetersAccuracyTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyHundredMeters.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeHundredMetersAccuracyTime@
cumulativeHundredMetersAccuracyTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeHundredMetersAccuracyTime mxLocationActivityMetric  =
  sendMsg mxLocationActivityMetric (mkSelector "cumulativeHundredMetersAccuracyTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeKilometerAccuracyTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyKilometer.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeKilometerAccuracyTime@
cumulativeKilometerAccuracyTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeKilometerAccuracyTime mxLocationActivityMetric  =
  sendMsg mxLocationActivityMetric (mkSelector "cumulativeKilometerAccuracyTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeThreeKilometersAccuracyTime
--
-- Cumulative time spent acquiring location at kCLLocationAccuracyThreeKilometers.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeThreeKilometersAccuracyTime@
cumulativeThreeKilometersAccuracyTime :: IsMXLocationActivityMetric mxLocationActivityMetric => mxLocationActivityMetric -> IO (Id NSMeasurement)
cumulativeThreeKilometersAccuracyTime mxLocationActivityMetric  =
  sendMsg mxLocationActivityMetric (mkSelector "cumulativeThreeKilometersAccuracyTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeBestAccuracyTime@
cumulativeBestAccuracyTimeSelector :: Selector
cumulativeBestAccuracyTimeSelector = mkSelector "cumulativeBestAccuracyTime"

-- | @Selector@ for @cumulativeBestAccuracyForNavigationTime@
cumulativeBestAccuracyForNavigationTimeSelector :: Selector
cumulativeBestAccuracyForNavigationTimeSelector = mkSelector "cumulativeBestAccuracyForNavigationTime"

-- | @Selector@ for @cumulativeNearestTenMetersAccuracyTime@
cumulativeNearestTenMetersAccuracyTimeSelector :: Selector
cumulativeNearestTenMetersAccuracyTimeSelector = mkSelector "cumulativeNearestTenMetersAccuracyTime"

-- | @Selector@ for @cumulativeHundredMetersAccuracyTime@
cumulativeHundredMetersAccuracyTimeSelector :: Selector
cumulativeHundredMetersAccuracyTimeSelector = mkSelector "cumulativeHundredMetersAccuracyTime"

-- | @Selector@ for @cumulativeKilometerAccuracyTime@
cumulativeKilometerAccuracyTimeSelector :: Selector
cumulativeKilometerAccuracyTimeSelector = mkSelector "cumulativeKilometerAccuracyTime"

-- | @Selector@ for @cumulativeThreeKilometersAccuracyTime@
cumulativeThreeKilometersAccuracyTimeSelector :: Selector
cumulativeThreeKilometersAccuracyTimeSelector = mkSelector "cumulativeThreeKilometersAccuracyTime"

