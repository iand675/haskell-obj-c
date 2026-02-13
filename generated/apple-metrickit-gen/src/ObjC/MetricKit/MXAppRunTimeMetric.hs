{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXAppRunTimeMetric
--
-- An MXMetric subclass that encapsulates app runtime metrics.
--
-- Runtime metrics describe application time spent running in different modes, such as audio, location, etc.
--
-- Generated bindings for @MXAppRunTimeMetric@.
module ObjC.MetricKit.MXAppRunTimeMetric
  ( MXAppRunTimeMetric
  , IsMXAppRunTimeMetric(..)
  , cumulativeForegroundTime
  , cumulativeBackgroundTime
  , cumulativeBackgroundAudioTime
  , cumulativeBackgroundLocationTime
  , cumulativeBackgroundAudioTimeSelector
  , cumulativeBackgroundLocationTimeSelector
  , cumulativeBackgroundTimeSelector
  , cumulativeForegroundTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | cumulativeForegroundTime
--
-- Cumulative application foreground time.
--
-- Time spent on screen and visible to the user.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeForegroundTime@
cumulativeForegroundTime :: IsMXAppRunTimeMetric mxAppRunTimeMetric => mxAppRunTimeMetric -> IO (Id NSMeasurement)
cumulativeForegroundTime mxAppRunTimeMetric =
  sendMessage mxAppRunTimeMetric cumulativeForegroundTimeSelector

-- | cumulativeBackgroundTime
--
-- Cumulative application background time.
--
-- Time spent off screen and in the background, invisible to the user.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeBackgroundTime@
cumulativeBackgroundTime :: IsMXAppRunTimeMetric mxAppRunTimeMetric => mxAppRunTimeMetric -> IO (Id NSMeasurement)
cumulativeBackgroundTime mxAppRunTimeMetric =
  sendMessage mxAppRunTimeMetric cumulativeBackgroundTimeSelector

-- | cumulativeBackgroundAudioTime
--
-- Cumulative time the application spent running in the background to play audio
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeBackgroundAudioTime@
cumulativeBackgroundAudioTime :: IsMXAppRunTimeMetric mxAppRunTimeMetric => mxAppRunTimeMetric -> IO (Id NSMeasurement)
cumulativeBackgroundAudioTime mxAppRunTimeMetric =
  sendMessage mxAppRunTimeMetric cumulativeBackgroundAudioTimeSelector

-- | cumulativeBackgroundLocationTime
--
-- Cumulative time the application spent running in the background to acquire or process location.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeBackgroundLocationTime@
cumulativeBackgroundLocationTime :: IsMXAppRunTimeMetric mxAppRunTimeMetric => mxAppRunTimeMetric -> IO (Id NSMeasurement)
cumulativeBackgroundLocationTime mxAppRunTimeMetric =
  sendMessage mxAppRunTimeMetric cumulativeBackgroundLocationTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeForegroundTime@
cumulativeForegroundTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeForegroundTimeSelector = mkSelector "cumulativeForegroundTime"

-- | @Selector@ for @cumulativeBackgroundTime@
cumulativeBackgroundTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeBackgroundTimeSelector = mkSelector "cumulativeBackgroundTime"

-- | @Selector@ for @cumulativeBackgroundAudioTime@
cumulativeBackgroundAudioTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeBackgroundAudioTimeSelector = mkSelector "cumulativeBackgroundAudioTime"

-- | @Selector@ for @cumulativeBackgroundLocationTime@
cumulativeBackgroundLocationTimeSelector :: Selector '[] (Id NSMeasurement)
cumulativeBackgroundLocationTimeSelector = mkSelector "cumulativeBackgroundLocationTime"

