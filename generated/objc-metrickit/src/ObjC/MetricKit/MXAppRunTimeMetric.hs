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
  , cumulativeForegroundTimeSelector
  , cumulativeBackgroundTimeSelector
  , cumulativeBackgroundAudioTimeSelector
  , cumulativeBackgroundLocationTimeSelector


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
cumulativeForegroundTime mxAppRunTimeMetric  =
  sendMsg mxAppRunTimeMetric (mkSelector "cumulativeForegroundTime") (retPtr retVoid) [] >>= retainedObject . castPtr

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
cumulativeBackgroundTime mxAppRunTimeMetric  =
  sendMsg mxAppRunTimeMetric (mkSelector "cumulativeBackgroundTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeBackgroundAudioTime
--
-- Cumulative time the application spent running in the background to play audio
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeBackgroundAudioTime@
cumulativeBackgroundAudioTime :: IsMXAppRunTimeMetric mxAppRunTimeMetric => mxAppRunTimeMetric -> IO (Id NSMeasurement)
cumulativeBackgroundAudioTime mxAppRunTimeMetric  =
  sendMsg mxAppRunTimeMetric (mkSelector "cumulativeBackgroundAudioTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cumulativeBackgroundLocationTime
--
-- Cumulative time the application spent running in the background to acquire or process location.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- cumulativeBackgroundLocationTime@
cumulativeBackgroundLocationTime :: IsMXAppRunTimeMetric mxAppRunTimeMetric => mxAppRunTimeMetric -> IO (Id NSMeasurement)
cumulativeBackgroundLocationTime mxAppRunTimeMetric  =
  sendMsg mxAppRunTimeMetric (mkSelector "cumulativeBackgroundLocationTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeForegroundTime@
cumulativeForegroundTimeSelector :: Selector
cumulativeForegroundTimeSelector = mkSelector "cumulativeForegroundTime"

-- | @Selector@ for @cumulativeBackgroundTime@
cumulativeBackgroundTimeSelector :: Selector
cumulativeBackgroundTimeSelector = mkSelector "cumulativeBackgroundTime"

-- | @Selector@ for @cumulativeBackgroundAudioTime@
cumulativeBackgroundAudioTimeSelector :: Selector
cumulativeBackgroundAudioTimeSelector = mkSelector "cumulativeBackgroundAudioTime"

-- | @Selector@ for @cumulativeBackgroundLocationTime@
cumulativeBackgroundLocationTimeSelector :: Selector
cumulativeBackgroundLocationTimeSelector = mkSelector "cumulativeBackgroundLocationTime"

