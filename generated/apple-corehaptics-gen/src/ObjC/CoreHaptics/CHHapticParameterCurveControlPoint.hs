{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CHHapticParameterCurveControlPoint
--
-- A CHHapticParameterCurveControlPoint contains a time/value pair for a single control point within a CHHapticParameterCurve.
--
-- The relativeTime property specifies the amount of time elapsed since the start of the CHHapticParameterCurve before the 		value is reached.
--
-- Generated bindings for @CHHapticParameterCurveControlPoint@.
module ObjC.CoreHaptics.CHHapticParameterCurveControlPoint
  ( CHHapticParameterCurveControlPoint
  , IsCHHapticParameterCurveControlPoint(..)
  , init_
  , initWithRelativeTime_value
  , relativeTime
  , setRelativeTime
  , value
  , setValue
  , initSelector
  , initWithRelativeTime_valueSelector
  , relativeTimeSelector
  , setRelativeTimeSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> IO (Id CHHapticParameterCurveControlPoint)
init_ chHapticParameterCurveControlPoint =
  sendOwnedMessage chHapticParameterCurveControlPoint initSelector

-- | initWithRelativeTime:value
--
-- Initialize a CHHapticParameterCurveControlPoint with a relative time and value.
--
-- @value@ — The value of the associated parameter.
--
-- @time@ — The time at which the associated parameter will reach this value, relative to the start time of the parameter curve.
--
-- ObjC selector: @- initWithRelativeTime:value:@
initWithRelativeTime_value :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> CDouble -> CFloat -> IO (Id CHHapticParameterCurveControlPoint)
initWithRelativeTime_value chHapticParameterCurveControlPoint time value =
  sendOwnedMessage chHapticParameterCurveControlPoint initWithRelativeTime_valueSelector time value

-- | @- relativeTime@
relativeTime :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> IO CDouble
relativeTime chHapticParameterCurveControlPoint =
  sendMessage chHapticParameterCurveControlPoint relativeTimeSelector

-- | @- setRelativeTime:@
setRelativeTime :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> CDouble -> IO ()
setRelativeTime chHapticParameterCurveControlPoint value =
  sendMessage chHapticParameterCurveControlPoint setRelativeTimeSelector value

-- | @- value@
value :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> IO CFloat
value chHapticParameterCurveControlPoint =
  sendMessage chHapticParameterCurveControlPoint valueSelector

-- | @- setValue:@
setValue :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> CFloat -> IO ()
setValue chHapticParameterCurveControlPoint value =
  sendMessage chHapticParameterCurveControlPoint setValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CHHapticParameterCurveControlPoint)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRelativeTime:value:@
initWithRelativeTime_valueSelector :: Selector '[CDouble, CFloat] (Id CHHapticParameterCurveControlPoint)
initWithRelativeTime_valueSelector = mkSelector "initWithRelativeTime:value:"

-- | @Selector@ for @relativeTime@
relativeTimeSelector :: Selector '[] CDouble
relativeTimeSelector = mkSelector "relativeTime"

-- | @Selector@ for @setRelativeTime:@
setRelativeTimeSelector :: Selector '[CDouble] ()
setRelativeTimeSelector = mkSelector "setRelativeTime:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CFloat
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CFloat] ()
setValueSelector = mkSelector "setValue:"

