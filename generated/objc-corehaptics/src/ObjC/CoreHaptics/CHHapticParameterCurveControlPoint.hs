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
  , valueSelector
  , setValueSelector


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

import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> IO (Id CHHapticParameterCurveControlPoint)
init_ chHapticParameterCurveControlPoint  =
  sendMsg chHapticParameterCurveControlPoint (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithRelativeTime_value chHapticParameterCurveControlPoint  time value =
  sendMsg chHapticParameterCurveControlPoint (mkSelector "initWithRelativeTime:value:") (retPtr retVoid) [argCDouble (fromIntegral time), argCFloat (fromIntegral value)] >>= ownedObject . castPtr

-- | @- relativeTime@
relativeTime :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> IO CDouble
relativeTime chHapticParameterCurveControlPoint  =
  sendMsg chHapticParameterCurveControlPoint (mkSelector "relativeTime") retCDouble []

-- | @- setRelativeTime:@
setRelativeTime :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> CDouble -> IO ()
setRelativeTime chHapticParameterCurveControlPoint  value =
  sendMsg chHapticParameterCurveControlPoint (mkSelector "setRelativeTime:") retVoid [argCDouble (fromIntegral value)]

-- | @- value@
value :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> IO CFloat
value chHapticParameterCurveControlPoint  =
  sendMsg chHapticParameterCurveControlPoint (mkSelector "value") retCFloat []

-- | @- setValue:@
setValue :: IsCHHapticParameterCurveControlPoint chHapticParameterCurveControlPoint => chHapticParameterCurveControlPoint -> CFloat -> IO ()
setValue chHapticParameterCurveControlPoint  value =
  sendMsg chHapticParameterCurveControlPoint (mkSelector "setValue:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRelativeTime:value:@
initWithRelativeTime_valueSelector :: Selector
initWithRelativeTime_valueSelector = mkSelector "initWithRelativeTime:value:"

-- | @Selector@ for @relativeTime@
relativeTimeSelector :: Selector
relativeTimeSelector = mkSelector "relativeTime"

-- | @Selector@ for @setRelativeTime:@
setRelativeTimeSelector :: Selector
setRelativeTimeSelector = mkSelector "setRelativeTime:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

