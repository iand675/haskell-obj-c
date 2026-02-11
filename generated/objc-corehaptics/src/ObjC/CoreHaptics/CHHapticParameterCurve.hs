{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CHHapticParameterCurve
--
-- A CHHapticParameterCurve is a set of CHHapticParameterCurveControlPoints which describe the control (inflection) points 		for the parameter values to be applied to the associated pattern.
--
-- The CHHapticParameterCurve generates an interpolated value output which passed through each control point at its 		associated relative time.  These times will all be relative to the start time of the CHHapticParameterCurve within the 		playing pattern.
--
-- Generated bindings for @CHHapticParameterCurve@.
module ObjC.CoreHaptics.CHHapticParameterCurve
  ( CHHapticParameterCurve
  , IsCHHapticParameterCurve(..)
  , init_
  , initWithParameterID_controlPoints_relativeTime
  , parameterID
  , relativeTime
  , setRelativeTime
  , controlPoints
  , initSelector
  , initWithParameterID_controlPoints_relativeTimeSelector
  , parameterIDSelector
  , relativeTimeSelector
  , setRelativeTimeSelector
  , controlPointsSelector


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
init_ :: IsCHHapticParameterCurve chHapticParameterCurve => chHapticParameterCurve -> IO (Id CHHapticParameterCurve)
init_ chHapticParameterCurve  =
  sendMsg chHapticParameterCurve (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithParameterID:controlPoints:relativeTime
--
-- Initialize a CHHapticParameterCurve with a parameter ID, time, and an array of CHHapticParameterCurveControlPoint.
--
-- @parameterID@ — The CHHapticDynamicParameterID for the desired parameter.
--
-- @controlPoints@ — An array of CHHapticParameterCurveControlPoints.
--
-- @relativeTime@ — The time at which this parameter curve should start, relative to the start time of the CHHapticPattern to which this 		parameter curve belongs.
--
-- ObjC selector: @- initWithParameterID:controlPoints:relativeTime:@
initWithParameterID_controlPoints_relativeTime :: (IsCHHapticParameterCurve chHapticParameterCurve, IsNSString parameterID, IsNSArray controlPoints) => chHapticParameterCurve -> parameterID -> controlPoints -> CDouble -> IO (Id CHHapticParameterCurve)
initWithParameterID_controlPoints_relativeTime chHapticParameterCurve  parameterID controlPoints relativeTime =
withObjCPtr parameterID $ \raw_parameterID ->
  withObjCPtr controlPoints $ \raw_controlPoints ->
      sendMsg chHapticParameterCurve (mkSelector "initWithParameterID:controlPoints:relativeTime:") (retPtr retVoid) [argPtr (castPtr raw_parameterID :: Ptr ()), argPtr (castPtr raw_controlPoints :: Ptr ()), argCDouble (fromIntegral relativeTime)] >>= ownedObject . castPtr

-- | @- parameterID@
parameterID :: IsCHHapticParameterCurve chHapticParameterCurve => chHapticParameterCurve -> IO (Id NSString)
parameterID chHapticParameterCurve  =
  sendMsg chHapticParameterCurve (mkSelector "parameterID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relativeTime@
relativeTime :: IsCHHapticParameterCurve chHapticParameterCurve => chHapticParameterCurve -> IO CDouble
relativeTime chHapticParameterCurve  =
  sendMsg chHapticParameterCurve (mkSelector "relativeTime") retCDouble []

-- | @- setRelativeTime:@
setRelativeTime :: IsCHHapticParameterCurve chHapticParameterCurve => chHapticParameterCurve -> CDouble -> IO ()
setRelativeTime chHapticParameterCurve  value =
  sendMsg chHapticParameterCurve (mkSelector "setRelativeTime:") retVoid [argCDouble (fromIntegral value)]

-- | @- controlPoints@
controlPoints :: IsCHHapticParameterCurve chHapticParameterCurve => chHapticParameterCurve -> IO (Id NSArray)
controlPoints chHapticParameterCurve  =
  sendMsg chHapticParameterCurve (mkSelector "controlPoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithParameterID:controlPoints:relativeTime:@
initWithParameterID_controlPoints_relativeTimeSelector :: Selector
initWithParameterID_controlPoints_relativeTimeSelector = mkSelector "initWithParameterID:controlPoints:relativeTime:"

-- | @Selector@ for @parameterID@
parameterIDSelector :: Selector
parameterIDSelector = mkSelector "parameterID"

-- | @Selector@ for @relativeTime@
relativeTimeSelector :: Selector
relativeTimeSelector = mkSelector "relativeTime"

-- | @Selector@ for @setRelativeTime:@
setRelativeTimeSelector :: Selector
setRelativeTimeSelector = mkSelector "setRelativeTime:"

-- | @Selector@ for @controlPoints@
controlPointsSelector :: Selector
controlPointsSelector = mkSelector "controlPoints"

