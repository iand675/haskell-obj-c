{-# LANGUAGE DataKinds #-}
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
  , controlPointsSelector
  , initSelector
  , initWithParameterID_controlPoints_relativeTimeSelector
  , parameterIDSelector
  , relativeTimeSelector
  , setRelativeTimeSelector


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
init_ :: IsCHHapticParameterCurve chHapticParameterCurve => chHapticParameterCurve -> IO (Id CHHapticParameterCurve)
init_ chHapticParameterCurve =
  sendOwnedMessage chHapticParameterCurve initSelector

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
initWithParameterID_controlPoints_relativeTime chHapticParameterCurve parameterID controlPoints relativeTime =
  sendOwnedMessage chHapticParameterCurve initWithParameterID_controlPoints_relativeTimeSelector (toNSString parameterID) (toNSArray controlPoints) relativeTime

-- | @- parameterID@
parameterID :: IsCHHapticParameterCurve chHapticParameterCurve => chHapticParameterCurve -> IO (Id NSString)
parameterID chHapticParameterCurve =
  sendMessage chHapticParameterCurve parameterIDSelector

-- | @- relativeTime@
relativeTime :: IsCHHapticParameterCurve chHapticParameterCurve => chHapticParameterCurve -> IO CDouble
relativeTime chHapticParameterCurve =
  sendMessage chHapticParameterCurve relativeTimeSelector

-- | @- setRelativeTime:@
setRelativeTime :: IsCHHapticParameterCurve chHapticParameterCurve => chHapticParameterCurve -> CDouble -> IO ()
setRelativeTime chHapticParameterCurve value =
  sendMessage chHapticParameterCurve setRelativeTimeSelector value

-- | @- controlPoints@
controlPoints :: IsCHHapticParameterCurve chHapticParameterCurve => chHapticParameterCurve -> IO (Id NSArray)
controlPoints chHapticParameterCurve =
  sendMessage chHapticParameterCurve controlPointsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CHHapticParameterCurve)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithParameterID:controlPoints:relativeTime:@
initWithParameterID_controlPoints_relativeTimeSelector :: Selector '[Id NSString, Id NSArray, CDouble] (Id CHHapticParameterCurve)
initWithParameterID_controlPoints_relativeTimeSelector = mkSelector "initWithParameterID:controlPoints:relativeTime:"

-- | @Selector@ for @parameterID@
parameterIDSelector :: Selector '[] (Id NSString)
parameterIDSelector = mkSelector "parameterID"

-- | @Selector@ for @relativeTime@
relativeTimeSelector :: Selector '[] CDouble
relativeTimeSelector = mkSelector "relativeTime"

-- | @Selector@ for @setRelativeTime:@
setRelativeTimeSelector :: Selector '[CDouble] ()
setRelativeTimeSelector = mkSelector "setRelativeTime:"

-- | @Selector@ for @controlPoints@
controlPointsSelector :: Selector '[] (Id NSArray)
controlPointsSelector = mkSelector "controlPoints"

