{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A nearby object with distance and direction measurements.
--
-- Generated bindings for @NINearbyObject@.
module ObjC.NearbyInteraction.NINearbyObject
  ( NINearbyObject
  , IsNINearbyObject(..)
  , init_
  , new
  , discoveryToken
  , distance
  , verticalDirectionEstimate
  , horizontalAngle
  , discoveryTokenSelector
  , distanceSelector
  , horizontalAngleSelector
  , initSelector
  , newSelector
  , verticalDirectionEstimateSelector

  -- * Enum types
  , NINearbyObjectVerticalDirectionEstimate(NINearbyObjectVerticalDirectionEstimate)
  , pattern NINearbyObjectVerticalDirectionEstimateUnknown
  , pattern NINearbyObjectVerticalDirectionEstimateSame
  , pattern NINearbyObjectVerticalDirectionEstimateAbove
  , pattern NINearbyObjectVerticalDirectionEstimateBelow
  , pattern NINearbyObjectVerticalDirectionEstimateAboveOrBelow

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NearbyInteraction.Internal.Classes
import ObjC.NearbyInteraction.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Unavailable
--
-- ObjC selector: @- init@
init_ :: IsNINearbyObject niNearbyObject => niNearbyObject -> IO (Id NINearbyObject)
init_ niNearbyObject =
  sendOwnedMessage niNearbyObject initSelector

-- | @+ new@
new :: IO (Id NINearbyObject)
new  =
  do
    cls' <- getRequiredClass "NINearbyObject"
    sendOwnedClassMessage cls' newSelector

-- | Nearby interaction discovery token
--
-- This discovery token will be equal to the token provided in the configuration with which the session was run.
--
-- ObjC selector: @- discoveryToken@
discoveryToken :: IsNINearbyObject niNearbyObject => niNearbyObject -> IO (Id NIDiscoveryToken)
discoveryToken niNearbyObject =
  sendMessage niNearbyObject discoveryTokenSelector

-- | Distance to the nearby object in meters. If not available in this update, the value of this property will be equal to NINearbyObjectDistanceNotAvailable in Objective C, or nil in Swift.
--
-- ObjC selector: @- distance@
distance :: IsNINearbyObject niNearbyObject => niNearbyObject -> IO CFloat
distance niNearbyObject =
  sendMessage niNearbyObject distanceSelector

-- | An indication of the positional relationship to the nearby object in the vertical dimension.
--
-- ObjC selector: @- verticalDirectionEstimate@
verticalDirectionEstimate :: IsNINearbyObject niNearbyObject => niNearbyObject -> IO NINearbyObjectVerticalDirectionEstimate
verticalDirectionEstimate niNearbyObject =
  sendMessage niNearbyObject verticalDirectionEstimateSelector

-- | An angle in radians indicating the azimuthal direction to the nearby object.
--
-- when unavailable, the value will be set to @NINearbyObjectAngleNotAvailable@.
--
-- ObjC selector: @- horizontalAngle@
horizontalAngle :: IsNINearbyObject niNearbyObject => niNearbyObject -> IO CFloat
horizontalAngle niNearbyObject =
  sendMessage niNearbyObject horizontalAngleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NINearbyObject)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NINearbyObject)
newSelector = mkSelector "new"

-- | @Selector@ for @discoveryToken@
discoveryTokenSelector :: Selector '[] (Id NIDiscoveryToken)
discoveryTokenSelector = mkSelector "discoveryToken"

-- | @Selector@ for @distance@
distanceSelector :: Selector '[] CFloat
distanceSelector = mkSelector "distance"

-- | @Selector@ for @verticalDirectionEstimate@
verticalDirectionEstimateSelector :: Selector '[] NINearbyObjectVerticalDirectionEstimate
verticalDirectionEstimateSelector = mkSelector "verticalDirectionEstimate"

-- | @Selector@ for @horizontalAngle@
horizontalAngleSelector :: Selector '[] CFloat
horizontalAngleSelector = mkSelector "horizontalAngle"

