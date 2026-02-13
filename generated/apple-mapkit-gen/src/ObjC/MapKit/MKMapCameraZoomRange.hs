{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapCameraZoomRange@.
module ObjC.MapKit.MKMapCameraZoomRange
  ( MKMapCameraZoomRange
  , IsMKMapCameraZoomRange(..)
  , initWithMinCenterCoordinateDistance_maxCenterCoordinateDistance
  , initWithMinCenterCoordinateDistance
  , initWithMaxCenterCoordinateDistance
  , minCenterCoordinateDistance
  , maxCenterCoordinateDistance
  , initWithMaxCenterCoordinateDistanceSelector
  , initWithMinCenterCoordinateDistanceSelector
  , initWithMinCenterCoordinateDistance_maxCenterCoordinateDistanceSelector
  , maxCenterCoordinateDistanceSelector
  , minCenterCoordinateDistanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMinCenterCoordinateDistance:maxCenterCoordinateDistance:@
initWithMinCenterCoordinateDistance_maxCenterCoordinateDistance :: IsMKMapCameraZoomRange mkMapCameraZoomRange => mkMapCameraZoomRange -> CDouble -> CDouble -> IO (Id MKMapCameraZoomRange)
initWithMinCenterCoordinateDistance_maxCenterCoordinateDistance mkMapCameraZoomRange minDistance maxDistance =
  sendOwnedMessage mkMapCameraZoomRange initWithMinCenterCoordinateDistance_maxCenterCoordinateDistanceSelector minDistance maxDistance

-- | @- initWithMinCenterCoordinateDistance:@
initWithMinCenterCoordinateDistance :: IsMKMapCameraZoomRange mkMapCameraZoomRange => mkMapCameraZoomRange -> CDouble -> IO (Id MKMapCameraZoomRange)
initWithMinCenterCoordinateDistance mkMapCameraZoomRange minDistance =
  sendOwnedMessage mkMapCameraZoomRange initWithMinCenterCoordinateDistanceSelector minDistance

-- | @- initWithMaxCenterCoordinateDistance:@
initWithMaxCenterCoordinateDistance :: IsMKMapCameraZoomRange mkMapCameraZoomRange => mkMapCameraZoomRange -> CDouble -> IO (Id MKMapCameraZoomRange)
initWithMaxCenterCoordinateDistance mkMapCameraZoomRange maxDistance =
  sendOwnedMessage mkMapCameraZoomRange initWithMaxCenterCoordinateDistanceSelector maxDistance

-- | @- minCenterCoordinateDistance@
minCenterCoordinateDistance :: IsMKMapCameraZoomRange mkMapCameraZoomRange => mkMapCameraZoomRange -> IO CDouble
minCenterCoordinateDistance mkMapCameraZoomRange =
  sendMessage mkMapCameraZoomRange minCenterCoordinateDistanceSelector

-- | @- maxCenterCoordinateDistance@
maxCenterCoordinateDistance :: IsMKMapCameraZoomRange mkMapCameraZoomRange => mkMapCameraZoomRange -> IO CDouble
maxCenterCoordinateDistance mkMapCameraZoomRange =
  sendMessage mkMapCameraZoomRange maxCenterCoordinateDistanceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMinCenterCoordinateDistance:maxCenterCoordinateDistance:@
initWithMinCenterCoordinateDistance_maxCenterCoordinateDistanceSelector :: Selector '[CDouble, CDouble] (Id MKMapCameraZoomRange)
initWithMinCenterCoordinateDistance_maxCenterCoordinateDistanceSelector = mkSelector "initWithMinCenterCoordinateDistance:maxCenterCoordinateDistance:"

-- | @Selector@ for @initWithMinCenterCoordinateDistance:@
initWithMinCenterCoordinateDistanceSelector :: Selector '[CDouble] (Id MKMapCameraZoomRange)
initWithMinCenterCoordinateDistanceSelector = mkSelector "initWithMinCenterCoordinateDistance:"

-- | @Selector@ for @initWithMaxCenterCoordinateDistance:@
initWithMaxCenterCoordinateDistanceSelector :: Selector '[CDouble] (Id MKMapCameraZoomRange)
initWithMaxCenterCoordinateDistanceSelector = mkSelector "initWithMaxCenterCoordinateDistance:"

-- | @Selector@ for @minCenterCoordinateDistance@
minCenterCoordinateDistanceSelector :: Selector '[] CDouble
minCenterCoordinateDistanceSelector = mkSelector "minCenterCoordinateDistance"

-- | @Selector@ for @maxCenterCoordinateDistance@
maxCenterCoordinateDistanceSelector :: Selector '[] CDouble
maxCenterCoordinateDistanceSelector = mkSelector "maxCenterCoordinateDistance"

