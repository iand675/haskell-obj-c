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
  , initWithMinCenterCoordinateDistance_maxCenterCoordinateDistanceSelector
  , initWithMinCenterCoordinateDistanceSelector
  , initWithMaxCenterCoordinateDistanceSelector
  , minCenterCoordinateDistanceSelector
  , maxCenterCoordinateDistanceSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMinCenterCoordinateDistance:maxCenterCoordinateDistance:@
initWithMinCenterCoordinateDistance_maxCenterCoordinateDistance :: IsMKMapCameraZoomRange mkMapCameraZoomRange => mkMapCameraZoomRange -> CDouble -> CDouble -> IO (Id MKMapCameraZoomRange)
initWithMinCenterCoordinateDistance_maxCenterCoordinateDistance mkMapCameraZoomRange  minDistance maxDistance =
  sendMsg mkMapCameraZoomRange (mkSelector "initWithMinCenterCoordinateDistance:maxCenterCoordinateDistance:") (retPtr retVoid) [argCDouble (fromIntegral minDistance), argCDouble (fromIntegral maxDistance)] >>= ownedObject . castPtr

-- | @- initWithMinCenterCoordinateDistance:@
initWithMinCenterCoordinateDistance :: IsMKMapCameraZoomRange mkMapCameraZoomRange => mkMapCameraZoomRange -> CDouble -> IO (Id MKMapCameraZoomRange)
initWithMinCenterCoordinateDistance mkMapCameraZoomRange  minDistance =
  sendMsg mkMapCameraZoomRange (mkSelector "initWithMinCenterCoordinateDistance:") (retPtr retVoid) [argCDouble (fromIntegral minDistance)] >>= ownedObject . castPtr

-- | @- initWithMaxCenterCoordinateDistance:@
initWithMaxCenterCoordinateDistance :: IsMKMapCameraZoomRange mkMapCameraZoomRange => mkMapCameraZoomRange -> CDouble -> IO (Id MKMapCameraZoomRange)
initWithMaxCenterCoordinateDistance mkMapCameraZoomRange  maxDistance =
  sendMsg mkMapCameraZoomRange (mkSelector "initWithMaxCenterCoordinateDistance:") (retPtr retVoid) [argCDouble (fromIntegral maxDistance)] >>= ownedObject . castPtr

-- | @- minCenterCoordinateDistance@
minCenterCoordinateDistance :: IsMKMapCameraZoomRange mkMapCameraZoomRange => mkMapCameraZoomRange -> IO CDouble
minCenterCoordinateDistance mkMapCameraZoomRange  =
  sendMsg mkMapCameraZoomRange (mkSelector "minCenterCoordinateDistance") retCDouble []

-- | @- maxCenterCoordinateDistance@
maxCenterCoordinateDistance :: IsMKMapCameraZoomRange mkMapCameraZoomRange => mkMapCameraZoomRange -> IO CDouble
maxCenterCoordinateDistance mkMapCameraZoomRange  =
  sendMsg mkMapCameraZoomRange (mkSelector "maxCenterCoordinateDistance") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMinCenterCoordinateDistance:maxCenterCoordinateDistance:@
initWithMinCenterCoordinateDistance_maxCenterCoordinateDistanceSelector :: Selector
initWithMinCenterCoordinateDistance_maxCenterCoordinateDistanceSelector = mkSelector "initWithMinCenterCoordinateDistance:maxCenterCoordinateDistance:"

-- | @Selector@ for @initWithMinCenterCoordinateDistance:@
initWithMinCenterCoordinateDistanceSelector :: Selector
initWithMinCenterCoordinateDistanceSelector = mkSelector "initWithMinCenterCoordinateDistance:"

-- | @Selector@ for @initWithMaxCenterCoordinateDistance:@
initWithMaxCenterCoordinateDistanceSelector :: Selector
initWithMaxCenterCoordinateDistanceSelector = mkSelector "initWithMaxCenterCoordinateDistance:"

-- | @Selector@ for @minCenterCoordinateDistance@
minCenterCoordinateDistanceSelector :: Selector
minCenterCoordinateDistanceSelector = mkSelector "minCenterCoordinateDistance"

-- | @Selector@ for @maxCenterCoordinateDistance@
maxCenterCoordinateDistanceSelector :: Selector
maxCenterCoordinateDistanceSelector = mkSelector "maxCenterCoordinateDistance"

