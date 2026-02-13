{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapCamera@.
module ObjC.MapKit.MKMapCamera
  ( MKMapCamera
  , IsMKMapCamera(..)
  , camera
  , centerCoordinateDistance
  , setCenterCoordinateDistance
  , heading
  , setHeading
  , pitch
  , setPitch
  , altitude
  , setAltitude
  , altitudeSelector
  , cameraSelector
  , centerCoordinateDistanceSelector
  , headingSelector
  , pitchSelector
  , setAltitudeSelector
  , setCenterCoordinateDistanceSelector
  , setHeadingSelector
  , setPitchSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ camera@
camera :: IO (Id MKMapCamera)
camera  =
  do
    cls' <- getRequiredClass "MKMapCamera"
    sendClassMessage cls' cameraSelector

-- | @- centerCoordinateDistance@
centerCoordinateDistance :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
centerCoordinateDistance mkMapCamera =
  sendMessage mkMapCamera centerCoordinateDistanceSelector

-- | @- setCenterCoordinateDistance:@
setCenterCoordinateDistance :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setCenterCoordinateDistance mkMapCamera value =
  sendMessage mkMapCamera setCenterCoordinateDistanceSelector value

-- | @- heading@
heading :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
heading mkMapCamera =
  sendMessage mkMapCamera headingSelector

-- | @- setHeading:@
setHeading :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setHeading mkMapCamera value =
  sendMessage mkMapCamera setHeadingSelector value

-- | @- pitch@
pitch :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
pitch mkMapCamera =
  sendMessage mkMapCamera pitchSelector

-- | @- setPitch:@
setPitch :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setPitch mkMapCamera value =
  sendMessage mkMapCamera setPitchSelector value

-- | @- altitude@
altitude :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
altitude mkMapCamera =
  sendMessage mkMapCamera altitudeSelector

-- | @- setAltitude:@
setAltitude :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setAltitude mkMapCamera value =
  sendMessage mkMapCamera setAltitudeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @camera@
cameraSelector :: Selector '[] (Id MKMapCamera)
cameraSelector = mkSelector "camera"

-- | @Selector@ for @centerCoordinateDistance@
centerCoordinateDistanceSelector :: Selector '[] CDouble
centerCoordinateDistanceSelector = mkSelector "centerCoordinateDistance"

-- | @Selector@ for @setCenterCoordinateDistance:@
setCenterCoordinateDistanceSelector :: Selector '[CDouble] ()
setCenterCoordinateDistanceSelector = mkSelector "setCenterCoordinateDistance:"

-- | @Selector@ for @heading@
headingSelector :: Selector '[] CDouble
headingSelector = mkSelector "heading"

-- | @Selector@ for @setHeading:@
setHeadingSelector :: Selector '[CDouble] ()
setHeadingSelector = mkSelector "setHeading:"

-- | @Selector@ for @pitch@
pitchSelector :: Selector '[] CDouble
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @setPitch:@
setPitchSelector :: Selector '[CDouble] ()
setPitchSelector = mkSelector "setPitch:"

-- | @Selector@ for @altitude@
altitudeSelector :: Selector '[] CDouble
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @setAltitude:@
setAltitudeSelector :: Selector '[CDouble] ()
setAltitudeSelector = mkSelector "setAltitude:"

