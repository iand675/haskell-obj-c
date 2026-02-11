{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapCamera@.
module ObjC.MapKit.MKMapCamera
  ( MKMapCamera
  , IsMKMapCamera(..)
  , camera
  , cameraLookingAtCenterCoordinate_fromEyeCoordinate_eyeAltitude
  , cameraLookingAtCenterCoordinate_fromDistance_pitch_heading
  , centerCoordinate
  , setCenterCoordinate
  , centerCoordinateDistance
  , setCenterCoordinateDistance
  , heading
  , setHeading
  , pitch
  , setPitch
  , altitude
  , setAltitude
  , cameraSelector
  , cameraLookingAtCenterCoordinate_fromEyeCoordinate_eyeAltitudeSelector
  , cameraLookingAtCenterCoordinate_fromDistance_pitch_headingSelector
  , centerCoordinateSelector
  , setCenterCoordinateSelector
  , centerCoordinateDistanceSelector
  , setCenterCoordinateDistanceSelector
  , headingSelector
  , setHeadingSelector
  , pitchSelector
  , setPitchSelector
  , altitudeSelector
  , setAltitudeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.CoreLocation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ camera@
camera :: IO (Id MKMapCamera)
camera  =
  do
    cls' <- getRequiredClass "MKMapCamera"
    sendClassMsg cls' (mkSelector "camera") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cameraLookingAtCenterCoordinate:fromEyeCoordinate:eyeAltitude:@
cameraLookingAtCenterCoordinate_fromEyeCoordinate_eyeAltitude :: CLLocationCoordinate2D -> CLLocationCoordinate2D -> CDouble -> IO (Id MKMapCamera)
cameraLookingAtCenterCoordinate_fromEyeCoordinate_eyeAltitude centerCoordinate eyeCoordinate eyeAltitude =
  do
    cls' <- getRequiredClass "MKMapCamera"
    sendClassMsg cls' (mkSelector "cameraLookingAtCenterCoordinate:fromEyeCoordinate:eyeAltitude:") (retPtr retVoid) [argCLLocationCoordinate2D centerCoordinate, argCLLocationCoordinate2D eyeCoordinate, argCDouble (fromIntegral eyeAltitude)] >>= retainedObject . castPtr

-- | @+ cameraLookingAtCenterCoordinate:fromDistance:pitch:heading:@
cameraLookingAtCenterCoordinate_fromDistance_pitch_heading :: CLLocationCoordinate2D -> CDouble -> CDouble -> CDouble -> IO (Id MKMapCamera)
cameraLookingAtCenterCoordinate_fromDistance_pitch_heading centerCoordinate distance pitch heading =
  do
    cls' <- getRequiredClass "MKMapCamera"
    sendClassMsg cls' (mkSelector "cameraLookingAtCenterCoordinate:fromDistance:pitch:heading:") (retPtr retVoid) [argCLLocationCoordinate2D centerCoordinate, argCDouble (fromIntegral distance), argCDouble (fromIntegral pitch), argCDouble (fromIntegral heading)] >>= retainedObject . castPtr

-- | @- centerCoordinate@
centerCoordinate :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CLLocationCoordinate2D
centerCoordinate mkMapCamera  =
  sendMsgStret mkMapCamera (mkSelector "centerCoordinate") retCLLocationCoordinate2D []

-- | @- setCenterCoordinate:@
setCenterCoordinate :: IsMKMapCamera mkMapCamera => mkMapCamera -> CLLocationCoordinate2D -> IO ()
setCenterCoordinate mkMapCamera  value =
  sendMsg mkMapCamera (mkSelector "setCenterCoordinate:") retVoid [argCLLocationCoordinate2D value]

-- | @- centerCoordinateDistance@
centerCoordinateDistance :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
centerCoordinateDistance mkMapCamera  =
  sendMsg mkMapCamera (mkSelector "centerCoordinateDistance") retCDouble []

-- | @- setCenterCoordinateDistance:@
setCenterCoordinateDistance :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setCenterCoordinateDistance mkMapCamera  value =
  sendMsg mkMapCamera (mkSelector "setCenterCoordinateDistance:") retVoid [argCDouble (fromIntegral value)]

-- | @- heading@
heading :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
heading mkMapCamera  =
  sendMsg mkMapCamera (mkSelector "heading") retCDouble []

-- | @- setHeading:@
setHeading :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setHeading mkMapCamera  value =
  sendMsg mkMapCamera (mkSelector "setHeading:") retVoid [argCDouble (fromIntegral value)]

-- | @- pitch@
pitch :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
pitch mkMapCamera  =
  sendMsg mkMapCamera (mkSelector "pitch") retCDouble []

-- | @- setPitch:@
setPitch :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setPitch mkMapCamera  value =
  sendMsg mkMapCamera (mkSelector "setPitch:") retVoid [argCDouble (fromIntegral value)]

-- | @- altitude@
altitude :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
altitude mkMapCamera  =
  sendMsg mkMapCamera (mkSelector "altitude") retCDouble []

-- | @- setAltitude:@
setAltitude :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setAltitude mkMapCamera  value =
  sendMsg mkMapCamera (mkSelector "setAltitude:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @camera@
cameraSelector :: Selector
cameraSelector = mkSelector "camera"

-- | @Selector@ for @cameraLookingAtCenterCoordinate:fromEyeCoordinate:eyeAltitude:@
cameraLookingAtCenterCoordinate_fromEyeCoordinate_eyeAltitudeSelector :: Selector
cameraLookingAtCenterCoordinate_fromEyeCoordinate_eyeAltitudeSelector = mkSelector "cameraLookingAtCenterCoordinate:fromEyeCoordinate:eyeAltitude:"

-- | @Selector@ for @cameraLookingAtCenterCoordinate:fromDistance:pitch:heading:@
cameraLookingAtCenterCoordinate_fromDistance_pitch_headingSelector :: Selector
cameraLookingAtCenterCoordinate_fromDistance_pitch_headingSelector = mkSelector "cameraLookingAtCenterCoordinate:fromDistance:pitch:heading:"

-- | @Selector@ for @centerCoordinate@
centerCoordinateSelector :: Selector
centerCoordinateSelector = mkSelector "centerCoordinate"

-- | @Selector@ for @setCenterCoordinate:@
setCenterCoordinateSelector :: Selector
setCenterCoordinateSelector = mkSelector "setCenterCoordinate:"

-- | @Selector@ for @centerCoordinateDistance@
centerCoordinateDistanceSelector :: Selector
centerCoordinateDistanceSelector = mkSelector "centerCoordinateDistance"

-- | @Selector@ for @setCenterCoordinateDistance:@
setCenterCoordinateDistanceSelector :: Selector
setCenterCoordinateDistanceSelector = mkSelector "setCenterCoordinateDistance:"

-- | @Selector@ for @heading@
headingSelector :: Selector
headingSelector = mkSelector "heading"

-- | @Selector@ for @setHeading:@
setHeadingSelector :: Selector
setHeadingSelector = mkSelector "setHeading:"

-- | @Selector@ for @pitch@
pitchSelector :: Selector
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @setPitch:@
setPitchSelector :: Selector
setPitchSelector = mkSelector "setPitch:"

-- | @Selector@ for @altitude@
altitudeSelector :: Selector
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @setAltitude:@
setAltitudeSelector :: Selector
setAltitudeSelector = mkSelector "setAltitude:"

