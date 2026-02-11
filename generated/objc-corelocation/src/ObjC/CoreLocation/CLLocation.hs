{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLLocation@.
module ObjC.CoreLocation.CLLocation
  ( CLLocation
  , IsCLLocation(..)
  , initWithLatitude_longitude
  , initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_timestamp
  , initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_speed_timestamp
  , initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestamp
  , initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestamp_sourceInfo
  , getDistanceFrom
  , distanceFromLocation
  , coordinate
  , altitude
  , ellipsoidalAltitude
  , horizontalAccuracy
  , verticalAccuracy
  , course
  , courseAccuracy
  , speed
  , speedAccuracy
  , timestamp
  , floor
  , sourceInformation
  , initWithLatitude_longitudeSelector
  , initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_timestampSelector
  , initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_speed_timestampSelector
  , initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestampSelector
  , initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestamp_sourceInfoSelector
  , getDistanceFromSelector
  , distanceFromLocationSelector
  , coordinateSelector
  , altitudeSelector
  , ellipsoidalAltitudeSelector
  , horizontalAccuracySelector
  , verticalAccuracySelector
  , courseSelector
  , courseAccuracySelector
  , speedSelector
  , speedAccuracySelector
  , timestampSelector
  , floorSelector
  , sourceInformationSelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithLatitude:longitude:@
initWithLatitude_longitude :: IsCLLocation clLocation => clLocation -> CDouble -> CDouble -> IO (Id CLLocation)
initWithLatitude_longitude clLocation  latitude longitude =
  sendMsg clLocation (mkSelector "initWithLatitude:longitude:") (retPtr retVoid) [argCDouble (fromIntegral latitude), argCDouble (fromIntegral longitude)] >>= ownedObject . castPtr

-- | @- initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:timestamp:@
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_timestamp :: (IsCLLocation clLocation, IsNSDate timestamp) => clLocation -> CLLocationCoordinate2D -> CDouble -> CDouble -> CDouble -> timestamp -> IO (Id CLLocation)
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_timestamp clLocation  coordinate altitude hAccuracy vAccuracy timestamp =
withObjCPtr timestamp $ \raw_timestamp ->
    sendMsg clLocation (mkSelector "initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:timestamp:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate, argCDouble (fromIntegral altitude), argCDouble (fromIntegral hAccuracy), argCDouble (fromIntegral vAccuracy), argPtr (castPtr raw_timestamp :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:speed:timestamp:@
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_speed_timestamp :: (IsCLLocation clLocation, IsNSDate timestamp) => clLocation -> CLLocationCoordinate2D -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> timestamp -> IO (Id CLLocation)
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_speed_timestamp clLocation  coordinate altitude hAccuracy vAccuracy course speed timestamp =
withObjCPtr timestamp $ \raw_timestamp ->
    sendMsg clLocation (mkSelector "initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:speed:timestamp:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate, argCDouble (fromIntegral altitude), argCDouble (fromIntegral hAccuracy), argCDouble (fromIntegral vAccuracy), argCDouble (fromIntegral course), argCDouble (fromIntegral speed), argPtr (castPtr raw_timestamp :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:courseAccuracy:speed:speedAccuracy:timestamp:@
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestamp :: (IsCLLocation clLocation, IsNSDate timestamp) => clLocation -> CLLocationCoordinate2D -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> timestamp -> IO (Id CLLocation)
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestamp clLocation  coordinate altitude hAccuracy vAccuracy course courseAccuracy speed speedAccuracy timestamp =
withObjCPtr timestamp $ \raw_timestamp ->
    sendMsg clLocation (mkSelector "initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:courseAccuracy:speed:speedAccuracy:timestamp:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate, argCDouble (fromIntegral altitude), argCDouble (fromIntegral hAccuracy), argCDouble (fromIntegral vAccuracy), argCDouble (fromIntegral course), argCDouble (fromIntegral courseAccuracy), argCDouble (fromIntegral speed), argCDouble (fromIntegral speedAccuracy), argPtr (castPtr raw_timestamp :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:courseAccuracy:speed:speedAccuracy:timestamp:sourceInfo:@
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestamp_sourceInfo :: (IsCLLocation clLocation, IsNSDate timestamp, IsCLLocationSourceInformation sourceInfo) => clLocation -> CLLocationCoordinate2D -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> timestamp -> sourceInfo -> IO (Id CLLocation)
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestamp_sourceInfo clLocation  coordinate altitude hAccuracy vAccuracy course courseAccuracy speed speedAccuracy timestamp sourceInfo =
withObjCPtr timestamp $ \raw_timestamp ->
  withObjCPtr sourceInfo $ \raw_sourceInfo ->
      sendMsg clLocation (mkSelector "initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:courseAccuracy:speed:speedAccuracy:timestamp:sourceInfo:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate, argCDouble (fromIntegral altitude), argCDouble (fromIntegral hAccuracy), argCDouble (fromIntegral vAccuracy), argCDouble (fromIntegral course), argCDouble (fromIntegral courseAccuracy), argCDouble (fromIntegral speed), argCDouble (fromIntegral speedAccuracy), argPtr (castPtr raw_timestamp :: Ptr ()), argPtr (castPtr raw_sourceInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- getDistanceFrom:@
getDistanceFrom :: IsCLLocation clLocation => clLocation -> Const (Id CLLocation) -> IO CDouble
getDistanceFrom clLocation  location =
withObjCPtr location $ \raw_location ->
    sendMsg clLocation (mkSelector "getDistanceFrom:") retCDouble [argPtr (castPtr raw_location :: Ptr ())]

-- | @- distanceFromLocation:@
distanceFromLocation :: IsCLLocation clLocation => clLocation -> Const (Id CLLocation) -> IO CDouble
distanceFromLocation clLocation  location =
withObjCPtr location $ \raw_location ->
    sendMsg clLocation (mkSelector "distanceFromLocation:") retCDouble [argPtr (castPtr raw_location :: Ptr ())]

-- | @- coordinate@
coordinate :: IsCLLocation clLocation => clLocation -> IO CLLocationCoordinate2D
coordinate clLocation  =
  sendMsgStret clLocation (mkSelector "coordinate") retCLLocationCoordinate2D []

-- | @- altitude@
altitude :: IsCLLocation clLocation => clLocation -> IO CDouble
altitude clLocation  =
  sendMsg clLocation (mkSelector "altitude") retCDouble []

-- | @- ellipsoidalAltitude@
ellipsoidalAltitude :: IsCLLocation clLocation => clLocation -> IO CDouble
ellipsoidalAltitude clLocation  =
  sendMsg clLocation (mkSelector "ellipsoidalAltitude") retCDouble []

-- | @- horizontalAccuracy@
horizontalAccuracy :: IsCLLocation clLocation => clLocation -> IO CDouble
horizontalAccuracy clLocation  =
  sendMsg clLocation (mkSelector "horizontalAccuracy") retCDouble []

-- | @- verticalAccuracy@
verticalAccuracy :: IsCLLocation clLocation => clLocation -> IO CDouble
verticalAccuracy clLocation  =
  sendMsg clLocation (mkSelector "verticalAccuracy") retCDouble []

-- | @- course@
course :: IsCLLocation clLocation => clLocation -> IO CDouble
course clLocation  =
  sendMsg clLocation (mkSelector "course") retCDouble []

-- | @- courseAccuracy@
courseAccuracy :: IsCLLocation clLocation => clLocation -> IO CDouble
courseAccuracy clLocation  =
  sendMsg clLocation (mkSelector "courseAccuracy") retCDouble []

-- | @- speed@
speed :: IsCLLocation clLocation => clLocation -> IO CDouble
speed clLocation  =
  sendMsg clLocation (mkSelector "speed") retCDouble []

-- | @- speedAccuracy@
speedAccuracy :: IsCLLocation clLocation => clLocation -> IO CDouble
speedAccuracy clLocation  =
  sendMsg clLocation (mkSelector "speedAccuracy") retCDouble []

-- | @- timestamp@
timestamp :: IsCLLocation clLocation => clLocation -> IO (Id NSDate)
timestamp clLocation  =
  sendMsg clLocation (mkSelector "timestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- floor@
floor :: IsCLLocation clLocation => clLocation -> IO (Id CLFloor)
floor clLocation  =
  sendMsg clLocation (mkSelector "floor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sourceInformation@
sourceInformation :: IsCLLocation clLocation => clLocation -> IO (Id CLLocationSourceInformation)
sourceInformation clLocation  =
  sendMsg clLocation (mkSelector "sourceInformation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLatitude:longitude:@
initWithLatitude_longitudeSelector :: Selector
initWithLatitude_longitudeSelector = mkSelector "initWithLatitude:longitude:"

-- | @Selector@ for @initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:timestamp:@
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_timestampSelector :: Selector
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_timestampSelector = mkSelector "initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:timestamp:"

-- | @Selector@ for @initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:speed:timestamp:@
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_speed_timestampSelector :: Selector
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_speed_timestampSelector = mkSelector "initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:speed:timestamp:"

-- | @Selector@ for @initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:courseAccuracy:speed:speedAccuracy:timestamp:@
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestampSelector :: Selector
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestampSelector = mkSelector "initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:courseAccuracy:speed:speedAccuracy:timestamp:"

-- | @Selector@ for @initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:courseAccuracy:speed:speedAccuracy:timestamp:sourceInfo:@
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestamp_sourceInfoSelector :: Selector
initWithCoordinate_altitude_horizontalAccuracy_verticalAccuracy_course_courseAccuracy_speed_speedAccuracy_timestamp_sourceInfoSelector = mkSelector "initWithCoordinate:altitude:horizontalAccuracy:verticalAccuracy:course:courseAccuracy:speed:speedAccuracy:timestamp:sourceInfo:"

-- | @Selector@ for @getDistanceFrom:@
getDistanceFromSelector :: Selector
getDistanceFromSelector = mkSelector "getDistanceFrom:"

-- | @Selector@ for @distanceFromLocation:@
distanceFromLocationSelector :: Selector
distanceFromLocationSelector = mkSelector "distanceFromLocation:"

-- | @Selector@ for @coordinate@
coordinateSelector :: Selector
coordinateSelector = mkSelector "coordinate"

-- | @Selector@ for @altitude@
altitudeSelector :: Selector
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @ellipsoidalAltitude@
ellipsoidalAltitudeSelector :: Selector
ellipsoidalAltitudeSelector = mkSelector "ellipsoidalAltitude"

-- | @Selector@ for @horizontalAccuracy@
horizontalAccuracySelector :: Selector
horizontalAccuracySelector = mkSelector "horizontalAccuracy"

-- | @Selector@ for @verticalAccuracy@
verticalAccuracySelector :: Selector
verticalAccuracySelector = mkSelector "verticalAccuracy"

-- | @Selector@ for @course@
courseSelector :: Selector
courseSelector = mkSelector "course"

-- | @Selector@ for @courseAccuracy@
courseAccuracySelector :: Selector
courseAccuracySelector = mkSelector "courseAccuracy"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @speedAccuracy@
speedAccuracySelector :: Selector
speedAccuracySelector = mkSelector "speedAccuracy"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @floor@
floorSelector :: Selector
floorSelector = mkSelector "floor"

-- | @Selector@ for @sourceInformation@
sourceInformationSelector :: Selector
sourceInformationSelector = mkSelector "sourceInformation"

