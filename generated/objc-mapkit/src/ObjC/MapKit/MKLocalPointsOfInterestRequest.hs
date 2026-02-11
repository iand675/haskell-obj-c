{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLocalPointsOfInterestRequest@.
module ObjC.MapKit.MKLocalPointsOfInterestRequest
  ( MKLocalPointsOfInterestRequest
  , IsMKLocalPointsOfInterestRequest(..)
  , init_
  , initWithCenterCoordinate_radius
  , coordinate
  , radius
  , pointOfInterestFilter
  , setPointOfInterestFilter
  , initSelector
  , initWithCenterCoordinate_radiusSelector
  , coordinateSelector
  , radiusSelector
  , pointOfInterestFilterSelector
  , setPointOfInterestFilterSelector


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

-- | @- init@
init_ :: IsMKLocalPointsOfInterestRequest mkLocalPointsOfInterestRequest => mkLocalPointsOfInterestRequest -> IO (Id MKLocalPointsOfInterestRequest)
init_ mkLocalPointsOfInterestRequest  =
  sendMsg mkLocalPointsOfInterestRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCenterCoordinate:radius:@
initWithCenterCoordinate_radius :: IsMKLocalPointsOfInterestRequest mkLocalPointsOfInterestRequest => mkLocalPointsOfInterestRequest -> CLLocationCoordinate2D -> CDouble -> IO (Id MKLocalPointsOfInterestRequest)
initWithCenterCoordinate_radius mkLocalPointsOfInterestRequest  coordinate radius =
  sendMsg mkLocalPointsOfInterestRequest (mkSelector "initWithCenterCoordinate:radius:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate, argCDouble (fromIntegral radius)] >>= ownedObject . castPtr

-- | @- coordinate@
coordinate :: IsMKLocalPointsOfInterestRequest mkLocalPointsOfInterestRequest => mkLocalPointsOfInterestRequest -> IO CLLocationCoordinate2D
coordinate mkLocalPointsOfInterestRequest  =
  sendMsgStret mkLocalPointsOfInterestRequest (mkSelector "coordinate") retCLLocationCoordinate2D []

-- | @- radius@
radius :: IsMKLocalPointsOfInterestRequest mkLocalPointsOfInterestRequest => mkLocalPointsOfInterestRequest -> IO CDouble
radius mkLocalPointsOfInterestRequest  =
  sendMsg mkLocalPointsOfInterestRequest (mkSelector "radius") retCDouble []

-- | @- pointOfInterestFilter@
pointOfInterestFilter :: IsMKLocalPointsOfInterestRequest mkLocalPointsOfInterestRequest => mkLocalPointsOfInterestRequest -> IO (Id MKPointOfInterestFilter)
pointOfInterestFilter mkLocalPointsOfInterestRequest  =
  sendMsg mkLocalPointsOfInterestRequest (mkSelector "pointOfInterestFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPointOfInterestFilter:@
setPointOfInterestFilter :: (IsMKLocalPointsOfInterestRequest mkLocalPointsOfInterestRequest, IsMKPointOfInterestFilter value) => mkLocalPointsOfInterestRequest -> value -> IO ()
setPointOfInterestFilter mkLocalPointsOfInterestRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkLocalPointsOfInterestRequest (mkSelector "setPointOfInterestFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCenterCoordinate:radius:@
initWithCenterCoordinate_radiusSelector :: Selector
initWithCenterCoordinate_radiusSelector = mkSelector "initWithCenterCoordinate:radius:"

-- | @Selector@ for @coordinate@
coordinateSelector :: Selector
coordinateSelector = mkSelector "coordinate"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @pointOfInterestFilter@
pointOfInterestFilterSelector :: Selector
pointOfInterestFilterSelector = mkSelector "pointOfInterestFilter"

-- | @Selector@ for @setPointOfInterestFilter:@
setPointOfInterestFilterSelector :: Selector
setPointOfInterestFilterSelector = mkSelector "setPointOfInterestFilter:"

