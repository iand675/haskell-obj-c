{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKCircle@.
module ObjC.MapKit.MKCircle
  ( MKCircle
  , IsMKCircle(..)
  , circleWithCenterCoordinate_radius
  , coordinate
  , radius
  , circleWithCenterCoordinate_radiusSelector
  , coordinateSelector
  , radiusSelector


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

-- | @+ circleWithCenterCoordinate:radius:@
circleWithCenterCoordinate_radius :: CLLocationCoordinate2D -> CDouble -> IO (Id MKCircle)
circleWithCenterCoordinate_radius coord radius =
  do
    cls' <- getRequiredClass "MKCircle"
    sendClassMsg cls' (mkSelector "circleWithCenterCoordinate:radius:") (retPtr retVoid) [argCLLocationCoordinate2D coord, argCDouble (fromIntegral radius)] >>= retainedObject . castPtr

-- | @- coordinate@
coordinate :: IsMKCircle mkCircle => mkCircle -> IO CLLocationCoordinate2D
coordinate mkCircle  =
  sendMsgStret mkCircle (mkSelector "coordinate") retCLLocationCoordinate2D []

-- | @- radius@
radius :: IsMKCircle mkCircle => mkCircle -> IO CDouble
radius mkCircle  =
  sendMsg mkCircle (mkSelector "radius") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @circleWithCenterCoordinate:radius:@
circleWithCenterCoordinate_radiusSelector :: Selector
circleWithCenterCoordinate_radiusSelector = mkSelector "circleWithCenterCoordinate:radius:"

-- | @Selector@ for @coordinate@
coordinateSelector :: Selector
coordinateSelector = mkSelector "coordinate"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

