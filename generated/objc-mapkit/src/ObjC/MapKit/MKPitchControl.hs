{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPitchControl@.
module ObjC.MapKit.MKPitchControl
  ( MKPitchControl
  , IsMKPitchControl(..)
  , pitchControlWithMapView
  , mapView
  , setMapView
  , pitchControlWithMapViewSelector
  , mapViewSelector
  , setMapViewSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ pitchControlWithMapView:@
pitchControlWithMapView :: IsMKMapView mapView => mapView -> IO (Id MKPitchControl)
pitchControlWithMapView mapView =
  do
    cls' <- getRequiredClass "MKPitchControl"
    withObjCPtr mapView $ \raw_mapView ->
      sendClassMsg cls' (mkSelector "pitchControlWithMapView:") (retPtr retVoid) [argPtr (castPtr raw_mapView :: Ptr ())] >>= retainedObject . castPtr

-- | @- mapView@
mapView :: IsMKPitchControl mkPitchControl => mkPitchControl -> IO (Id MKMapView)
mapView mkPitchControl  =
  sendMsg mkPitchControl (mkSelector "mapView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMapView:@
setMapView :: (IsMKPitchControl mkPitchControl, IsMKMapView value) => mkPitchControl -> value -> IO ()
setMapView mkPitchControl  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkPitchControl (mkSelector "setMapView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pitchControlWithMapView:@
pitchControlWithMapViewSelector :: Selector
pitchControlWithMapViewSelector = mkSelector "pitchControlWithMapView:"

-- | @Selector@ for @mapView@
mapViewSelector :: Selector
mapViewSelector = mkSelector "mapView"

-- | @Selector@ for @setMapView:@
setMapViewSelector :: Selector
setMapViewSelector = mkSelector "setMapView:"

