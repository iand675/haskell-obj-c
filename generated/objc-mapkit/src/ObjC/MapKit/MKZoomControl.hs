{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKZoomControl@.
module ObjC.MapKit.MKZoomControl
  ( MKZoomControl
  , IsMKZoomControl(..)
  , zoomControlWithMapView
  , mapView
  , setMapView
  , zoomControlWithMapViewSelector
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

-- | @+ zoomControlWithMapView:@
zoomControlWithMapView :: IsMKMapView mapView => mapView -> IO (Id MKZoomControl)
zoomControlWithMapView mapView =
  do
    cls' <- getRequiredClass "MKZoomControl"
    withObjCPtr mapView $ \raw_mapView ->
      sendClassMsg cls' (mkSelector "zoomControlWithMapView:") (retPtr retVoid) [argPtr (castPtr raw_mapView :: Ptr ())] >>= retainedObject . castPtr

-- | @- mapView@
mapView :: IsMKZoomControl mkZoomControl => mkZoomControl -> IO (Id MKMapView)
mapView mkZoomControl  =
  sendMsg mkZoomControl (mkSelector "mapView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMapView:@
setMapView :: (IsMKZoomControl mkZoomControl, IsMKMapView value) => mkZoomControl -> value -> IO ()
setMapView mkZoomControl  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkZoomControl (mkSelector "setMapView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoomControlWithMapView:@
zoomControlWithMapViewSelector :: Selector
zoomControlWithMapViewSelector = mkSelector "zoomControlWithMapView:"

-- | @Selector@ for @mapView@
mapViewSelector :: Selector
mapViewSelector = mkSelector "mapView"

-- | @Selector@ for @setMapView:@
setMapViewSelector :: Selector
setMapViewSelector = mkSelector "setMapView:"

