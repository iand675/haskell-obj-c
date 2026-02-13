{-# LANGUAGE DataKinds #-}
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
  , mapViewSelector
  , setMapViewSelector
  , zoomControlWithMapViewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' zoomControlWithMapViewSelector (toMKMapView mapView)

-- | @- mapView@
mapView :: IsMKZoomControl mkZoomControl => mkZoomControl -> IO (Id MKMapView)
mapView mkZoomControl =
  sendMessage mkZoomControl mapViewSelector

-- | @- setMapView:@
setMapView :: (IsMKZoomControl mkZoomControl, IsMKMapView value) => mkZoomControl -> value -> IO ()
setMapView mkZoomControl value =
  sendMessage mkZoomControl setMapViewSelector (toMKMapView value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoomControlWithMapView:@
zoomControlWithMapViewSelector :: Selector '[Id MKMapView] (Id MKZoomControl)
zoomControlWithMapViewSelector = mkSelector "zoomControlWithMapView:"

-- | @Selector@ for @mapView@
mapViewSelector :: Selector '[] (Id MKMapView)
mapViewSelector = mkSelector "mapView"

-- | @Selector@ for @setMapView:@
setMapViewSelector :: Selector '[Id MKMapView] ()
setMapViewSelector = mkSelector "setMapView:"

