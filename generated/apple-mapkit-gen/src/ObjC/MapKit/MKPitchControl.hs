{-# LANGUAGE DataKinds #-}
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
  , mapViewSelector
  , pitchControlWithMapViewSelector
  , setMapViewSelector


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

-- | @+ pitchControlWithMapView:@
pitchControlWithMapView :: IsMKMapView mapView => mapView -> IO (Id MKPitchControl)
pitchControlWithMapView mapView =
  do
    cls' <- getRequiredClass "MKPitchControl"
    sendClassMessage cls' pitchControlWithMapViewSelector (toMKMapView mapView)

-- | @- mapView@
mapView :: IsMKPitchControl mkPitchControl => mkPitchControl -> IO (Id MKMapView)
mapView mkPitchControl =
  sendMessage mkPitchControl mapViewSelector

-- | @- setMapView:@
setMapView :: (IsMKPitchControl mkPitchControl, IsMKMapView value) => mkPitchControl -> value -> IO ()
setMapView mkPitchControl value =
  sendMessage mkPitchControl setMapViewSelector (toMKMapView value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pitchControlWithMapView:@
pitchControlWithMapViewSelector :: Selector '[Id MKMapView] (Id MKPitchControl)
pitchControlWithMapViewSelector = mkSelector "pitchControlWithMapView:"

-- | @Selector@ for @mapView@
mapViewSelector :: Selector '[] (Id MKMapView)
mapViewSelector = mkSelector "mapView"

-- | @Selector@ for @setMapView:@
setMapViewSelector :: Selector '[Id MKMapView] ()
setMapViewSelector = mkSelector "setMapView:"

