{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKCompassButton@.
module ObjC.MapKit.MKCompassButton
  ( MKCompassButton
  , IsMKCompassButton(..)
  , compassButtonWithMapView
  , mapView
  , setMapView
  , compassVisibility
  , setCompassVisibility
  , compassButtonWithMapViewSelector
  , compassVisibilitySelector
  , mapViewSelector
  , setCompassVisibilitySelector
  , setMapViewSelector

  -- * Enum types
  , MKFeatureVisibility(MKFeatureVisibility)
  , pattern MKFeatureVisibilityAdaptive
  , pattern MKFeatureVisibilityHidden
  , pattern MKFeatureVisibilityVisible

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ compassButtonWithMapView:@
compassButtonWithMapView :: IsMKMapView mapView => mapView -> IO (Id MKCompassButton)
compassButtonWithMapView mapView =
  do
    cls' <- getRequiredClass "MKCompassButton"
    sendClassMessage cls' compassButtonWithMapViewSelector (toMKMapView mapView)

-- | @- mapView@
mapView :: IsMKCompassButton mkCompassButton => mkCompassButton -> IO (Id MKMapView)
mapView mkCompassButton =
  sendMessage mkCompassButton mapViewSelector

-- | @- setMapView:@
setMapView :: (IsMKCompassButton mkCompassButton, IsMKMapView value) => mkCompassButton -> value -> IO ()
setMapView mkCompassButton value =
  sendMessage mkCompassButton setMapViewSelector (toMKMapView value)

-- | @- compassVisibility@
compassVisibility :: IsMKCompassButton mkCompassButton => mkCompassButton -> IO MKFeatureVisibility
compassVisibility mkCompassButton =
  sendMessage mkCompassButton compassVisibilitySelector

-- | @- setCompassVisibility:@
setCompassVisibility :: IsMKCompassButton mkCompassButton => mkCompassButton -> MKFeatureVisibility -> IO ()
setCompassVisibility mkCompassButton value =
  sendMessage mkCompassButton setCompassVisibilitySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compassButtonWithMapView:@
compassButtonWithMapViewSelector :: Selector '[Id MKMapView] (Id MKCompassButton)
compassButtonWithMapViewSelector = mkSelector "compassButtonWithMapView:"

-- | @Selector@ for @mapView@
mapViewSelector :: Selector '[] (Id MKMapView)
mapViewSelector = mkSelector "mapView"

-- | @Selector@ for @setMapView:@
setMapViewSelector :: Selector '[Id MKMapView] ()
setMapViewSelector = mkSelector "setMapView:"

-- | @Selector@ for @compassVisibility@
compassVisibilitySelector :: Selector '[] MKFeatureVisibility
compassVisibilitySelector = mkSelector "compassVisibility"

-- | @Selector@ for @setCompassVisibility:@
setCompassVisibilitySelector :: Selector '[MKFeatureVisibility] ()
setCompassVisibilitySelector = mkSelector "setCompassVisibility:"

