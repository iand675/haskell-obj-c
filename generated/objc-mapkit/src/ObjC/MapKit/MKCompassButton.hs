{-# LANGUAGE PatternSynonyms #-}
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
  , mapViewSelector
  , setMapViewSelector
  , compassVisibilitySelector
  , setCompassVisibilitySelector

  -- * Enum types
  , MKFeatureVisibility(MKFeatureVisibility)
  , pattern MKFeatureVisibilityAdaptive
  , pattern MKFeatureVisibilityHidden
  , pattern MKFeatureVisibilityVisible

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
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ compassButtonWithMapView:@
compassButtonWithMapView :: IsMKMapView mapView => mapView -> IO (Id MKCompassButton)
compassButtonWithMapView mapView =
  do
    cls' <- getRequiredClass "MKCompassButton"
    withObjCPtr mapView $ \raw_mapView ->
      sendClassMsg cls' (mkSelector "compassButtonWithMapView:") (retPtr retVoid) [argPtr (castPtr raw_mapView :: Ptr ())] >>= retainedObject . castPtr

-- | @- mapView@
mapView :: IsMKCompassButton mkCompassButton => mkCompassButton -> IO (Id MKMapView)
mapView mkCompassButton  =
  sendMsg mkCompassButton (mkSelector "mapView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMapView:@
setMapView :: (IsMKCompassButton mkCompassButton, IsMKMapView value) => mkCompassButton -> value -> IO ()
setMapView mkCompassButton  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkCompassButton (mkSelector "setMapView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- compassVisibility@
compassVisibility :: IsMKCompassButton mkCompassButton => mkCompassButton -> IO MKFeatureVisibility
compassVisibility mkCompassButton  =
  fmap (coerce :: CLong -> MKFeatureVisibility) $ sendMsg mkCompassButton (mkSelector "compassVisibility") retCLong []

-- | @- setCompassVisibility:@
setCompassVisibility :: IsMKCompassButton mkCompassButton => mkCompassButton -> MKFeatureVisibility -> IO ()
setCompassVisibility mkCompassButton  value =
  sendMsg mkCompassButton (mkSelector "setCompassVisibility:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compassButtonWithMapView:@
compassButtonWithMapViewSelector :: Selector
compassButtonWithMapViewSelector = mkSelector "compassButtonWithMapView:"

-- | @Selector@ for @mapView@
mapViewSelector :: Selector
mapViewSelector = mkSelector "mapView"

-- | @Selector@ for @setMapView:@
setMapViewSelector :: Selector
setMapViewSelector = mkSelector "setMapView:"

-- | @Selector@ for @compassVisibility@
compassVisibilitySelector :: Selector
compassVisibilitySelector = mkSelector "compassVisibility"

-- | @Selector@ for @setCompassVisibility:@
setCompassVisibilitySelector :: Selector
setCompassVisibilitySelector = mkSelector "setCompassVisibility:"

