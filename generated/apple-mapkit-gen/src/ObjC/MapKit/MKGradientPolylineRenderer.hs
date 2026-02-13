{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKGradientPolylineRenderer@.
module ObjC.MapKit.MKGradientPolylineRenderer
  ( MKGradientPolylineRenderer
  , IsMKGradientPolylineRenderer(..)
  , setColors_atLocations
  , locations
  , colors
  , colorsSelector
  , locationsSelector
  , setColors_atLocationsSelector


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

-- | @- setColors:atLocations:@
setColors_atLocations :: (IsMKGradientPolylineRenderer mkGradientPolylineRenderer, IsNSArray colors, IsNSArray locations) => mkGradientPolylineRenderer -> colors -> locations -> IO ()
setColors_atLocations mkGradientPolylineRenderer colors locations =
  sendMessage mkGradientPolylineRenderer setColors_atLocationsSelector (toNSArray colors) (toNSArray locations)

-- | @- locations@
locations :: IsMKGradientPolylineRenderer mkGradientPolylineRenderer => mkGradientPolylineRenderer -> IO (Id NSArray)
locations mkGradientPolylineRenderer =
  sendMessage mkGradientPolylineRenderer locationsSelector

-- | @- colors@
colors :: IsMKGradientPolylineRenderer mkGradientPolylineRenderer => mkGradientPolylineRenderer -> IO (Id NSArray)
colors mkGradientPolylineRenderer =
  sendMessage mkGradientPolylineRenderer colorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setColors:atLocations:@
setColors_atLocationsSelector :: Selector '[Id NSArray, Id NSArray] ()
setColors_atLocationsSelector = mkSelector "setColors:atLocations:"

-- | @Selector@ for @locations@
locationsSelector :: Selector '[] (Id NSArray)
locationsSelector = mkSelector "locations"

-- | @Selector@ for @colors@
colorsSelector :: Selector '[] (Id NSArray)
colorsSelector = mkSelector "colors"

