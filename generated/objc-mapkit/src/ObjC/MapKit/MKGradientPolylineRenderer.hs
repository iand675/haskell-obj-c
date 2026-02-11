{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKGradientPolylineRenderer@.
module ObjC.MapKit.MKGradientPolylineRenderer
  ( MKGradientPolylineRenderer
  , IsMKGradientPolylineRenderer(..)
  , setColors_atLocations
  , colors
  , setColors_atLocationsSelector
  , colorsSelector


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

-- | @- setColors:atLocations:@
setColors_atLocations :: (IsMKGradientPolylineRenderer mkGradientPolylineRenderer, IsNSArray colors, IsNSArray locations) => mkGradientPolylineRenderer -> colors -> locations -> IO ()
setColors_atLocations mkGradientPolylineRenderer  colors locations =
withObjCPtr colors $ \raw_colors ->
  withObjCPtr locations $ \raw_locations ->
      sendMsg mkGradientPolylineRenderer (mkSelector "setColors:atLocations:") retVoid [argPtr (castPtr raw_colors :: Ptr ()), argPtr (castPtr raw_locations :: Ptr ())]

-- | @- colors@
colors :: IsMKGradientPolylineRenderer mkGradientPolylineRenderer => mkGradientPolylineRenderer -> IO (Id NSArray)
colors mkGradientPolylineRenderer  =
  sendMsg mkGradientPolylineRenderer (mkSelector "colors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setColors:atLocations:@
setColors_atLocationsSelector :: Selector
setColors_atLocationsSelector = mkSelector "setColors:atLocations:"

-- | @Selector@ for @colors@
colorsSelector :: Selector
colorsSelector = mkSelector "colors"

