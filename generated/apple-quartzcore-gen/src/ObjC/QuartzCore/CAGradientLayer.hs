{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAGradientLayer@.
module ObjC.QuartzCore.CAGradientLayer
  ( CAGradientLayer
  , IsCAGradientLayer(..)
  , colors
  , setColors
  , locations
  , setLocations
  , type_
  , setType
  , colorsSelector
  , locationsSelector
  , setColorsSelector
  , setLocationsSelector
  , setTypeSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- colors@
colors :: IsCAGradientLayer caGradientLayer => caGradientLayer -> IO (Id NSArray)
colors caGradientLayer =
  sendMessage caGradientLayer colorsSelector

-- | @- setColors:@
setColors :: (IsCAGradientLayer caGradientLayer, IsNSArray value) => caGradientLayer -> value -> IO ()
setColors caGradientLayer value =
  sendMessage caGradientLayer setColorsSelector (toNSArray value)

-- | @- locations@
locations :: IsCAGradientLayer caGradientLayer => caGradientLayer -> IO (Id NSArray)
locations caGradientLayer =
  sendMessage caGradientLayer locationsSelector

-- | @- setLocations:@
setLocations :: (IsCAGradientLayer caGradientLayer, IsNSArray value) => caGradientLayer -> value -> IO ()
setLocations caGradientLayer value =
  sendMessage caGradientLayer setLocationsSelector (toNSArray value)

-- | @- type@
type_ :: IsCAGradientLayer caGradientLayer => caGradientLayer -> IO (Id NSString)
type_ caGradientLayer =
  sendMessage caGradientLayer typeSelector

-- | @- setType:@
setType :: (IsCAGradientLayer caGradientLayer, IsNSString value) => caGradientLayer -> value -> IO ()
setType caGradientLayer value =
  sendMessage caGradientLayer setTypeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colors@
colorsSelector :: Selector '[] (Id NSArray)
colorsSelector = mkSelector "colors"

-- | @Selector@ for @setColors:@
setColorsSelector :: Selector '[Id NSArray] ()
setColorsSelector = mkSelector "setColors:"

-- | @Selector@ for @locations@
locationsSelector :: Selector '[] (Id NSArray)
locationsSelector = mkSelector "locations"

-- | @Selector@ for @setLocations:@
setLocationsSelector :: Selector '[Id NSArray] ()
setLocationsSelector = mkSelector "setLocations:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

