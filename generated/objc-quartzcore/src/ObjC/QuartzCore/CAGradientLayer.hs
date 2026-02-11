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
  , setColorsSelector
  , locationsSelector
  , setLocationsSelector
  , typeSelector
  , setTypeSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- colors@
colors :: IsCAGradientLayer caGradientLayer => caGradientLayer -> IO (Id NSArray)
colors caGradientLayer  =
  sendMsg caGradientLayer (mkSelector "colors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColors:@
setColors :: (IsCAGradientLayer caGradientLayer, IsNSArray value) => caGradientLayer -> value -> IO ()
setColors caGradientLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caGradientLayer (mkSelector "setColors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- locations@
locations :: IsCAGradientLayer caGradientLayer => caGradientLayer -> IO (Id NSArray)
locations caGradientLayer  =
  sendMsg caGradientLayer (mkSelector "locations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocations:@
setLocations :: (IsCAGradientLayer caGradientLayer, IsNSArray value) => caGradientLayer -> value -> IO ()
setLocations caGradientLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caGradientLayer (mkSelector "setLocations:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsCAGradientLayer caGradientLayer => caGradientLayer -> IO (Id NSString)
type_ caGradientLayer  =
  sendMsg caGradientLayer (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsCAGradientLayer caGradientLayer, IsNSString value) => caGradientLayer -> value -> IO ()
setType caGradientLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caGradientLayer (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colors@
colorsSelector :: Selector
colorsSelector = mkSelector "colors"

-- | @Selector@ for @setColors:@
setColorsSelector :: Selector
setColorsSelector = mkSelector "setColors:"

-- | @Selector@ for @locations@
locationsSelector :: Selector
locationsSelector = mkSelector "locations"

-- | @Selector@ for @setLocations:@
setLocationsSelector :: Selector
setLocationsSelector = mkSelector "setLocations:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

