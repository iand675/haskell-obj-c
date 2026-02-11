{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLCircularRegion@.
module ObjC.CoreLocation.CLCircularRegion
  ( CLCircularRegion
  , IsCLCircularRegion(..)
  , initWithCenter_radius_identifier
  , containsCoordinate
  , center
  , radius
  , initWithCenter_radius_identifierSelector
  , containsCoordinateSelector
  , centerSelector
  , radiusSelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithCenter:radius:identifier:@
initWithCenter_radius_identifier :: (IsCLCircularRegion clCircularRegion, IsNSString identifier) => clCircularRegion -> CLLocationCoordinate2D -> CDouble -> identifier -> IO (Id CLCircularRegion)
initWithCenter_radius_identifier clCircularRegion  center radius identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg clCircularRegion (mkSelector "initWithCenter:radius:identifier:") (retPtr retVoid) [argCLLocationCoordinate2D center, argCDouble (fromIntegral radius), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- containsCoordinate:@
containsCoordinate :: IsCLCircularRegion clCircularRegion => clCircularRegion -> CLLocationCoordinate2D -> IO Bool
containsCoordinate clCircularRegion  coordinate =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clCircularRegion (mkSelector "containsCoordinate:") retCULong [argCLLocationCoordinate2D coordinate]

-- | @- center@
center :: IsCLCircularRegion clCircularRegion => clCircularRegion -> IO CLLocationCoordinate2D
center clCircularRegion  =
  sendMsgStret clCircularRegion (mkSelector "center") retCLLocationCoordinate2D []

-- | @- radius@
radius :: IsCLCircularRegion clCircularRegion => clCircularRegion -> IO CDouble
radius clCircularRegion  =
  sendMsg clCircularRegion (mkSelector "radius") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCenter:radius:identifier:@
initWithCenter_radius_identifierSelector :: Selector
initWithCenter_radius_identifierSelector = mkSelector "initWithCenter:radius:identifier:"

-- | @Selector@ for @containsCoordinate:@
containsCoordinateSelector :: Selector
containsCoordinateSelector = mkSelector "containsCoordinate:"

-- | @Selector@ for @center@
centerSelector :: Selector
centerSelector = mkSelector "center"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

