{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLRegion@.
module ObjC.CoreLocation.CLRegion
  ( CLRegion
  , IsCLRegion(..)
  , initCircularRegionWithCenter_radius_identifier
  , containsCoordinate
  , center
  , radius
  , identifier
  , notifyOnEntry
  , setNotifyOnEntry
  , notifyOnExit
  , setNotifyOnExit
  , initCircularRegionWithCenter_radius_identifierSelector
  , containsCoordinateSelector
  , centerSelector
  , radiusSelector
  , identifierSelector
  , notifyOnEntrySelector
  , setNotifyOnEntrySelector
  , notifyOnExitSelector
  , setNotifyOnExitSelector


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

-- | @- initCircularRegionWithCenter:radius:identifier:@
initCircularRegionWithCenter_radius_identifier :: (IsCLRegion clRegion, IsNSString identifier) => clRegion -> CLLocationCoordinate2D -> CDouble -> identifier -> IO (Id CLRegion)
initCircularRegionWithCenter_radius_identifier clRegion  center radius identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg clRegion (mkSelector "initCircularRegionWithCenter:radius:identifier:") (retPtr retVoid) [argCLLocationCoordinate2D center, argCDouble (fromIntegral radius), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- containsCoordinate:@
containsCoordinate :: IsCLRegion clRegion => clRegion -> CLLocationCoordinate2D -> IO Bool
containsCoordinate clRegion  coordinate =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clRegion (mkSelector "containsCoordinate:") retCULong [argCLLocationCoordinate2D coordinate]

-- | @- center@
center :: IsCLRegion clRegion => clRegion -> IO CLLocationCoordinate2D
center clRegion  =
  sendMsgStret clRegion (mkSelector "center") retCLLocationCoordinate2D []

-- | @- radius@
radius :: IsCLRegion clRegion => clRegion -> IO CDouble
radius clRegion  =
  sendMsg clRegion (mkSelector "radius") retCDouble []

-- | @- identifier@
identifier :: IsCLRegion clRegion => clRegion -> IO (Id NSString)
identifier clRegion  =
  sendMsg clRegion (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- notifyOnEntry@
notifyOnEntry :: IsCLRegion clRegion => clRegion -> IO Bool
notifyOnEntry clRegion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clRegion (mkSelector "notifyOnEntry") retCULong []

-- | @- setNotifyOnEntry:@
setNotifyOnEntry :: IsCLRegion clRegion => clRegion -> Bool -> IO ()
setNotifyOnEntry clRegion  value =
  sendMsg clRegion (mkSelector "setNotifyOnEntry:") retVoid [argCULong (if value then 1 else 0)]

-- | @- notifyOnExit@
notifyOnExit :: IsCLRegion clRegion => clRegion -> IO Bool
notifyOnExit clRegion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clRegion (mkSelector "notifyOnExit") retCULong []

-- | @- setNotifyOnExit:@
setNotifyOnExit :: IsCLRegion clRegion => clRegion -> Bool -> IO ()
setNotifyOnExit clRegion  value =
  sendMsg clRegion (mkSelector "setNotifyOnExit:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initCircularRegionWithCenter:radius:identifier:@
initCircularRegionWithCenter_radius_identifierSelector :: Selector
initCircularRegionWithCenter_radius_identifierSelector = mkSelector "initCircularRegionWithCenter:radius:identifier:"

-- | @Selector@ for @containsCoordinate:@
containsCoordinateSelector :: Selector
containsCoordinateSelector = mkSelector "containsCoordinate:"

-- | @Selector@ for @center@
centerSelector :: Selector
centerSelector = mkSelector "center"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @notifyOnEntry@
notifyOnEntrySelector :: Selector
notifyOnEntrySelector = mkSelector "notifyOnEntry"

-- | @Selector@ for @setNotifyOnEntry:@
setNotifyOnEntrySelector :: Selector
setNotifyOnEntrySelector = mkSelector "setNotifyOnEntry:"

-- | @Selector@ for @notifyOnExit@
notifyOnExitSelector :: Selector
notifyOnExitSelector = mkSelector "notifyOnExit"

-- | @Selector@ for @setNotifyOnExit:@
setNotifyOnExitSelector :: Selector
setNotifyOnExitSelector = mkSelector "setNotifyOnExit:"

