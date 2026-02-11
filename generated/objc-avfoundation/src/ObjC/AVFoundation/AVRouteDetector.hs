{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVRouteDetector
--
-- AVRouteDetector detects the presence of media playback routes.
--
-- If route detection is enabled (it is disabled by default), AVRouteDetector reports whether or not multiple playback routes have been detected. If this is the case, AVKit's AVRoutePickerView can be used to allow users to pick from the set of available routes.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVRouteDetector@.
module ObjC.AVFoundation.AVRouteDetector
  ( AVRouteDetector
  , IsAVRouteDetector(..)
  , routeDetectionEnabled
  , setRouteDetectionEnabled
  , multipleRoutesDetected
  , detectsCustomRoutes
  , setDetectsCustomRoutes
  , routeDetectionEnabledSelector
  , setRouteDetectionEnabledSelector
  , multipleRoutesDetectedSelector
  , detectsCustomRoutesSelector
  , setDetectsCustomRoutesSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | routeDetectionEnabled
--
-- Whether or not route detection is enabled. The default value is NO.
--
-- Route detection significantly increases power consumption and must be turned off when it's no longer needed.
--
-- ObjC selector: @- routeDetectionEnabled@
routeDetectionEnabled :: IsAVRouteDetector avRouteDetector => avRouteDetector -> IO Bool
routeDetectionEnabled avRouteDetector  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avRouteDetector (mkSelector "routeDetectionEnabled") retCULong []

-- | routeDetectionEnabled
--
-- Whether or not route detection is enabled. The default value is NO.
--
-- Route detection significantly increases power consumption and must be turned off when it's no longer needed.
--
-- ObjC selector: @- setRouteDetectionEnabled:@
setRouteDetectionEnabled :: IsAVRouteDetector avRouteDetector => avRouteDetector -> Bool -> IO ()
setRouteDetectionEnabled avRouteDetector  value =
  sendMsg avRouteDetector (mkSelector "setRouteDetectionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | multipleRoutesDetected
--
-- This property is YES if, in addition to the local playback route, at least one more playback route has been detected.
--
-- If multiple route have been detected, AVKit's AVRoutePickerView can be used to allow users to pick from the set of available routes. When the values of this property changes AVRouteDetectorMultipleRoutesDetectedDidChangeNotification is posted.
--
-- ObjC selector: @- multipleRoutesDetected@
multipleRoutesDetected :: IsAVRouteDetector avRouteDetector => avRouteDetector -> IO Bool
multipleRoutesDetected avRouteDetector  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avRouteDetector (mkSelector "multipleRoutesDetected") retCULong []

-- | detectsCustomRoutes
--
-- Whether or not route detection will include custom routes. The default value is NO.
--
-- Only set this to YES if also using AVCustomRoutingController.
--
-- ObjC selector: @- detectsCustomRoutes@
detectsCustomRoutes :: IsAVRouteDetector avRouteDetector => avRouteDetector -> IO Bool
detectsCustomRoutes avRouteDetector  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avRouteDetector (mkSelector "detectsCustomRoutes") retCULong []

-- | detectsCustomRoutes
--
-- Whether or not route detection will include custom routes. The default value is NO.
--
-- Only set this to YES if also using AVCustomRoutingController.
--
-- ObjC selector: @- setDetectsCustomRoutes:@
setDetectsCustomRoutes :: IsAVRouteDetector avRouteDetector => avRouteDetector -> Bool -> IO ()
setDetectsCustomRoutes avRouteDetector  value =
  sendMsg avRouteDetector (mkSelector "setDetectsCustomRoutes:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @routeDetectionEnabled@
routeDetectionEnabledSelector :: Selector
routeDetectionEnabledSelector = mkSelector "routeDetectionEnabled"

-- | @Selector@ for @setRouteDetectionEnabled:@
setRouteDetectionEnabledSelector :: Selector
setRouteDetectionEnabledSelector = mkSelector "setRouteDetectionEnabled:"

-- | @Selector@ for @multipleRoutesDetected@
multipleRoutesDetectedSelector :: Selector
multipleRoutesDetectedSelector = mkSelector "multipleRoutesDetected"

-- | @Selector@ for @detectsCustomRoutes@
detectsCustomRoutesSelector :: Selector
detectsCustomRoutesSelector = mkSelector "detectsCustomRoutes"

-- | @Selector@ for @setDetectsCustomRoutes:@
setDetectsCustomRoutesSelector :: Selector
setDetectsCustomRoutesSelector = mkSelector "setDetectsCustomRoutes:"

