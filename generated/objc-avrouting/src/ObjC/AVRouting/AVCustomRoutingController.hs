{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that manages the connection from a device to a destination.
--
-- A routing controller also informs its ``AVCustomRoutingController/delegate`` object about which routes the user previously authorized, so it can reconnect, if appropriate.
--
-- Generated bindings for @AVCustomRoutingController@.
module ObjC.AVRouting.AVCustomRoutingController
  ( AVCustomRoutingController
  , IsAVCustomRoutingController(..)
  , invalidateAuthorizationForRoute
  , setActive_forRoute
  , isRouteActive
  , invalidateAuthorizationForRouteSelector
  , setActive_forRouteSelector
  , isRouteActiveSelector


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

import ObjC.AVRouting.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Revokes an app’s authorization to connect to a route.
--
-- The route only becomes authorized again if the user selects it using the route picker.
--
-- - Parameters:   - route: The route to invalidate authorization for.
--
-- ObjC selector: @- invalidateAuthorizationForRoute:@
invalidateAuthorizationForRoute :: (IsAVCustomRoutingController avCustomRoutingController, IsAVCustomDeviceRoute route) => avCustomRoutingController -> route -> IO ()
invalidateAuthorizationForRoute avCustomRoutingController  route =
withObjCPtr route $ \raw_route ->
    sendMsg avCustomRoutingController (mkSelector "invalidateAuthorizationForRoute:") retVoid [argPtr (castPtr raw_route :: Ptr ())]

-- | Sets the active state of a route.
--
-- Set the value to <doc://com.apple.documentation/documentation/objectivec/no> if the connection to the route becomes unavailable, and set it to <doc://com.apple.documentation/documentation/objectivec/yes> after you reestablish the connection.
--
-- - Parameters:   - active: A Boolean value that indicates whether the route is active.
--
-- - route: A route to change the active state for.
--
-- ObjC selector: @- setActive:forRoute:@
setActive_forRoute :: (IsAVCustomRoutingController avCustomRoutingController, IsAVCustomDeviceRoute route) => avCustomRoutingController -> Bool -> route -> IO ()
setActive_forRoute avCustomRoutingController  active route =
withObjCPtr route $ \raw_route ->
    sendMsg avCustomRoutingController (mkSelector "setActive:forRoute:") retVoid [argCULong (if active then 1 else 0), argPtr (castPtr raw_route :: Ptr ())]

-- | Returns a Boolean value that indicates whether a route is active.
--
-- - Parameters:   - route: A route for determining its active state.
--
-- - Returns: <doc://com.apple.documentation/documentation/objectivec/yes> if the route is in an active state; otherwise, <doc://com.apple.documentation/documentation/objectivec/no>.
--
-- ObjC selector: @- isRouteActive:@
isRouteActive :: (IsAVCustomRoutingController avCustomRoutingController, IsAVCustomDeviceRoute route) => avCustomRoutingController -> route -> IO Bool
isRouteActive avCustomRoutingController  route =
withObjCPtr route $ \raw_route ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCustomRoutingController (mkSelector "isRouteActive:") retCULong [argPtr (castPtr raw_route :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateAuthorizationForRoute:@
invalidateAuthorizationForRouteSelector :: Selector
invalidateAuthorizationForRouteSelector = mkSelector "invalidateAuthorizationForRoute:"

-- | @Selector@ for @setActive:forRoute:@
setActive_forRouteSelector :: Selector
setActive_forRouteSelector = mkSelector "setActive:forRoute:"

-- | @Selector@ for @isRouteActive:@
isRouteActiveSelector :: Selector
isRouteActiveSelector = mkSelector "isRouteActive:"

