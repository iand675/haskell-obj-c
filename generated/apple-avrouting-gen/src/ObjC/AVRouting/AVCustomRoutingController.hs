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
  , delegate
  , setDelegate
  , authorizedRoutes
  , knownRouteIPs
  , setKnownRouteIPs
  , customActionItems
  , setCustomActionItems
  , invalidateAuthorizationForRouteSelector
  , setActive_forRouteSelector
  , isRouteActiveSelector
  , delegateSelector
  , setDelegateSelector
  , authorizedRoutesSelector
  , knownRouteIPsSelector
  , setKnownRouteIPsSelector
  , customActionItemsSelector
  , setCustomActionItemsSelector


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

-- | A delegate object for a routing controller.
--
-- ObjC selector: @- delegate@
delegate :: IsAVCustomRoutingController avCustomRoutingController => avCustomRoutingController -> IO RawId
delegate avCustomRoutingController  =
    fmap (RawId . castPtr) $ sendMsg avCustomRoutingController (mkSelector "delegate") (retPtr retVoid) []

-- | A delegate object for a routing controller.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVCustomRoutingController avCustomRoutingController => avCustomRoutingController -> RawId -> IO ()
setDelegate avCustomRoutingController  value =
    sendMsg avCustomRoutingController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | A list of authorized routes.
--
-- After a user activates a route, it remains authorized for a certain amount of time even if the connection to the route is temporarily unavailable. Your app may reactivate any one of these routes when appropriate, but it needs to inform the system by calling ``AVCustomRoutingController/setActive:forRoute:``.
--
-- ObjC selector: @- authorizedRoutes@
authorizedRoutes :: IsAVCustomRoutingController avCustomRoutingController => avCustomRoutingController -> IO (Id NSArray)
authorizedRoutes avCustomRoutingController  =
    sendMsg avCustomRoutingController (mkSelector "authorizedRoutes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of route addresses known to be on the local network.
--
-- ObjC selector: @- knownRouteIPs@
knownRouteIPs :: IsAVCustomRoutingController avCustomRoutingController => avCustomRoutingController -> IO (Id NSArray)
knownRouteIPs avCustomRoutingController  =
    sendMsg avCustomRoutingController (mkSelector "knownRouteIPs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of route addresses known to be on the local network.
--
-- ObjC selector: @- setKnownRouteIPs:@
setKnownRouteIPs :: (IsAVCustomRoutingController avCustomRoutingController, IsNSArray value) => avCustomRoutingController -> value -> IO ()
setKnownRouteIPs avCustomRoutingController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avCustomRoutingController (mkSelector "setKnownRouteIPs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An array of custom action items to add to a route picker.
--
-- ObjC selector: @- customActionItems@
customActionItems :: IsAVCustomRoutingController avCustomRoutingController => avCustomRoutingController -> IO (Id NSArray)
customActionItems avCustomRoutingController  =
    sendMsg avCustomRoutingController (mkSelector "customActionItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of custom action items to add to a route picker.
--
-- ObjC selector: @- setCustomActionItems:@
setCustomActionItems :: (IsAVCustomRoutingController avCustomRoutingController, IsNSArray value) => avCustomRoutingController -> value -> IO ()
setCustomActionItems avCustomRoutingController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avCustomRoutingController (mkSelector "setCustomActionItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @authorizedRoutes@
authorizedRoutesSelector :: Selector
authorizedRoutesSelector = mkSelector "authorizedRoutes"

-- | @Selector@ for @knownRouteIPs@
knownRouteIPsSelector :: Selector
knownRouteIPsSelector = mkSelector "knownRouteIPs"

-- | @Selector@ for @setKnownRouteIPs:@
setKnownRouteIPsSelector :: Selector
setKnownRouteIPsSelector = mkSelector "setKnownRouteIPs:"

-- | @Selector@ for @customActionItems@
customActionItemsSelector :: Selector
customActionItemsSelector = mkSelector "customActionItems"

-- | @Selector@ for @setCustomActionItems:@
setCustomActionItemsSelector :: Selector
setCustomActionItemsSelector = mkSelector "setCustomActionItems:"

