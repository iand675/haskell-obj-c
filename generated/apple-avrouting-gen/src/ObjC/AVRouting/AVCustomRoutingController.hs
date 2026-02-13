{-# LANGUAGE DataKinds #-}
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
  , authorizedRoutesSelector
  , customActionItemsSelector
  , delegateSelector
  , invalidateAuthorizationForRouteSelector
  , isRouteActiveSelector
  , knownRouteIPsSelector
  , setActive_forRouteSelector
  , setCustomActionItemsSelector
  , setDelegateSelector
  , setKnownRouteIPsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
invalidateAuthorizationForRoute avCustomRoutingController route =
  sendMessage avCustomRoutingController invalidateAuthorizationForRouteSelector (toAVCustomDeviceRoute route)

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
setActive_forRoute avCustomRoutingController active route =
  sendMessage avCustomRoutingController setActive_forRouteSelector active (toAVCustomDeviceRoute route)

-- | Returns a Boolean value that indicates whether a route is active.
--
-- - Parameters:   - route: A route for determining its active state.
--
-- - Returns: <doc://com.apple.documentation/documentation/objectivec/yes> if the route is in an active state; otherwise, <doc://com.apple.documentation/documentation/objectivec/no>.
--
-- ObjC selector: @- isRouteActive:@
isRouteActive :: (IsAVCustomRoutingController avCustomRoutingController, IsAVCustomDeviceRoute route) => avCustomRoutingController -> route -> IO Bool
isRouteActive avCustomRoutingController route =
  sendMessage avCustomRoutingController isRouteActiveSelector (toAVCustomDeviceRoute route)

-- | A delegate object for a routing controller.
--
-- ObjC selector: @- delegate@
delegate :: IsAVCustomRoutingController avCustomRoutingController => avCustomRoutingController -> IO RawId
delegate avCustomRoutingController =
  sendMessage avCustomRoutingController delegateSelector

-- | A delegate object for a routing controller.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVCustomRoutingController avCustomRoutingController => avCustomRoutingController -> RawId -> IO ()
setDelegate avCustomRoutingController value =
  sendMessage avCustomRoutingController setDelegateSelector value

-- | A list of authorized routes.
--
-- After a user activates a route, it remains authorized for a certain amount of time even if the connection to the route is temporarily unavailable. Your app may reactivate any one of these routes when appropriate, but it needs to inform the system by calling ``AVCustomRoutingController/setActive:forRoute:``.
--
-- ObjC selector: @- authorizedRoutes@
authorizedRoutes :: IsAVCustomRoutingController avCustomRoutingController => avCustomRoutingController -> IO (Id NSArray)
authorizedRoutes avCustomRoutingController =
  sendMessage avCustomRoutingController authorizedRoutesSelector

-- | An array of route addresses known to be on the local network.
--
-- ObjC selector: @- knownRouteIPs@
knownRouteIPs :: IsAVCustomRoutingController avCustomRoutingController => avCustomRoutingController -> IO (Id NSArray)
knownRouteIPs avCustomRoutingController =
  sendMessage avCustomRoutingController knownRouteIPsSelector

-- | An array of route addresses known to be on the local network.
--
-- ObjC selector: @- setKnownRouteIPs:@
setKnownRouteIPs :: (IsAVCustomRoutingController avCustomRoutingController, IsNSArray value) => avCustomRoutingController -> value -> IO ()
setKnownRouteIPs avCustomRoutingController value =
  sendMessage avCustomRoutingController setKnownRouteIPsSelector (toNSArray value)

-- | An array of custom action items to add to a route picker.
--
-- ObjC selector: @- customActionItems@
customActionItems :: IsAVCustomRoutingController avCustomRoutingController => avCustomRoutingController -> IO (Id NSArray)
customActionItems avCustomRoutingController =
  sendMessage avCustomRoutingController customActionItemsSelector

-- | An array of custom action items to add to a route picker.
--
-- ObjC selector: @- setCustomActionItems:@
setCustomActionItems :: (IsAVCustomRoutingController avCustomRoutingController, IsNSArray value) => avCustomRoutingController -> value -> IO ()
setCustomActionItems avCustomRoutingController value =
  sendMessage avCustomRoutingController setCustomActionItemsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateAuthorizationForRoute:@
invalidateAuthorizationForRouteSelector :: Selector '[Id AVCustomDeviceRoute] ()
invalidateAuthorizationForRouteSelector = mkSelector "invalidateAuthorizationForRoute:"

-- | @Selector@ for @setActive:forRoute:@
setActive_forRouteSelector :: Selector '[Bool, Id AVCustomDeviceRoute] ()
setActive_forRouteSelector = mkSelector "setActive:forRoute:"

-- | @Selector@ for @isRouteActive:@
isRouteActiveSelector :: Selector '[Id AVCustomDeviceRoute] Bool
isRouteActiveSelector = mkSelector "isRouteActive:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @authorizedRoutes@
authorizedRoutesSelector :: Selector '[] (Id NSArray)
authorizedRoutesSelector = mkSelector "authorizedRoutes"

-- | @Selector@ for @knownRouteIPs@
knownRouteIPsSelector :: Selector '[] (Id NSArray)
knownRouteIPsSelector = mkSelector "knownRouteIPs"

-- | @Selector@ for @setKnownRouteIPs:@
setKnownRouteIPsSelector :: Selector '[Id NSArray] ()
setKnownRouteIPsSelector = mkSelector "setKnownRouteIPs:"

-- | @Selector@ for @customActionItems@
customActionItemsSelector :: Selector '[] (Id NSArray)
customActionItemsSelector = mkSelector "customActionItems"

-- | @Selector@ for @setCustomActionItems:@
setCustomActionItemsSelector :: Selector '[Id NSArray] ()
setCustomActionItemsSelector = mkSelector "setCustomActionItems:"

