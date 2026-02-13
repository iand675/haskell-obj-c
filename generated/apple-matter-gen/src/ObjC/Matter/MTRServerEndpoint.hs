{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of an endpoint implemented by an MTRDeviceController.
--
-- MTRServerEndpoint's API can be accessed from any thread.
--
-- Generated bindings for @MTRServerEndpoint@.
module ObjC.Matter.MTRServerEndpoint
  ( MTRServerEndpoint
  , IsMTRServerEndpoint(..)
  , init_
  , new
  , initWithEndpointID_deviceTypes
  , addAccessGrant
  , removeAccessGrant
  , addServerCluster
  , endpointID
  , deviceTypes
  , accessGrants
  , serverClusters
  , accessGrantsSelector
  , addAccessGrantSelector
  , addServerClusterSelector
  , deviceTypesSelector
  , endpointIDSelector
  , initSelector
  , initWithEndpointID_deviceTypesSelector
  , newSelector
  , removeAccessGrantSelector
  , serverClustersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRServerEndpoint mtrServerEndpoint => mtrServerEndpoint -> IO (Id MTRServerEndpoint)
init_ mtrServerEndpoint =
  sendOwnedMessage mtrServerEndpoint initSelector

-- | @+ new@
new :: IO (Id MTRServerEndpoint)
new  =
  do
    cls' <- getRequiredClass "MTRServerEndpoint"
    sendOwnedClassMessage cls' newSelector

-- | The provided endpointID must be in the range 1-65535.  The list of device types provided must be nonempty (but may include vendor-specific device types).
--
-- ObjC selector: @- initWithEndpointID:deviceTypes:@
initWithEndpointID_deviceTypes :: (IsMTRServerEndpoint mtrServerEndpoint, IsNSNumber endpointID, IsNSArray deviceTypes) => mtrServerEndpoint -> endpointID -> deviceTypes -> IO (Id MTRServerEndpoint)
initWithEndpointID_deviceTypes mtrServerEndpoint endpointID deviceTypes =
  sendOwnedMessage mtrServerEndpoint initWithEndpointID_deviceTypesSelector (toNSNumber endpointID) (toNSArray deviceTypes)

-- | Add an access grant to the endpoint.  If the same access grant is added multiple times, it will be treated as if it were added once (and removing it once will remove it).
--
-- ObjC selector: @- addAccessGrant:@
addAccessGrant :: (IsMTRServerEndpoint mtrServerEndpoint, IsMTRAccessGrant accessGrant) => mtrServerEndpoint -> accessGrant -> IO ()
addAccessGrant mtrServerEndpoint accessGrant =
  sendMessage mtrServerEndpoint addAccessGrantSelector (toMTRAccessGrant accessGrant)

-- | Remove an access grant from the endpoint.
--
-- ObjC selector: @- removeAccessGrant:@
removeAccessGrant :: (IsMTRServerEndpoint mtrServerEndpoint, IsMTRAccessGrant accessGrant) => mtrServerEndpoint -> accessGrant -> IO ()
removeAccessGrant mtrServerEndpoint accessGrant =
  sendMessage mtrServerEndpoint removeAccessGrantSelector (toMTRAccessGrant accessGrant)

-- | Add a server cluster to the endpoint.  This can only be done before the endpoint has been added to a controller.
--
-- The cluster must not have the same cluster ID as another cluster on this endpoint.
--
-- The cluster must not already be added to another endpoint.
--
-- ObjC selector: @- addServerCluster:@
addServerCluster :: (IsMTRServerEndpoint mtrServerEndpoint, IsMTRServerCluster serverCluster) => mtrServerEndpoint -> serverCluster -> IO Bool
addServerCluster mtrServerEndpoint serverCluster =
  sendMessage mtrServerEndpoint addServerClusterSelector (toMTRServerCluster serverCluster)

-- | @- endpointID@
endpointID :: IsMTRServerEndpoint mtrServerEndpoint => mtrServerEndpoint -> IO (Id NSNumber)
endpointID mtrServerEndpoint =
  sendMessage mtrServerEndpoint endpointIDSelector

-- | @- deviceTypes@
deviceTypes :: IsMTRServerEndpoint mtrServerEndpoint => mtrServerEndpoint -> IO (Id NSArray)
deviceTypes mtrServerEndpoint =
  sendMessage mtrServerEndpoint deviceTypesSelector

-- | The list of entities that are allowed to access all clusters on this endpoint.  If more fine-grained access control is desired, access grants should be defined on individual clusters.
--
-- Defaults to empty list, which means no access granted.
--
-- ObjC selector: @- accessGrants@
accessGrants :: IsMTRServerEndpoint mtrServerEndpoint => mtrServerEndpoint -> IO (Id NSArray)
accessGrants mtrServerEndpoint =
  sendMessage mtrServerEndpoint accessGrantsSelector

-- | A list of server clusters supported on this endpoint.  The Descriptor cluster does not need to be included unless a TagList attribute is desired on it or it has a non-empty PartsList, or it needs to have cluster-specific access grants.  If not included, the Descriptor cluster will be generated automatically.
--
-- ObjC selector: @- serverClusters@
serverClusters :: IsMTRServerEndpoint mtrServerEndpoint => mtrServerEndpoint -> IO (Id NSArray)
serverClusters mtrServerEndpoint =
  sendMessage mtrServerEndpoint serverClustersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRServerEndpoint)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRServerEndpoint)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEndpointID:deviceTypes:@
initWithEndpointID_deviceTypesSelector :: Selector '[Id NSNumber, Id NSArray] (Id MTRServerEndpoint)
initWithEndpointID_deviceTypesSelector = mkSelector "initWithEndpointID:deviceTypes:"

-- | @Selector@ for @addAccessGrant:@
addAccessGrantSelector :: Selector '[Id MTRAccessGrant] ()
addAccessGrantSelector = mkSelector "addAccessGrant:"

-- | @Selector@ for @removeAccessGrant:@
removeAccessGrantSelector :: Selector '[Id MTRAccessGrant] ()
removeAccessGrantSelector = mkSelector "removeAccessGrant:"

-- | @Selector@ for @addServerCluster:@
addServerClusterSelector :: Selector '[Id MTRServerCluster] Bool
addServerClusterSelector = mkSelector "addServerCluster:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @deviceTypes@
deviceTypesSelector :: Selector '[] (Id NSArray)
deviceTypesSelector = mkSelector "deviceTypes"

-- | @Selector@ for @accessGrants@
accessGrantsSelector :: Selector '[] (Id NSArray)
accessGrantsSelector = mkSelector "accessGrants"

-- | @Selector@ for @serverClusters@
serverClustersSelector :: Selector '[] (Id NSArray)
serverClustersSelector = mkSelector "serverClusters"

