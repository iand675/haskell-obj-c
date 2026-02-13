{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTASoftwareUpdateRequestorClusterProviderLocation@.
module ObjC.Matter.MTROTASoftwareUpdateRequestorClusterProviderLocation
  ( MTROTASoftwareUpdateRequestorClusterProviderLocation
  , IsMTROTASoftwareUpdateRequestorClusterProviderLocation(..)
  , providerNodeID
  , setProviderNodeID
  , endpoint
  , setEndpoint
  , fabricIndex
  , setFabricIndex
  , endpointSelector
  , fabricIndexSelector
  , providerNodeIDSelector
  , setEndpointSelector
  , setFabricIndexSelector
  , setProviderNodeIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- providerNodeID@
providerNodeID :: IsMTROTASoftwareUpdateRequestorClusterProviderLocation mtrotaSoftwareUpdateRequestorClusterProviderLocation => mtrotaSoftwareUpdateRequestorClusterProviderLocation -> IO (Id NSNumber)
providerNodeID mtrotaSoftwareUpdateRequestorClusterProviderLocation =
  sendMessage mtrotaSoftwareUpdateRequestorClusterProviderLocation providerNodeIDSelector

-- | @- setProviderNodeID:@
setProviderNodeID :: (IsMTROTASoftwareUpdateRequestorClusterProviderLocation mtrotaSoftwareUpdateRequestorClusterProviderLocation, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterProviderLocation -> value -> IO ()
setProviderNodeID mtrotaSoftwareUpdateRequestorClusterProviderLocation value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterProviderLocation setProviderNodeIDSelector (toNSNumber value)

-- | @- endpoint@
endpoint :: IsMTROTASoftwareUpdateRequestorClusterProviderLocation mtrotaSoftwareUpdateRequestorClusterProviderLocation => mtrotaSoftwareUpdateRequestorClusterProviderLocation -> IO (Id NSNumber)
endpoint mtrotaSoftwareUpdateRequestorClusterProviderLocation =
  sendMessage mtrotaSoftwareUpdateRequestorClusterProviderLocation endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTROTASoftwareUpdateRequestorClusterProviderLocation mtrotaSoftwareUpdateRequestorClusterProviderLocation, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterProviderLocation -> value -> IO ()
setEndpoint mtrotaSoftwareUpdateRequestorClusterProviderLocation value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterProviderLocation setEndpointSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTROTASoftwareUpdateRequestorClusterProviderLocation mtrotaSoftwareUpdateRequestorClusterProviderLocation => mtrotaSoftwareUpdateRequestorClusterProviderLocation -> IO (Id NSNumber)
fabricIndex mtrotaSoftwareUpdateRequestorClusterProviderLocation =
  sendMessage mtrotaSoftwareUpdateRequestorClusterProviderLocation fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROTASoftwareUpdateRequestorClusterProviderLocation mtrotaSoftwareUpdateRequestorClusterProviderLocation, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterProviderLocation -> value -> IO ()
setFabricIndex mtrotaSoftwareUpdateRequestorClusterProviderLocation value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterProviderLocation setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @providerNodeID@
providerNodeIDSelector :: Selector '[] (Id NSNumber)
providerNodeIDSelector = mkSelector "providerNodeID"

-- | @Selector@ for @setProviderNodeID:@
setProviderNodeIDSelector :: Selector '[Id NSNumber] ()
setProviderNodeIDSelector = mkSelector "setProviderNodeID:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector '[Id NSNumber] ()
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

