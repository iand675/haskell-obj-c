{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that holds a cluster state cache.  It can be passed to MTRBaseDevice's subscribeWithQueue to fill the cache with data the subscription returns.  Then reads can happen against the cache without going out to the network.
--
-- Generated bindings for @MTRClusterStateCacheContainer@.
module ObjC.Matter.MTRClusterStateCacheContainer
  ( MTRClusterStateCacheContainer
  , IsMTRClusterStateCacheContainer(..)
  , readAttributesWithEndpointID_clusterID_attributeID_queue_completion
  , readAttributesWithEndpointID_clusterID_attributeID_queue_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Reads the given attributes from the cluster state cache inside this cache container.
--
-- @endpointID@ — endpoint ID of the attributes. Nil means wildcard.
--
-- @clusterID@ — cluster ID of the attributes. Nil means wildcard.
--
-- @attributeID@ — attribute ID of the attributes. Nil means wildcard.
--
-- @queue@ — client queue to dispatch the completion handler through
--
-- @completion@ — block to receive the result.                   "values" received by the block will have the same format of object as the one received by the completion block                   of the MTRBaseDevice readAttributesWithEndpointID:clusterID:attributeID:queue:completion method.
--
-- Note: : not all combinations of wildcards might be supported.
--
-- ObjC selector: @- readAttributesWithEndpointID:clusterID:attributeID:queue:completion:@
readAttributesWithEndpointID_clusterID_attributeID_queue_completion :: (IsMTRClusterStateCacheContainer mtrClusterStateCacheContainer, IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber attributeID, IsNSObject queue) => mtrClusterStateCacheContainer -> endpointID -> clusterID -> attributeID -> queue -> Ptr () -> IO ()
readAttributesWithEndpointID_clusterID_attributeID_queue_completion mtrClusterStateCacheContainer endpointID clusterID attributeID queue completion =
  sendMessage mtrClusterStateCacheContainer readAttributesWithEndpointID_clusterID_attributeID_queue_completionSelector (toNSNumber endpointID) (toNSNumber clusterID) (toNSNumber attributeID) (toNSObject queue) completion

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributesWithEndpointID:clusterID:attributeID:queue:completion:@
readAttributesWithEndpointID_clusterID_attributeID_queue_completionSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributesWithEndpointID_clusterID_attributeID_queue_completionSelector = mkSelector "readAttributesWithEndpointID:clusterID:attributeID:queue:completion:"

