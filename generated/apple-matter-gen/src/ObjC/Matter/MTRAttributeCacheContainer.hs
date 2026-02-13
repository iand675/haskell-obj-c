{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAttributeCacheContainer@.
module ObjC.Matter.MTRAttributeCacheContainer
  ( MTRAttributeCacheContainer
  , IsMTRAttributeCacheContainer(..)
  , readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completion
  , readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeWithEndpointId:clusterId:attributeId:clientQueue:completion:@
readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completion :: (IsMTRAttributeCacheContainer mtrAttributeCacheContainer, IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber attributeId, IsNSObject clientQueue) => mtrAttributeCacheContainer -> endpointId -> clusterId -> attributeId -> clientQueue -> Ptr () -> IO ()
readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completion mtrAttributeCacheContainer endpointId clusterId attributeId clientQueue completion =
  sendMessage mtrAttributeCacheContainer readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completionSelector (toNSNumber endpointId) (toNSNumber clusterId) (toNSNumber attributeId) (toNSObject clientQueue) completion

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeWithEndpointId:clusterId:attributeId:clientQueue:completion:@
readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completionSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeWithEndpointId_clusterId_attributeId_clientQueue_completionSelector = mkSelector "readAttributeWithEndpointId:clusterId:attributeId:clientQueue:completion:"

