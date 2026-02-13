{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating a specific attribute on a device (i.e. without any wildcards).
--
-- Generated bindings for @MTRAttributePath@.
module ObjC.Matter.MTRAttributePath
  ( MTRAttributePath
  , IsMTRAttributePath(..)
  , attributePathWithEndpointID_clusterID_attributeID
  , attributePathWithEndpointId_clusterId_attributeId
  , attribute
  , attributePathWithEndpointID_clusterID_attributeIDSelector
  , attributePathWithEndpointId_clusterId_attributeIdSelector
  , attributeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ attributePathWithEndpointID:clusterID:attributeID:@
attributePathWithEndpointID_clusterID_attributeID :: (IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber attributeID) => endpointID -> clusterID -> attributeID -> IO (Id MTRAttributePath)
attributePathWithEndpointID_clusterID_attributeID endpointID clusterID attributeID =
  do
    cls' <- getRequiredClass "MTRAttributePath"
    sendClassMessage cls' attributePathWithEndpointID_clusterID_attributeIDSelector (toNSNumber endpointID) (toNSNumber clusterID) (toNSNumber attributeID)

-- | @+ attributePathWithEndpointId:clusterId:attributeId:@
attributePathWithEndpointId_clusterId_attributeId :: (IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber attributeId) => endpointId -> clusterId -> attributeId -> IO (Id MTRAttributePath)
attributePathWithEndpointId_clusterId_attributeId endpointId clusterId attributeId =
  do
    cls' <- getRequiredClass "MTRAttributePath"
    sendClassMessage cls' attributePathWithEndpointId_clusterId_attributeIdSelector (toNSNumber endpointId) (toNSNumber clusterId) (toNSNumber attributeId)

-- | @- attribute@
attribute :: IsMTRAttributePath mtrAttributePath => mtrAttributePath -> IO (Id NSNumber)
attribute mtrAttributePath =
  sendMessage mtrAttributePath attributeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributePathWithEndpointID:clusterID:attributeID:@
attributePathWithEndpointID_clusterID_attributeIDSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber] (Id MTRAttributePath)
attributePathWithEndpointID_clusterID_attributeIDSelector = mkSelector "attributePathWithEndpointID:clusterID:attributeID:"

-- | @Selector@ for @attributePathWithEndpointId:clusterId:attributeId:@
attributePathWithEndpointId_clusterId_attributeIdSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber] (Id MTRAttributePath)
attributePathWithEndpointId_clusterId_attributeIdSelector = mkSelector "attributePathWithEndpointId:clusterId:attributeId:"

-- | @Selector@ for @attribute@
attributeSelector :: Selector '[] (Id NSNumber)
attributeSelector = mkSelector "attribute"

