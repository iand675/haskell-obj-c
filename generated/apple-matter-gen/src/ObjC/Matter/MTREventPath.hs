{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating a specific event that can be emitted on a device (i.e. without any wildcards).  There can be multiple instances of actual events for a given event path.
--
-- Generated bindings for @MTREventPath@.
module ObjC.Matter.MTREventPath
  ( MTREventPath
  , IsMTREventPath(..)
  , eventPathWithEndpointID_clusterID_eventID
  , eventPathWithEndpointId_clusterId_eventId
  , event
  , eventPathWithEndpointID_clusterID_eventIDSelector
  , eventPathWithEndpointId_clusterId_eventIdSelector
  , eventSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ eventPathWithEndpointID:clusterID:eventID:@
eventPathWithEndpointID_clusterID_eventID :: (IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber eventID) => endpointID -> clusterID -> eventID -> IO (Id MTREventPath)
eventPathWithEndpointID_clusterID_eventID endpointID clusterID eventID =
  do
    cls' <- getRequiredClass "MTREventPath"
    sendClassMessage cls' eventPathWithEndpointID_clusterID_eventIDSelector (toNSNumber endpointID) (toNSNumber clusterID) (toNSNumber eventID)

-- | @+ eventPathWithEndpointId:clusterId:eventId:@
eventPathWithEndpointId_clusterId_eventId :: (IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber eventId) => endpointId -> clusterId -> eventId -> IO (Id MTREventPath)
eventPathWithEndpointId_clusterId_eventId endpointId clusterId eventId =
  do
    cls' <- getRequiredClass "MTREventPath"
    sendClassMessage cls' eventPathWithEndpointId_clusterId_eventIdSelector (toNSNumber endpointId) (toNSNumber clusterId) (toNSNumber eventId)

-- | @- event@
event :: IsMTREventPath mtrEventPath => mtrEventPath -> IO (Id NSNumber)
event mtrEventPath =
  sendMessage mtrEventPath eventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @eventPathWithEndpointID:clusterID:eventID:@
eventPathWithEndpointID_clusterID_eventIDSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber] (Id MTREventPath)
eventPathWithEndpointID_clusterID_eventIDSelector = mkSelector "eventPathWithEndpointID:clusterID:eventID:"

-- | @Selector@ for @eventPathWithEndpointId:clusterId:eventId:@
eventPathWithEndpointId_clusterId_eventIdSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber] (Id MTREventPath)
eventPathWithEndpointId_clusterId_eventIdSelector = mkSelector "eventPathWithEndpointId:clusterId:eventId:"

-- | @Selector@ for @event@
eventSelector :: Selector '[] (Id NSNumber)
eventSelector = mkSelector "event"

