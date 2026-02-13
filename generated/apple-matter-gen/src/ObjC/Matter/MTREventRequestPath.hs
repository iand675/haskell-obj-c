{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating an event being requested (for read or subscribe).
--
-- nil is used to represent wildcards.
--
-- Generated bindings for @MTREventRequestPath@.
module ObjC.Matter.MTREventRequestPath
  ( MTREventRequestPath
  , IsMTREventRequestPath(..)
  , requestPathWithEndpointID_clusterID_eventID
  , endpoint
  , cluster
  , event
  , clusterSelector
  , endpointSelector
  , eventSelector
  , requestPathWithEndpointID_clusterID_eventIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ requestPathWithEndpointID:clusterID:eventID:@
requestPathWithEndpointID_clusterID_eventID :: (IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber eventID) => endpointID -> clusterID -> eventID -> IO (Id MTREventRequestPath)
requestPathWithEndpointID_clusterID_eventID endpointID clusterID eventID =
  do
    cls' <- getRequiredClass "MTREventRequestPath"
    sendClassMessage cls' requestPathWithEndpointID_clusterID_eventIDSelector (toNSNumber endpointID) (toNSNumber clusterID) (toNSNumber eventID)

-- | @- endpoint@
endpoint :: IsMTREventRequestPath mtrEventRequestPath => mtrEventRequestPath -> IO (Id NSNumber)
endpoint mtrEventRequestPath =
  sendMessage mtrEventRequestPath endpointSelector

-- | @- cluster@
cluster :: IsMTREventRequestPath mtrEventRequestPath => mtrEventRequestPath -> IO (Id NSNumber)
cluster mtrEventRequestPath =
  sendMessage mtrEventRequestPath clusterSelector

-- | @- event@
event :: IsMTREventRequestPath mtrEventRequestPath => mtrEventRequestPath -> IO (Id NSNumber)
event mtrEventRequestPath =
  sendMessage mtrEventRequestPath eventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestPathWithEndpointID:clusterID:eventID:@
requestPathWithEndpointID_clusterID_eventIDSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber] (Id MTREventRequestPath)
requestPathWithEndpointID_clusterID_eventIDSelector = mkSelector "requestPathWithEndpointID:clusterID:eventID:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @cluster@
clusterSelector :: Selector '[] (Id NSNumber)
clusterSelector = mkSelector "cluster"

-- | @Selector@ for @event@
eventSelector :: Selector '[] (Id NSNumber)
eventSelector = mkSelector "event"

