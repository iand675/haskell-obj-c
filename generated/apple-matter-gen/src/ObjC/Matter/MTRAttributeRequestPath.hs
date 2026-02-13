{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating an attribute being requested (for read or subscribe).
--
-- nil is used to represent wildcards.
--
-- Generated bindings for @MTRAttributeRequestPath@.
module ObjC.Matter.MTRAttributeRequestPath
  ( MTRAttributeRequestPath
  , IsMTRAttributeRequestPath(..)
  , requestPathWithEndpointID_clusterID_attributeID
  , endpoint
  , cluster
  , attribute
  , attributeSelector
  , clusterSelector
  , endpointSelector
  , requestPathWithEndpointID_clusterID_attributeIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ requestPathWithEndpointID:clusterID:attributeID:@
requestPathWithEndpointID_clusterID_attributeID :: (IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber attributeID) => endpointID -> clusterID -> attributeID -> IO (Id MTRAttributeRequestPath)
requestPathWithEndpointID_clusterID_attributeID endpointID clusterID attributeID =
  do
    cls' <- getRequiredClass "MTRAttributeRequestPath"
    sendClassMessage cls' requestPathWithEndpointID_clusterID_attributeIDSelector (toNSNumber endpointID) (toNSNumber clusterID) (toNSNumber attributeID)

-- | @- endpoint@
endpoint :: IsMTRAttributeRequestPath mtrAttributeRequestPath => mtrAttributeRequestPath -> IO (Id NSNumber)
endpoint mtrAttributeRequestPath =
  sendMessage mtrAttributeRequestPath endpointSelector

-- | @- cluster@
cluster :: IsMTRAttributeRequestPath mtrAttributeRequestPath => mtrAttributeRequestPath -> IO (Id NSNumber)
cluster mtrAttributeRequestPath =
  sendMessage mtrAttributeRequestPath clusterSelector

-- | @- attribute@
attribute :: IsMTRAttributeRequestPath mtrAttributeRequestPath => mtrAttributeRequestPath -> IO (Id NSNumber)
attribute mtrAttributeRequestPath =
  sendMessage mtrAttributeRequestPath attributeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestPathWithEndpointID:clusterID:attributeID:@
requestPathWithEndpointID_clusterID_attributeIDSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber] (Id MTRAttributeRequestPath)
requestPathWithEndpointID_clusterID_attributeIDSelector = mkSelector "requestPathWithEndpointID:clusterID:attributeID:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @cluster@
clusterSelector :: Selector '[] (Id NSNumber)
clusterSelector = mkSelector "cluster"

-- | @Selector@ for @attribute@
attributeSelector :: Selector '[] (Id NSNumber)
attributeSelector = mkSelector "attribute"

