{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating a specific command on a device (i.e. without any wildcards).
--
-- Generated bindings for @MTRCommandPath@.
module ObjC.Matter.MTRCommandPath
  ( MTRCommandPath
  , IsMTRCommandPath(..)
  , commandPathWithEndpointID_clusterID_commandID
  , commandPathWithEndpointId_clusterId_commandId
  , command
  , commandPathWithEndpointID_clusterID_commandIDSelector
  , commandPathWithEndpointId_clusterId_commandIdSelector
  , commandSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ commandPathWithEndpointID:clusterID:commandID:@
commandPathWithEndpointID_clusterID_commandID :: (IsNSNumber endpointID, IsNSNumber clusterID, IsNSNumber commandID) => endpointID -> clusterID -> commandID -> IO (Id MTRCommandPath)
commandPathWithEndpointID_clusterID_commandID endpointID clusterID commandID =
  do
    cls' <- getRequiredClass "MTRCommandPath"
    sendClassMessage cls' commandPathWithEndpointID_clusterID_commandIDSelector (toNSNumber endpointID) (toNSNumber clusterID) (toNSNumber commandID)

-- | @+ commandPathWithEndpointId:clusterId:commandId:@
commandPathWithEndpointId_clusterId_commandId :: (IsNSNumber endpointId, IsNSNumber clusterId, IsNSNumber commandId) => endpointId -> clusterId -> commandId -> IO (Id MTRCommandPath)
commandPathWithEndpointId_clusterId_commandId endpointId clusterId commandId =
  do
    cls' <- getRequiredClass "MTRCommandPath"
    sendClassMessage cls' commandPathWithEndpointId_clusterId_commandIdSelector (toNSNumber endpointId) (toNSNumber clusterId) (toNSNumber commandId)

-- | @- command@
command :: IsMTRCommandPath mtrCommandPath => mtrCommandPath -> IO (Id NSNumber)
command mtrCommandPath =
  sendMessage mtrCommandPath commandSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @commandPathWithEndpointID:clusterID:commandID:@
commandPathWithEndpointID_clusterID_commandIDSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber] (Id MTRCommandPath)
commandPathWithEndpointID_clusterID_commandIDSelector = mkSelector "commandPathWithEndpointID:clusterID:commandID:"

-- | @Selector@ for @commandPathWithEndpointId:clusterId:commandId:@
commandPathWithEndpointId_clusterId_commandIdSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSNumber] (Id MTRCommandPath)
commandPathWithEndpointId_clusterId_commandIdSelector = mkSelector "commandPathWithEndpointId:clusterId:commandId:"

-- | @Selector@ for @command@
commandSelector :: Selector '[] (Id NSNumber)
commandSelector = mkSelector "command"

