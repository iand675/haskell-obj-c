{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating a specific cluster on a device (i.e. without any wildcards).
--
-- Generated bindings for @MTRClusterPath@.
module ObjC.Matter.MTRClusterPath
  ( MTRClusterPath
  , IsMTRClusterPath(..)
  , clusterPathWithEndpointID_clusterID
  , init_
  , new
  , endpoint
  , cluster
  , clusterPathWithEndpointID_clusterIDSelector
  , clusterSelector
  , endpointSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ clusterPathWithEndpointID:clusterID:@
clusterPathWithEndpointID_clusterID :: (IsNSNumber endpointID, IsNSNumber clusterID) => endpointID -> clusterID -> IO (Id MTRClusterPath)
clusterPathWithEndpointID_clusterID endpointID clusterID =
  do
    cls' <- getRequiredClass "MTRClusterPath"
    sendClassMessage cls' clusterPathWithEndpointID_clusterIDSelector (toNSNumber endpointID) (toNSNumber clusterID)

-- | @- init@
init_ :: IsMTRClusterPath mtrClusterPath => mtrClusterPath -> IO (Id MTRClusterPath)
init_ mtrClusterPath =
  sendOwnedMessage mtrClusterPath initSelector

-- | @+ new@
new :: IO (Id MTRClusterPath)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPath"
    sendOwnedClassMessage cls' newSelector

-- | @- endpoint@
endpoint :: IsMTRClusterPath mtrClusterPath => mtrClusterPath -> IO (Id NSNumber)
endpoint mtrClusterPath =
  sendMessage mtrClusterPath endpointSelector

-- | @- cluster@
cluster :: IsMTRClusterPath mtrClusterPath => mtrClusterPath -> IO (Id NSNumber)
cluster mtrClusterPath =
  sendMessage mtrClusterPath clusterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clusterPathWithEndpointID:clusterID:@
clusterPathWithEndpointID_clusterIDSelector :: Selector '[Id NSNumber, Id NSNumber] (Id MTRClusterPath)
clusterPathWithEndpointID_clusterIDSelector = mkSelector "clusterPathWithEndpointID:clusterID:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRClusterPath)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterPath)
newSelector = mkSelector "new"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @cluster@
clusterSelector :: Selector '[] (Id NSNumber)
clusterSelector = mkSelector "cluster"

