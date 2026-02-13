{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEVConnectedEvent@.
module ObjC.Matter.MTREnergyEVSEClusterEVConnectedEvent
  ( MTREnergyEVSEClusterEVConnectedEvent
  , IsMTREnergyEVSEClusterEVConnectedEvent(..)
  , sessionID
  , setSessionID
  , sessionIDSelector
  , setSessionIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sessionID@
sessionID :: IsMTREnergyEVSEClusterEVConnectedEvent mtrEnergyEVSEClusterEVConnectedEvent => mtrEnergyEVSEClusterEVConnectedEvent -> IO (Id NSNumber)
sessionID mtrEnergyEVSEClusterEVConnectedEvent =
  sendMessage mtrEnergyEVSEClusterEVConnectedEvent sessionIDSelector

-- | @- setSessionID:@
setSessionID :: (IsMTREnergyEVSEClusterEVConnectedEvent mtrEnergyEVSEClusterEVConnectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVConnectedEvent -> value -> IO ()
setSessionID mtrEnergyEVSEClusterEVConnectedEvent value =
  sendMessage mtrEnergyEVSEClusterEVConnectedEvent setSessionIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector '[] (Id NSNumber)
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @setSessionID:@
setSessionIDSelector :: Selector '[Id NSNumber] ()
setSessionIDSelector = mkSelector "setSessionID:"

