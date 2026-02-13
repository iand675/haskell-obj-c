{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterRFIDEvent@.
module ObjC.Matter.MTREnergyEVSEClusterRFIDEvent
  ( MTREnergyEVSEClusterRFIDEvent
  , IsMTREnergyEVSEClusterRFIDEvent(..)
  , uid
  , setUid
  , setUidSelector
  , uidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- uid@
uid :: IsMTREnergyEVSEClusterRFIDEvent mtrEnergyEVSEClusterRFIDEvent => mtrEnergyEVSEClusterRFIDEvent -> IO (Id NSData)
uid mtrEnergyEVSEClusterRFIDEvent =
  sendMessage mtrEnergyEVSEClusterRFIDEvent uidSelector

-- | @- setUid:@
setUid :: (IsMTREnergyEVSEClusterRFIDEvent mtrEnergyEVSEClusterRFIDEvent, IsNSData value) => mtrEnergyEVSEClusterRFIDEvent -> value -> IO ()
setUid mtrEnergyEVSEClusterRFIDEvent value =
  sendMessage mtrEnergyEVSEClusterRFIDEvent setUidSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uid@
uidSelector :: Selector '[] (Id NSData)
uidSelector = mkSelector "uid"

-- | @Selector@ for @setUid:@
setUidSelector :: Selector '[Id NSData] ()
setUidSelector = mkSelector "setUid:"

