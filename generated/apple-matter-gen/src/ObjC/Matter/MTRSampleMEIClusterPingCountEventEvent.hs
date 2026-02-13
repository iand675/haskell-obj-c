{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSampleMEIClusterPingCountEventEvent@.
module ObjC.Matter.MTRSampleMEIClusterPingCountEventEvent
  ( MTRSampleMEIClusterPingCountEventEvent
  , IsMTRSampleMEIClusterPingCountEventEvent(..)
  , count
  , setCount
  , fabricIndex
  , setFabricIndex
  , countSelector
  , fabricIndexSelector
  , setCountSelector
  , setFabricIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- count@
count :: IsMTRSampleMEIClusterPingCountEventEvent mtrSampleMEIClusterPingCountEventEvent => mtrSampleMEIClusterPingCountEventEvent -> IO (Id NSNumber)
count mtrSampleMEIClusterPingCountEventEvent =
  sendMessage mtrSampleMEIClusterPingCountEventEvent countSelector

-- | @- setCount:@
setCount :: (IsMTRSampleMEIClusterPingCountEventEvent mtrSampleMEIClusterPingCountEventEvent, IsNSNumber value) => mtrSampleMEIClusterPingCountEventEvent -> value -> IO ()
setCount mtrSampleMEIClusterPingCountEventEvent value =
  sendMessage mtrSampleMEIClusterPingCountEventEvent setCountSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRSampleMEIClusterPingCountEventEvent mtrSampleMEIClusterPingCountEventEvent => mtrSampleMEIClusterPingCountEventEvent -> IO (Id NSNumber)
fabricIndex mtrSampleMEIClusterPingCountEventEvent =
  sendMessage mtrSampleMEIClusterPingCountEventEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRSampleMEIClusterPingCountEventEvent mtrSampleMEIClusterPingCountEventEvent, IsNSNumber value) => mtrSampleMEIClusterPingCountEventEvent -> value -> IO ()
setFabricIndex mtrSampleMEIClusterPingCountEventEvent value =
  sendMessage mtrSampleMEIClusterPingCountEventEvent setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @count@
countSelector :: Selector '[] (Id NSNumber)
countSelector = mkSelector "count"

-- | @Selector@ for @setCount:@
setCountSelector :: Selector '[Id NSNumber] ()
setCountSelector = mkSelector "setCount:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

