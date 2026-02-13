{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterManagementClusterBoostStartedEvent@.
module ObjC.Matter.MTRWaterHeaterManagementClusterBoostStartedEvent
  ( MTRWaterHeaterManagementClusterBoostStartedEvent
  , IsMTRWaterHeaterManagementClusterBoostStartedEvent(..)
  , boostInfo
  , setBoostInfo
  , boostInfoSelector
  , setBoostInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- boostInfo@
boostInfo :: IsMTRWaterHeaterManagementClusterBoostStartedEvent mtrWaterHeaterManagementClusterBoostStartedEvent => mtrWaterHeaterManagementClusterBoostStartedEvent -> IO (Id MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct)
boostInfo mtrWaterHeaterManagementClusterBoostStartedEvent =
  sendMessage mtrWaterHeaterManagementClusterBoostStartedEvent boostInfoSelector

-- | @- setBoostInfo:@
setBoostInfo :: (IsMTRWaterHeaterManagementClusterBoostStartedEvent mtrWaterHeaterManagementClusterBoostStartedEvent, IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value) => mtrWaterHeaterManagementClusterBoostStartedEvent -> value -> IO ()
setBoostInfo mtrWaterHeaterManagementClusterBoostStartedEvent value =
  sendMessage mtrWaterHeaterManagementClusterBoostStartedEvent setBoostInfoSelector (toMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boostInfo@
boostInfoSelector :: Selector '[] (Id MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct)
boostInfoSelector = mkSelector "boostInfo"

-- | @Selector@ for @setBoostInfo:@
setBoostInfoSelector :: Selector '[Id MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct] ()
setBoostInfoSelector = mkSelector "setBoostInfo:"

