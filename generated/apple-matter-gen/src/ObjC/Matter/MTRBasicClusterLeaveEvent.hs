{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicClusterLeaveEvent@.
module ObjC.Matter.MTRBasicClusterLeaveEvent
  ( MTRBasicClusterLeaveEvent
  , IsMTRBasicClusterLeaveEvent(..)
  , fabricIndex
  , setFabricIndex
  , fabricIndexSelector
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

-- | @- fabricIndex@
fabricIndex :: IsMTRBasicClusterLeaveEvent mtrBasicClusterLeaveEvent => mtrBasicClusterLeaveEvent -> IO (Id NSNumber)
fabricIndex mtrBasicClusterLeaveEvent =
  sendMessage mtrBasicClusterLeaveEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRBasicClusterLeaveEvent mtrBasicClusterLeaveEvent, IsNSNumber value) => mtrBasicClusterLeaveEvent -> value -> IO ()
setFabricIndex mtrBasicClusterLeaveEvent value =
  sendMessage mtrBasicClusterLeaveEvent setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

