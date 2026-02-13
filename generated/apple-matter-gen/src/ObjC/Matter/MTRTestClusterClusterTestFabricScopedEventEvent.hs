{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestFabricScopedEventEvent@.
module ObjC.Matter.MTRTestClusterClusterTestFabricScopedEventEvent
  ( MTRTestClusterClusterTestFabricScopedEventEvent
  , IsMTRTestClusterClusterTestFabricScopedEventEvent(..)
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
fabricIndex :: IsMTRTestClusterClusterTestFabricScopedEventEvent mtrTestClusterClusterTestFabricScopedEventEvent => mtrTestClusterClusterTestFabricScopedEventEvent -> IO (Id NSNumber)
fabricIndex mtrTestClusterClusterTestFabricScopedEventEvent =
  sendMessage mtrTestClusterClusterTestFabricScopedEventEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTestClusterClusterTestFabricScopedEventEvent mtrTestClusterClusterTestFabricScopedEventEvent, IsNSNumber value) => mtrTestClusterClusterTestFabricScopedEventEvent -> value -> IO ()
setFabricIndex mtrTestClusterClusterTestFabricScopedEventEvent value =
  sendMessage mtrTestClusterClusterTestFabricScopedEventEvent setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

