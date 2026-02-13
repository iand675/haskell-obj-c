{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestFabricScopedEventEvent@.
module ObjC.Matter.MTRUnitTestingClusterTestFabricScopedEventEvent
  ( MTRUnitTestingClusterTestFabricScopedEventEvent
  , IsMTRUnitTestingClusterTestFabricScopedEventEvent(..)
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
fabricIndex :: IsMTRUnitTestingClusterTestFabricScopedEventEvent mtrUnitTestingClusterTestFabricScopedEventEvent => mtrUnitTestingClusterTestFabricScopedEventEvent -> IO (Id NSNumber)
fabricIndex mtrUnitTestingClusterTestFabricScopedEventEvent =
  sendMessage mtrUnitTestingClusterTestFabricScopedEventEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRUnitTestingClusterTestFabricScopedEventEvent mtrUnitTestingClusterTestFabricScopedEventEvent, IsNSNumber value) => mtrUnitTestingClusterTestFabricScopedEventEvent -> value -> IO ()
setFabricIndex mtrUnitTestingClusterTestFabricScopedEventEvent value =
  sendMessage mtrUnitTestingClusterTestFabricScopedEventEvent setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

