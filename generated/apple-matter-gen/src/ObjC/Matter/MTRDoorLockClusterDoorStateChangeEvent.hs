{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterDoorStateChangeEvent@.
module ObjC.Matter.MTRDoorLockClusterDoorStateChangeEvent
  ( MTRDoorLockClusterDoorStateChangeEvent
  , IsMTRDoorLockClusterDoorStateChangeEvent(..)
  , doorState
  , setDoorState
  , doorStateSelector
  , setDoorStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- doorState@
doorState :: IsMTRDoorLockClusterDoorStateChangeEvent mtrDoorLockClusterDoorStateChangeEvent => mtrDoorLockClusterDoorStateChangeEvent -> IO (Id NSNumber)
doorState mtrDoorLockClusterDoorStateChangeEvent =
  sendMessage mtrDoorLockClusterDoorStateChangeEvent doorStateSelector

-- | @- setDoorState:@
setDoorState :: (IsMTRDoorLockClusterDoorStateChangeEvent mtrDoorLockClusterDoorStateChangeEvent, IsNSNumber value) => mtrDoorLockClusterDoorStateChangeEvent -> value -> IO ()
setDoorState mtrDoorLockClusterDoorStateChangeEvent value =
  sendMessage mtrDoorLockClusterDoorStateChangeEvent setDoorStateSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @doorState@
doorStateSelector :: Selector '[] (Id NSNumber)
doorStateSelector = mkSelector "doorState"

-- | @Selector@ for @setDoorState:@
setDoorStateSelector :: Selector '[Id NSNumber] ()
setDoorStateSelector = mkSelector "setDoorState:"

