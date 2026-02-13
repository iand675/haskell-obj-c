{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccountLoginClusterLoggedOutEvent@.
module ObjC.Matter.MTRAccountLoginClusterLoggedOutEvent
  ( MTRAccountLoginClusterLoggedOutEvent
  , IsMTRAccountLoginClusterLoggedOutEvent(..)
  , node
  , setNode
  , fabricIndex
  , setFabricIndex
  , fabricIndexSelector
  , nodeSelector
  , setFabricIndexSelector
  , setNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- node@
node :: IsMTRAccountLoginClusterLoggedOutEvent mtrAccountLoginClusterLoggedOutEvent => mtrAccountLoginClusterLoggedOutEvent -> IO (Id NSNumber)
node mtrAccountLoginClusterLoggedOutEvent =
  sendMessage mtrAccountLoginClusterLoggedOutEvent nodeSelector

-- | @- setNode:@
setNode :: (IsMTRAccountLoginClusterLoggedOutEvent mtrAccountLoginClusterLoggedOutEvent, IsNSNumber value) => mtrAccountLoginClusterLoggedOutEvent -> value -> IO ()
setNode mtrAccountLoginClusterLoggedOutEvent value =
  sendMessage mtrAccountLoginClusterLoggedOutEvent setNodeSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRAccountLoginClusterLoggedOutEvent mtrAccountLoginClusterLoggedOutEvent => mtrAccountLoginClusterLoggedOutEvent -> IO (Id NSNumber)
fabricIndex mtrAccountLoginClusterLoggedOutEvent =
  sendMessage mtrAccountLoginClusterLoggedOutEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccountLoginClusterLoggedOutEvent mtrAccountLoginClusterLoggedOutEvent, IsNSNumber value) => mtrAccountLoginClusterLoggedOutEvent -> value -> IO ()
setFabricIndex mtrAccountLoginClusterLoggedOutEvent value =
  sendMessage mtrAccountLoginClusterLoggedOutEvent setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @node@
nodeSelector :: Selector '[] (Id NSNumber)
nodeSelector = mkSelector "node"

-- | @Selector@ for @setNode:@
setNodeSelector :: Selector '[Id NSNumber] ()
setNodeSelector = mkSelector "setNode:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

