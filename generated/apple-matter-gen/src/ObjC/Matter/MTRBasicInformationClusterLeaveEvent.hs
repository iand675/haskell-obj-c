{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicInformationClusterLeaveEvent@.
module ObjC.Matter.MTRBasicInformationClusterLeaveEvent
  ( MTRBasicInformationClusterLeaveEvent
  , IsMTRBasicInformationClusterLeaveEvent(..)
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
fabricIndex :: IsMTRBasicInformationClusterLeaveEvent mtrBasicInformationClusterLeaveEvent => mtrBasicInformationClusterLeaveEvent -> IO (Id NSNumber)
fabricIndex mtrBasicInformationClusterLeaveEvent =
  sendMessage mtrBasicInformationClusterLeaveEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRBasicInformationClusterLeaveEvent mtrBasicInformationClusterLeaveEvent, IsNSNumber value) => mtrBasicInformationClusterLeaveEvent -> value -> IO ()
setFabricIndex mtrBasicInformationClusterLeaveEvent value =
  sendMessage mtrBasicInformationClusterLeaveEvent setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

