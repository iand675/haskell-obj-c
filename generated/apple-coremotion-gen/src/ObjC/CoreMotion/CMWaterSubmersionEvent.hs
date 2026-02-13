{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMWaterSubmersionEvent@.
module ObjC.CoreMotion.CMWaterSubmersionEvent
  ( CMWaterSubmersionEvent
  , IsCMWaterSubmersionEvent(..)
  , date
  , state
  , dateSelector
  , stateSelector

  -- * Enum types
  , CMWaterSubmersionState(CMWaterSubmersionState)
  , pattern CMWaterSubmersionStateUnknown
  , pattern CMWaterSubmersionStateNotSubmerged
  , pattern CMWaterSubmersionStateSubmerged

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- date@
date :: IsCMWaterSubmersionEvent cmWaterSubmersionEvent => cmWaterSubmersionEvent -> IO (Id NSDate)
date cmWaterSubmersionEvent =
  sendMessage cmWaterSubmersionEvent dateSelector

-- | @- state@
state :: IsCMWaterSubmersionEvent cmWaterSubmersionEvent => cmWaterSubmersionEvent -> IO CMWaterSubmersionState
state cmWaterSubmersionEvent =
  sendMessage cmWaterSubmersionEvent stateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @state@
stateSelector :: Selector '[] CMWaterSubmersionState
stateSelector = mkSelector "state"

