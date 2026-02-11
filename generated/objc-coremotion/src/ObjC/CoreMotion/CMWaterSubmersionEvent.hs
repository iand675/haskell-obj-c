{-# LANGUAGE PatternSynonyms #-}
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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- date@
date :: IsCMWaterSubmersionEvent cmWaterSubmersionEvent => cmWaterSubmersionEvent -> IO (Id NSDate)
date cmWaterSubmersionEvent  =
  sendMsg cmWaterSubmersionEvent (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- state@
state :: IsCMWaterSubmersionEvent cmWaterSubmersionEvent => cmWaterSubmersionEvent -> IO CMWaterSubmersionState
state cmWaterSubmersionEvent  =
  fmap (coerce :: CLong -> CMWaterSubmersionState) $ sendMsg cmWaterSubmersionEvent (mkSelector "state") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

