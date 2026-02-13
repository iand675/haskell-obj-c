{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMPedometerEvent@.
module ObjC.CoreMotion.CMPedometerEvent
  ( CMPedometerEvent
  , IsCMPedometerEvent(..)
  , date
  , type_
  , dateSelector
  , typeSelector

  -- * Enum types
  , CMPedometerEventType(CMPedometerEventType)
  , pattern CMPedometerEventTypePause
  , pattern CMPedometerEventTypeResume

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
date :: IsCMPedometerEvent cmPedometerEvent => cmPedometerEvent -> IO (Id NSDate)
date cmPedometerEvent =
  sendMessage cmPedometerEvent dateSelector

-- | @- type@
type_ :: IsCMPedometerEvent cmPedometerEvent => cmPedometerEvent -> IO CMPedometerEventType
type_ cmPedometerEvent =
  sendMessage cmPedometerEvent typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CMPedometerEventType
typeSelector = mkSelector "type"

