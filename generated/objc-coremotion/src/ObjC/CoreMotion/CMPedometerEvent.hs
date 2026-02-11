{-# LANGUAGE PatternSynonyms #-}
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
date :: IsCMPedometerEvent cmPedometerEvent => cmPedometerEvent -> IO (Id NSDate)
date cmPedometerEvent  =
  sendMsg cmPedometerEvent (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsCMPedometerEvent cmPedometerEvent => cmPedometerEvent -> IO CMPedometerEventType
type_ cmPedometerEvent  =
  fmap (coerce :: CLong -> CMPedometerEventType) $ sendMsg cmPedometerEvent (mkSelector "type") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

