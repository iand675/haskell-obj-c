{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents an event that occurs on a route.
--
-- Depending on the route’s reason, apps establish or tear down a connection to a specified route.
--
-- Generated bindings for @AVCustomRoutingEvent@.
module ObjC.AVRouting.AVCustomRoutingEvent
  ( AVCustomRoutingEvent
  , IsAVCustomRoutingEvent(..)
  , reason
  , reasonSelector

  -- * Enum types
  , AVCustomRoutingEventReason(AVCustomRoutingEventReason)
  , pattern AVCustomRoutingEventReasonActivate
  , pattern AVCustomRoutingEventReasonDeactivate
  , pattern AVCustomRoutingEventReasonReactivate

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

import ObjC.AVRouting.Internal.Classes
import ObjC.AVRouting.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | A reason for an event, such as a user request to activate or deactivate a route.
--
-- ObjC selector: @- reason@
reason :: IsAVCustomRoutingEvent avCustomRoutingEvent => avCustomRoutingEvent -> IO AVCustomRoutingEventReason
reason avCustomRoutingEvent  =
  fmap (coerce :: CLong -> AVCustomRoutingEventReason) $ sendMsg avCustomRoutingEvent (mkSelector "reason") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reason@
reasonSelector :: Selector
reasonSelector = mkSelector "reason"

