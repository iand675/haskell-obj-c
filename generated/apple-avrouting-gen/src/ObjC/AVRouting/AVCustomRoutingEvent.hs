{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , route
  , reasonSelector
  , routeSelector

  -- * Enum types
  , AVCustomRoutingEventReason(AVCustomRoutingEventReason)
  , pattern AVCustomRoutingEventReasonActivate
  , pattern AVCustomRoutingEventReasonDeactivate
  , pattern AVCustomRoutingEventReasonReactivate

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVRouting.Internal.Classes
import ObjC.AVRouting.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | A reason for an event, such as a user request to activate or deactivate a route.
--
-- ObjC selector: @- reason@
reason :: IsAVCustomRoutingEvent avCustomRoutingEvent => avCustomRoutingEvent -> IO AVCustomRoutingEventReason
reason avCustomRoutingEvent =
  sendMessage avCustomRoutingEvent reasonSelector

-- | A route for the event.
--
-- ObjC selector: @- route@
route :: IsAVCustomRoutingEvent avCustomRoutingEvent => avCustomRoutingEvent -> IO (Id AVCustomDeviceRoute)
route avCustomRoutingEvent =
  sendMessage avCustomRoutingEvent routeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] AVCustomRoutingEventReason
reasonSelector = mkSelector "reason"

-- | @Selector@ for @route@
routeSelector :: Selector '[] (Id AVCustomDeviceRoute)
routeSelector = mkSelector "route"

