{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSpatialEventTrigger@.
module ObjC.Intents.INSpatialEventTrigger
  ( INSpatialEventTrigger
  , IsINSpatialEventTrigger(..)
  , initWithPlacemark_event
  , placemark
  , event
  , eventSelector
  , initWithPlacemark_eventSelector
  , placemarkSelector

  -- * Enum types
  , INSpatialEvent(INSpatialEvent)
  , pattern INSpatialEventUnknown
  , pattern INSpatialEventArrive
  , pattern INSpatialEventDepart

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPlacemark:event:@
initWithPlacemark_event :: (IsINSpatialEventTrigger inSpatialEventTrigger, IsCLPlacemark placemark) => inSpatialEventTrigger -> placemark -> INSpatialEvent -> IO (Id INSpatialEventTrigger)
initWithPlacemark_event inSpatialEventTrigger placemark event =
  sendOwnedMessage inSpatialEventTrigger initWithPlacemark_eventSelector (toCLPlacemark placemark) event

-- | @- placemark@
placemark :: IsINSpatialEventTrigger inSpatialEventTrigger => inSpatialEventTrigger -> IO (Id CLPlacemark)
placemark inSpatialEventTrigger =
  sendMessage inSpatialEventTrigger placemarkSelector

-- | @- event@
event :: IsINSpatialEventTrigger inSpatialEventTrigger => inSpatialEventTrigger -> IO INSpatialEvent
event inSpatialEventTrigger =
  sendMessage inSpatialEventTrigger eventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPlacemark:event:@
initWithPlacemark_eventSelector :: Selector '[Id CLPlacemark, INSpatialEvent] (Id INSpatialEventTrigger)
initWithPlacemark_eventSelector = mkSelector "initWithPlacemark:event:"

-- | @Selector@ for @placemark@
placemarkSelector :: Selector '[] (Id CLPlacemark)
placemarkSelector = mkSelector "placemark"

-- | @Selector@ for @event@
eventSelector :: Selector '[] INSpatialEvent
eventSelector = mkSelector "event"

