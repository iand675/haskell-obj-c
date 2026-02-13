{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTicketedEvent@.
module ObjC.Intents.INTicketedEvent
  ( INTicketedEvent
  , IsINTicketedEvent(..)
  , init_
  , initWithCategory_name_eventDuration_location
  , category
  , name
  , eventDuration
  , location
  , categorySelector
  , eventDurationSelector
  , initSelector
  , initWithCategory_name_eventDuration_locationSelector
  , locationSelector
  , nameSelector

  -- * Enum types
  , INTicketedEventCategory(INTicketedEventCategory)
  , pattern INTicketedEventCategoryUnknown
  , pattern INTicketedEventCategoryMovie

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

-- | @- init@
init_ :: IsINTicketedEvent inTicketedEvent => inTicketedEvent -> IO (Id INTicketedEvent)
init_ inTicketedEvent =
  sendOwnedMessage inTicketedEvent initSelector

-- | @- initWithCategory:name:eventDuration:location:@
initWithCategory_name_eventDuration_location :: (IsINTicketedEvent inTicketedEvent, IsNSString name, IsINDateComponentsRange eventDuration, IsCLPlacemark location) => inTicketedEvent -> INTicketedEventCategory -> name -> eventDuration -> location -> IO (Id INTicketedEvent)
initWithCategory_name_eventDuration_location inTicketedEvent category name eventDuration location =
  sendOwnedMessage inTicketedEvent initWithCategory_name_eventDuration_locationSelector category (toNSString name) (toINDateComponentsRange eventDuration) (toCLPlacemark location)

-- | @- category@
category :: IsINTicketedEvent inTicketedEvent => inTicketedEvent -> IO INTicketedEventCategory
category inTicketedEvent =
  sendMessage inTicketedEvent categorySelector

-- | @- name@
name :: IsINTicketedEvent inTicketedEvent => inTicketedEvent -> IO (Id NSString)
name inTicketedEvent =
  sendMessage inTicketedEvent nameSelector

-- | @- eventDuration@
eventDuration :: IsINTicketedEvent inTicketedEvent => inTicketedEvent -> IO (Id INDateComponentsRange)
eventDuration inTicketedEvent =
  sendMessage inTicketedEvent eventDurationSelector

-- | @- location@
location :: IsINTicketedEvent inTicketedEvent => inTicketedEvent -> IO (Id CLPlacemark)
location inTicketedEvent =
  sendMessage inTicketedEvent locationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INTicketedEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCategory:name:eventDuration:location:@
initWithCategory_name_eventDuration_locationSelector :: Selector '[INTicketedEventCategory, Id NSString, Id INDateComponentsRange, Id CLPlacemark] (Id INTicketedEvent)
initWithCategory_name_eventDuration_locationSelector = mkSelector "initWithCategory:name:eventDuration:location:"

-- | @Selector@ for @category@
categorySelector :: Selector '[] INTicketedEventCategory
categorySelector = mkSelector "category"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @eventDuration@
eventDurationSelector :: Selector '[] (Id INDateComponentsRange)
eventDurationSelector = mkSelector "eventDuration"

-- | @Selector@ for @location@
locationSelector :: Selector '[] (Id CLPlacemark)
locationSelector = mkSelector "location"

