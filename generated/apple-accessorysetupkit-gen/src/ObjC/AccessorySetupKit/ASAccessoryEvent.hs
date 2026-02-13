{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Event for status and other updates.
--
-- Generated bindings for @ASAccessoryEvent@.
module ObjC.AccessorySetupKit.ASAccessoryEvent
  ( ASAccessoryEvent
  , IsASAccessoryEvent(..)
  , init_
  , new
  , eventType
  , accessory
  , error_
  , accessorySelector
  , errorSelector
  , eventTypeSelector
  , initSelector
  , newSelector

  -- * Enum types
  , ASAccessoryEventType(ASAccessoryEventType)
  , pattern ASAccessoryEventTypeUnknown
  , pattern ASAccessoryEventTypeActivated
  , pattern ASAccessoryEventTypeInvalidated
  , pattern ASAccessoryEventTypeMigrationComplete
  , pattern ASAccessoryEventTypeAccessoryAdded
  , pattern ASAccessoryEventTypeAccessoryRemoved
  , pattern ASAccessoryEventTypeAccessoryChanged
  , pattern ASAccessoryEventTypeAccessoryDiscovered
  , pattern ASAccessoryEventTypePickerDidPresent
  , pattern ASAccessoryEventTypePickerDidDismiss
  , pattern ASAccessoryEventTypePickerSetupBridging
  , pattern ASAccessoryEventTypePickerSetupFailed
  , pattern ASAccessoryEventTypePickerSetupPairing
  , pattern ASAccessoryEventTypePickerSetupRename

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.AccessorySetupKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASAccessoryEvent asAccessoryEvent => asAccessoryEvent -> IO (Id ASAccessoryEvent)
init_ asAccessoryEvent =
  sendOwnedMessage asAccessoryEvent initSelector

-- | @- new@
new :: IsASAccessoryEvent asAccessoryEvent => asAccessoryEvent -> IO (Id ASAccessoryEvent)
new asAccessoryEvent =
  sendOwnedMessage asAccessoryEvent newSelector

-- | The type of event, such as accessory addition or removal, or picker presentation or removal.
--
-- Some event types may indicate that the event is a subclass of ``ASAccessoryEvent-c.class`` that provides additional properties.
--
-- ObjC selector: @- eventType@
eventType :: IsASAccessoryEvent asAccessoryEvent => asAccessoryEvent -> IO ASAccessoryEventType
eventType asAccessoryEvent =
  sendMessage asAccessoryEvent eventTypeSelector

-- | The accessory involved in the event, if any.
--
-- The session populates this member for event types like ``ASAccessoryEventType/accessoryAdded`` and ``ASAccessoryEventType/accessoryChanged``, but not for life cycle or picker events like ``ASAccessoryEventType/activated`` or ``ASAccessoryEventType/pickerDidPresent``.
--
-- ObjC selector: @- accessory@
accessory :: IsASAccessoryEvent asAccessoryEvent => asAccessoryEvent -> IO (Id ASAccessory)
accessory asAccessoryEvent =
  sendMessage asAccessoryEvent accessorySelector

-- | The error associated with the event, if any.
--
-- ObjC selector: @- error@
error_ :: IsASAccessoryEvent asAccessoryEvent => asAccessoryEvent -> IO (Id NSError)
error_ asAccessoryEvent =
  sendMessage asAccessoryEvent errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAccessoryEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAccessoryEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @eventType@
eventTypeSelector :: Selector '[] ASAccessoryEventType
eventTypeSelector = mkSelector "eventType"

-- | @Selector@ for @accessory@
accessorySelector :: Selector '[] (Id ASAccessory)
accessorySelector = mkSelector "accessory"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

