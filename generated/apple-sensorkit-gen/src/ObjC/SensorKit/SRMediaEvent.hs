{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRMediaEvent@.
module ObjC.SensorKit.SRMediaEvent
  ( SRMediaEvent
  , IsSRMediaEvent(..)
  , mediaIdentifier
  , eventType
  , eventTypeSelector
  , mediaIdentifierSelector

  -- * Enum types
  , SRMediaEventType(SRMediaEventType)
  , pattern SRMediaEventOnScreen
  , pattern SRMediaEventOffScreen

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | mediaIdentifier
--
-- Unique media identifier
--
-- Unique media identifier to track a specific media object.
--
-- ObjC selector: @- mediaIdentifier@
mediaIdentifier :: IsSRMediaEvent srMediaEvent => srMediaEvent -> IO (Id NSString)
mediaIdentifier srMediaEvent =
  sendMessage srMediaEvent mediaIdentifierSelector

-- | eventType
--
-- Type of the event
--
-- Type of media event (e.g., media has been displayed on a screen).
--
-- ObjC selector: @- eventType@
eventType :: IsSRMediaEvent srMediaEvent => srMediaEvent -> IO SRMediaEventType
eventType srMediaEvent =
  sendMessage srMediaEvent eventTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaIdentifier@
mediaIdentifierSelector :: Selector '[] (Id NSString)
mediaIdentifierSelector = mkSelector "mediaIdentifier"

-- | @Selector@ for @eventType@
eventTypeSelector :: Selector '[] SRMediaEventType
eventTypeSelector = mkSelector "eventType"

