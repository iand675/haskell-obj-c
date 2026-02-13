{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRNotificationUsage@.
module ObjC.SensorKit.SRNotificationUsage
  ( SRNotificationUsage
  , IsSRNotificationUsage(..)
  , bundleIdentifier
  , event
  , bundleIdentifierSelector
  , eventSelector

  -- * Enum types
  , SRNotificationEvent(SRNotificationEvent)
  , pattern SRNotificationEventUnknown
  , pattern SRNotificationEventReceived
  , pattern SRNotificationEventDefaultAction
  , pattern SRNotificationEventSupplementaryAction
  , pattern SRNotificationEventClear
  , pattern SRNotificationEventNotificationCenterClearAll
  , pattern SRNotificationEventRemoved
  , pattern SRNotificationEventHide
  , pattern SRNotificationEventLongLook
  , pattern SRNotificationEventSilence
  , pattern SRNotificationEventAppLaunch
  , pattern SRNotificationEventExpired
  , pattern SRNotificationEventBannerPulldown
  , pattern SRNotificationEventTapCoalesce
  , pattern SRNotificationEventDeduped
  , pattern SRNotificationEventDeviceActivated
  , pattern SRNotificationEventDeviceUnlocked

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

-- | The bundle identifier of the application that corresponds to the notification. Only populated for Apple apps.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsSRNotificationUsage srNotificationUsage => srNotificationUsage -> IO (Id NSString)
bundleIdentifier srNotificationUsage =
  sendMessage srNotificationUsage bundleIdentifierSelector

-- | @- event@
event :: IsSRNotificationUsage srNotificationUsage => srNotificationUsage -> IO SRNotificationEvent
event srNotificationUsage =
  sendMessage srNotificationUsage eventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector '[] (Id NSString)
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @event@
eventSelector :: Selector '[] SRNotificationEvent
eventSelector = mkSelector "event"

