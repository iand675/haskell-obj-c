{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSNotificationQueue@.
module ObjC.Foundation.NSNotificationQueue
  ( NSNotificationQueue
  , IsNSNotificationQueue(..)
  , initWithNotificationCenter
  , enqueueNotification_postingStyle
  , enqueueNotification_postingStyle_coalesceMask_forModes
  , dequeueNotificationsMatching_coalesceMask
  , defaultQueue
  , defaultQueueSelector
  , dequeueNotificationsMatching_coalesceMaskSelector
  , enqueueNotification_postingStyleSelector
  , enqueueNotification_postingStyle_coalesceMask_forModesSelector
  , initWithNotificationCenterSelector

  -- * Enum types
  , NSNotificationCoalescing(NSNotificationCoalescing)
  , pattern NSNotificationNoCoalescing
  , pattern NSNotificationCoalescingOnName
  , pattern NSNotificationCoalescingOnSender
  , NSPostingStyle(NSPostingStyle)
  , pattern NSPostWhenIdle
  , pattern NSPostASAP
  , pattern NSPostNow

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithNotificationCenter:@
initWithNotificationCenter :: (IsNSNotificationQueue nsNotificationQueue, IsNSNotificationCenter notificationCenter) => nsNotificationQueue -> notificationCenter -> IO (Id NSNotificationQueue)
initWithNotificationCenter nsNotificationQueue notificationCenter =
  sendOwnedMessage nsNotificationQueue initWithNotificationCenterSelector (toNSNotificationCenter notificationCenter)

-- | @- enqueueNotification:postingStyle:@
enqueueNotification_postingStyle :: (IsNSNotificationQueue nsNotificationQueue, IsNSNotification notification) => nsNotificationQueue -> notification -> NSPostingStyle -> IO ()
enqueueNotification_postingStyle nsNotificationQueue notification postingStyle =
  sendMessage nsNotificationQueue enqueueNotification_postingStyleSelector (toNSNotification notification) postingStyle

-- | @- enqueueNotification:postingStyle:coalesceMask:forModes:@
enqueueNotification_postingStyle_coalesceMask_forModes :: (IsNSNotificationQueue nsNotificationQueue, IsNSNotification notification, IsNSArray modes) => nsNotificationQueue -> notification -> NSPostingStyle -> NSNotificationCoalescing -> modes -> IO ()
enqueueNotification_postingStyle_coalesceMask_forModes nsNotificationQueue notification postingStyle coalesceMask modes =
  sendMessage nsNotificationQueue enqueueNotification_postingStyle_coalesceMask_forModesSelector (toNSNotification notification) postingStyle coalesceMask (toNSArray modes)

-- | @- dequeueNotificationsMatching:coalesceMask:@
dequeueNotificationsMatching_coalesceMask :: (IsNSNotificationQueue nsNotificationQueue, IsNSNotification notification) => nsNotificationQueue -> notification -> CULong -> IO ()
dequeueNotificationsMatching_coalesceMask nsNotificationQueue notification coalesceMask =
  sendMessage nsNotificationQueue dequeueNotificationsMatching_coalesceMaskSelector (toNSNotification notification) coalesceMask

-- | @+ defaultQueue@
defaultQueue :: IO (Id NSNotificationQueue)
defaultQueue  =
  do
    cls' <- getRequiredClass "NSNotificationQueue"
    sendClassMessage cls' defaultQueueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNotificationCenter:@
initWithNotificationCenterSelector :: Selector '[Id NSNotificationCenter] (Id NSNotificationQueue)
initWithNotificationCenterSelector = mkSelector "initWithNotificationCenter:"

-- | @Selector@ for @enqueueNotification:postingStyle:@
enqueueNotification_postingStyleSelector :: Selector '[Id NSNotification, NSPostingStyle] ()
enqueueNotification_postingStyleSelector = mkSelector "enqueueNotification:postingStyle:"

-- | @Selector@ for @enqueueNotification:postingStyle:coalesceMask:forModes:@
enqueueNotification_postingStyle_coalesceMask_forModesSelector :: Selector '[Id NSNotification, NSPostingStyle, NSNotificationCoalescing, Id NSArray] ()
enqueueNotification_postingStyle_coalesceMask_forModesSelector = mkSelector "enqueueNotification:postingStyle:coalesceMask:forModes:"

-- | @Selector@ for @dequeueNotificationsMatching:coalesceMask:@
dequeueNotificationsMatching_coalesceMaskSelector :: Selector '[Id NSNotification, CULong] ()
dequeueNotificationsMatching_coalesceMaskSelector = mkSelector "dequeueNotificationsMatching:coalesceMask:"

-- | @Selector@ for @defaultQueue@
defaultQueueSelector :: Selector '[] (Id NSNotificationQueue)
defaultQueueSelector = mkSelector "defaultQueue"

