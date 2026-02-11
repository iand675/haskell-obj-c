{-# LANGUAGE PatternSynonyms #-}
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
  , initWithNotificationCenterSelector
  , enqueueNotification_postingStyleSelector
  , enqueueNotification_postingStyle_coalesceMask_forModesSelector
  , dequeueNotificationsMatching_coalesceMaskSelector
  , defaultQueueSelector

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithNotificationCenter:@
initWithNotificationCenter :: (IsNSNotificationQueue nsNotificationQueue, IsNSNotificationCenter notificationCenter) => nsNotificationQueue -> notificationCenter -> IO (Id NSNotificationQueue)
initWithNotificationCenter nsNotificationQueue  notificationCenter =
withObjCPtr notificationCenter $ \raw_notificationCenter ->
    sendMsg nsNotificationQueue (mkSelector "initWithNotificationCenter:") (retPtr retVoid) [argPtr (castPtr raw_notificationCenter :: Ptr ())] >>= ownedObject . castPtr

-- | @- enqueueNotification:postingStyle:@
enqueueNotification_postingStyle :: (IsNSNotificationQueue nsNotificationQueue, IsNSNotification notification) => nsNotificationQueue -> notification -> NSPostingStyle -> IO ()
enqueueNotification_postingStyle nsNotificationQueue  notification postingStyle =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsNotificationQueue (mkSelector "enqueueNotification:postingStyle:") retVoid [argPtr (castPtr raw_notification :: Ptr ()), argCULong (coerce postingStyle)]

-- | @- enqueueNotification:postingStyle:coalesceMask:forModes:@
enqueueNotification_postingStyle_coalesceMask_forModes :: (IsNSNotificationQueue nsNotificationQueue, IsNSNotification notification, IsNSArray modes) => nsNotificationQueue -> notification -> NSPostingStyle -> NSNotificationCoalescing -> modes -> IO ()
enqueueNotification_postingStyle_coalesceMask_forModes nsNotificationQueue  notification postingStyle coalesceMask modes =
withObjCPtr notification $ \raw_notification ->
  withObjCPtr modes $ \raw_modes ->
      sendMsg nsNotificationQueue (mkSelector "enqueueNotification:postingStyle:coalesceMask:forModes:") retVoid [argPtr (castPtr raw_notification :: Ptr ()), argCULong (coerce postingStyle), argCULong (coerce coalesceMask), argPtr (castPtr raw_modes :: Ptr ())]

-- | @- dequeueNotificationsMatching:coalesceMask:@
dequeueNotificationsMatching_coalesceMask :: (IsNSNotificationQueue nsNotificationQueue, IsNSNotification notification) => nsNotificationQueue -> notification -> CULong -> IO ()
dequeueNotificationsMatching_coalesceMask nsNotificationQueue  notification coalesceMask =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsNotificationQueue (mkSelector "dequeueNotificationsMatching:coalesceMask:") retVoid [argPtr (castPtr raw_notification :: Ptr ()), argCULong (fromIntegral coalesceMask)]

-- | @+ defaultQueue@
defaultQueue :: IO (Id NSNotificationQueue)
defaultQueue  =
  do
    cls' <- getRequiredClass "NSNotificationQueue"
    sendClassMsg cls' (mkSelector "defaultQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNotificationCenter:@
initWithNotificationCenterSelector :: Selector
initWithNotificationCenterSelector = mkSelector "initWithNotificationCenter:"

-- | @Selector@ for @enqueueNotification:postingStyle:@
enqueueNotification_postingStyleSelector :: Selector
enqueueNotification_postingStyleSelector = mkSelector "enqueueNotification:postingStyle:"

-- | @Selector@ for @enqueueNotification:postingStyle:coalesceMask:forModes:@
enqueueNotification_postingStyle_coalesceMask_forModesSelector :: Selector
enqueueNotification_postingStyle_coalesceMask_forModesSelector = mkSelector "enqueueNotification:postingStyle:coalesceMask:forModes:"

-- | @Selector@ for @dequeueNotificationsMatching:coalesceMask:@
dequeueNotificationsMatching_coalesceMaskSelector :: Selector
dequeueNotificationsMatching_coalesceMaskSelector = mkSelector "dequeueNotificationsMatching:coalesceMask:"

-- | @Selector@ for @defaultQueue@
defaultQueueSelector :: Selector
defaultQueueSelector = mkSelector "defaultQueue"

