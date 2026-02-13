{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNTimeIntervalNotificationTrigger@.
module ObjC.UserNotifications.UNTimeIntervalNotificationTrigger
  ( UNTimeIntervalNotificationTrigger
  , IsUNTimeIntervalNotificationTrigger(..)
  , triggerWithTimeInterval_repeats
  , nextTriggerDate
  , timeInterval
  , nextTriggerDateSelector
  , timeIntervalSelector
  , triggerWithTimeInterval_repeatsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ triggerWithTimeInterval:repeats:@
triggerWithTimeInterval_repeats :: CDouble -> Bool -> IO (Id UNTimeIntervalNotificationTrigger)
triggerWithTimeInterval_repeats timeInterval repeats =
  do
    cls' <- getRequiredClass "UNTimeIntervalNotificationTrigger"
    sendClassMessage cls' triggerWithTimeInterval_repeatsSelector timeInterval repeats

-- | @- nextTriggerDate@
nextTriggerDate :: IsUNTimeIntervalNotificationTrigger unTimeIntervalNotificationTrigger => unTimeIntervalNotificationTrigger -> IO (Id NSDate)
nextTriggerDate unTimeIntervalNotificationTrigger =
  sendMessage unTimeIntervalNotificationTrigger nextTriggerDateSelector

-- | @- timeInterval@
timeInterval :: IsUNTimeIntervalNotificationTrigger unTimeIntervalNotificationTrigger => unTimeIntervalNotificationTrigger -> IO CDouble
timeInterval unTimeIntervalNotificationTrigger =
  sendMessage unTimeIntervalNotificationTrigger timeIntervalSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triggerWithTimeInterval:repeats:@
triggerWithTimeInterval_repeatsSelector :: Selector '[CDouble, Bool] (Id UNTimeIntervalNotificationTrigger)
triggerWithTimeInterval_repeatsSelector = mkSelector "triggerWithTimeInterval:repeats:"

-- | @Selector@ for @nextTriggerDate@
nextTriggerDateSelector :: Selector '[] (Id NSDate)
nextTriggerDateSelector = mkSelector "nextTriggerDate"

-- | @Selector@ for @timeInterval@
timeIntervalSelector :: Selector '[] CDouble
timeIntervalSelector = mkSelector "timeInterval"

