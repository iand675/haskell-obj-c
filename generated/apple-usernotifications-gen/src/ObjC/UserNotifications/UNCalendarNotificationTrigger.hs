{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNCalendarNotificationTrigger@.
module ObjC.UserNotifications.UNCalendarNotificationTrigger
  ( UNCalendarNotificationTrigger
  , IsUNCalendarNotificationTrigger(..)
  , triggerWithDateMatchingComponents_repeats
  , nextTriggerDate
  , dateComponents
  , dateComponentsSelector
  , nextTriggerDateSelector
  , triggerWithDateMatchingComponents_repeatsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ triggerWithDateMatchingComponents:repeats:@
triggerWithDateMatchingComponents_repeats :: IsNSDateComponents dateComponents => dateComponents -> Bool -> IO (Id UNCalendarNotificationTrigger)
triggerWithDateMatchingComponents_repeats dateComponents repeats =
  do
    cls' <- getRequiredClass "UNCalendarNotificationTrigger"
    sendClassMessage cls' triggerWithDateMatchingComponents_repeatsSelector (toNSDateComponents dateComponents) repeats

-- | @- nextTriggerDate@
nextTriggerDate :: IsUNCalendarNotificationTrigger unCalendarNotificationTrigger => unCalendarNotificationTrigger -> IO (Id NSDate)
nextTriggerDate unCalendarNotificationTrigger =
  sendMessage unCalendarNotificationTrigger nextTriggerDateSelector

-- | @- dateComponents@
dateComponents :: IsUNCalendarNotificationTrigger unCalendarNotificationTrigger => unCalendarNotificationTrigger -> IO (Id NSDateComponents)
dateComponents unCalendarNotificationTrigger =
  sendMessage unCalendarNotificationTrigger dateComponentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triggerWithDateMatchingComponents:repeats:@
triggerWithDateMatchingComponents_repeatsSelector :: Selector '[Id NSDateComponents, Bool] (Id UNCalendarNotificationTrigger)
triggerWithDateMatchingComponents_repeatsSelector = mkSelector "triggerWithDateMatchingComponents:repeats:"

-- | @Selector@ for @nextTriggerDate@
nextTriggerDateSelector :: Selector '[] (Id NSDate)
nextTriggerDateSelector = mkSelector "nextTriggerDate"

-- | @Selector@ for @dateComponents@
dateComponentsSelector :: Selector '[] (Id NSDateComponents)
dateComponentsSelector = mkSelector "dateComponents"

