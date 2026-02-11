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
  , triggerWithDateMatchingComponents_repeatsSelector
  , nextTriggerDateSelector
  , dateComponentsSelector


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

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ triggerWithDateMatchingComponents:repeats:@
triggerWithDateMatchingComponents_repeats :: IsNSDateComponents dateComponents => dateComponents -> Bool -> IO (Id UNCalendarNotificationTrigger)
triggerWithDateMatchingComponents_repeats dateComponents repeats =
  do
    cls' <- getRequiredClass "UNCalendarNotificationTrigger"
    withObjCPtr dateComponents $ \raw_dateComponents ->
      sendClassMsg cls' (mkSelector "triggerWithDateMatchingComponents:repeats:") (retPtr retVoid) [argPtr (castPtr raw_dateComponents :: Ptr ()), argCULong (if repeats then 1 else 0)] >>= retainedObject . castPtr

-- | @- nextTriggerDate@
nextTriggerDate :: IsUNCalendarNotificationTrigger unCalendarNotificationTrigger => unCalendarNotificationTrigger -> IO (Id NSDate)
nextTriggerDate unCalendarNotificationTrigger  =
  sendMsg unCalendarNotificationTrigger (mkSelector "nextTriggerDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dateComponents@
dateComponents :: IsUNCalendarNotificationTrigger unCalendarNotificationTrigger => unCalendarNotificationTrigger -> IO (Id NSDateComponents)
dateComponents unCalendarNotificationTrigger  =
  sendMsg unCalendarNotificationTrigger (mkSelector "dateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triggerWithDateMatchingComponents:repeats:@
triggerWithDateMatchingComponents_repeatsSelector :: Selector
triggerWithDateMatchingComponents_repeatsSelector = mkSelector "triggerWithDateMatchingComponents:repeats:"

-- | @Selector@ for @nextTriggerDate@
nextTriggerDateSelector :: Selector
nextTriggerDateSelector = mkSelector "nextTriggerDate"

-- | @Selector@ for @dateComponents@
dateComponentsSelector :: Selector
dateComponentsSelector = mkSelector "dateComponents"

