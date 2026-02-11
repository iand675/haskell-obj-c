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
  , triggerWithTimeInterval_repeatsSelector
  , nextTriggerDateSelector
  , timeIntervalSelector


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

-- | @+ triggerWithTimeInterval:repeats:@
triggerWithTimeInterval_repeats :: CDouble -> Bool -> IO (Id UNTimeIntervalNotificationTrigger)
triggerWithTimeInterval_repeats timeInterval repeats =
  do
    cls' <- getRequiredClass "UNTimeIntervalNotificationTrigger"
    sendClassMsg cls' (mkSelector "triggerWithTimeInterval:repeats:") (retPtr retVoid) [argCDouble (fromIntegral timeInterval), argCULong (if repeats then 1 else 0)] >>= retainedObject . castPtr

-- | @- nextTriggerDate@
nextTriggerDate :: IsUNTimeIntervalNotificationTrigger unTimeIntervalNotificationTrigger => unTimeIntervalNotificationTrigger -> IO (Id NSDate)
nextTriggerDate unTimeIntervalNotificationTrigger  =
  sendMsg unTimeIntervalNotificationTrigger (mkSelector "nextTriggerDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- timeInterval@
timeInterval :: IsUNTimeIntervalNotificationTrigger unTimeIntervalNotificationTrigger => unTimeIntervalNotificationTrigger -> IO CDouble
timeInterval unTimeIntervalNotificationTrigger  =
  sendMsg unTimeIntervalNotificationTrigger (mkSelector "timeInterval") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triggerWithTimeInterval:repeats:@
triggerWithTimeInterval_repeatsSelector :: Selector
triggerWithTimeInterval_repeatsSelector = mkSelector "triggerWithTimeInterval:repeats:"

-- | @Selector@ for @nextTriggerDate@
nextTriggerDateSelector :: Selector
nextTriggerDateSelector = mkSelector "nextTriggerDate"

-- | @Selector@ for @timeInterval@
timeIntervalSelector :: Selector
timeIntervalSelector = mkSelector "timeInterval"

