{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalAlarm@.
module ObjC.CalendarStore.CalAlarm
  ( CalAlarm
  , IsCalAlarm(..)
  , alarm
  , setAcknowledged
  , acknowledged
  , setRelatedTo
  , relatedTo
  , triggerDateRelativeTo
  , action
  , setAction
  , sound
  , setSound
  , emailAddress
  , setEmailAddress
  , url
  , setUrl
  , relativeTrigger
  , setRelativeTrigger
  , absoluteTrigger
  , setAbsoluteTrigger
  , absoluteTriggerSelector
  , acknowledgedSelector
  , actionSelector
  , alarmSelector
  , emailAddressSelector
  , relatedToSelector
  , relativeTriggerSelector
  , setAbsoluteTriggerSelector
  , setAcknowledgedSelector
  , setActionSelector
  , setEmailAddressSelector
  , setRelatedToSelector
  , setRelativeTriggerSelector
  , setSoundSelector
  , setUrlSelector
  , soundSelector
  , triggerDateRelativeToSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ alarm@
alarm :: IO RawId
alarm  =
  do
    cls' <- getRequiredClass "CalAlarm"
    sendClassMessage cls' alarmSelector

-- | @- setAcknowledged:@
setAcknowledged :: (IsCalAlarm calAlarm, IsNSDate date) => calAlarm -> date -> IO ()
setAcknowledged calAlarm date =
  sendMessage calAlarm setAcknowledgedSelector (toNSDate date)

-- | @- acknowledged@
acknowledged :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSDate)
acknowledged calAlarm =
  sendMessage calAlarm acknowledgedSelector

-- | @- setRelatedTo:@
setRelatedTo :: (IsCalAlarm calAlarm, IsNSString relatedTo) => calAlarm -> relatedTo -> IO ()
setRelatedTo calAlarm relatedTo =
  sendMessage calAlarm setRelatedToSelector (toNSString relatedTo)

-- | @- relatedTo@
relatedTo :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSString)
relatedTo calAlarm =
  sendMessage calAlarm relatedToSelector

-- | @- triggerDateRelativeTo:@
triggerDateRelativeTo :: (IsCalAlarm calAlarm, IsNSDate date) => calAlarm -> date -> IO (Id NSDate)
triggerDateRelativeTo calAlarm date =
  sendMessage calAlarm triggerDateRelativeToSelector (toNSDate date)

-- | @- action@
action :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSString)
action calAlarm =
  sendMessage calAlarm actionSelector

-- | @- setAction:@
setAction :: (IsCalAlarm calAlarm, IsNSString value) => calAlarm -> value -> IO ()
setAction calAlarm value =
  sendMessage calAlarm setActionSelector (toNSString value)

-- | @- sound@
sound :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSString)
sound calAlarm =
  sendMessage calAlarm soundSelector

-- | @- setSound:@
setSound :: (IsCalAlarm calAlarm, IsNSString value) => calAlarm -> value -> IO ()
setSound calAlarm value =
  sendMessage calAlarm setSoundSelector (toNSString value)

-- | @- emailAddress@
emailAddress :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSString)
emailAddress calAlarm =
  sendMessage calAlarm emailAddressSelector

-- | @- setEmailAddress:@
setEmailAddress :: (IsCalAlarm calAlarm, IsNSString value) => calAlarm -> value -> IO ()
setEmailAddress calAlarm value =
  sendMessage calAlarm setEmailAddressSelector (toNSString value)

-- | @- url@
url :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSURL)
url calAlarm =
  sendMessage calAlarm urlSelector

-- | @- setUrl:@
setUrl :: (IsCalAlarm calAlarm, IsNSURL value) => calAlarm -> value -> IO ()
setUrl calAlarm value =
  sendMessage calAlarm setUrlSelector (toNSURL value)

-- | @- relativeTrigger@
relativeTrigger :: IsCalAlarm calAlarm => calAlarm -> IO CDouble
relativeTrigger calAlarm =
  sendMessage calAlarm relativeTriggerSelector

-- | @- setRelativeTrigger:@
setRelativeTrigger :: IsCalAlarm calAlarm => calAlarm -> CDouble -> IO ()
setRelativeTrigger calAlarm value =
  sendMessage calAlarm setRelativeTriggerSelector value

-- | @- absoluteTrigger@
absoluteTrigger :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSDate)
absoluteTrigger calAlarm =
  sendMessage calAlarm absoluteTriggerSelector

-- | @- setAbsoluteTrigger:@
setAbsoluteTrigger :: (IsCalAlarm calAlarm, IsNSDate value) => calAlarm -> value -> IO ()
setAbsoluteTrigger calAlarm value =
  sendMessage calAlarm setAbsoluteTriggerSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarm@
alarmSelector :: Selector '[] RawId
alarmSelector = mkSelector "alarm"

-- | @Selector@ for @setAcknowledged:@
setAcknowledgedSelector :: Selector '[Id NSDate] ()
setAcknowledgedSelector = mkSelector "setAcknowledged:"

-- | @Selector@ for @acknowledged@
acknowledgedSelector :: Selector '[] (Id NSDate)
acknowledgedSelector = mkSelector "acknowledged"

-- | @Selector@ for @setRelatedTo:@
setRelatedToSelector :: Selector '[Id NSString] ()
setRelatedToSelector = mkSelector "setRelatedTo:"

-- | @Selector@ for @relatedTo@
relatedToSelector :: Selector '[] (Id NSString)
relatedToSelector = mkSelector "relatedTo"

-- | @Selector@ for @triggerDateRelativeTo:@
triggerDateRelativeToSelector :: Selector '[Id NSDate] (Id NSDate)
triggerDateRelativeToSelector = mkSelector "triggerDateRelativeTo:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] (Id NSString)
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Id NSString] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @sound@
soundSelector :: Selector '[] (Id NSString)
soundSelector = mkSelector "sound"

-- | @Selector@ for @setSound:@
setSoundSelector :: Selector '[Id NSString] ()
setSoundSelector = mkSelector "setSound:"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector '[] (Id NSString)
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @setEmailAddress:@
setEmailAddressSelector :: Selector '[Id NSString] ()
setEmailAddressSelector = mkSelector "setEmailAddress:"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector '[Id NSURL] ()
setUrlSelector = mkSelector "setUrl:"

-- | @Selector@ for @relativeTrigger@
relativeTriggerSelector :: Selector '[] CDouble
relativeTriggerSelector = mkSelector "relativeTrigger"

-- | @Selector@ for @setRelativeTrigger:@
setRelativeTriggerSelector :: Selector '[CDouble] ()
setRelativeTriggerSelector = mkSelector "setRelativeTrigger:"

-- | @Selector@ for @absoluteTrigger@
absoluteTriggerSelector :: Selector '[] (Id NSDate)
absoluteTriggerSelector = mkSelector "absoluteTrigger"

-- | @Selector@ for @setAbsoluteTrigger:@
setAbsoluteTriggerSelector :: Selector '[Id NSDate] ()
setAbsoluteTriggerSelector = mkSelector "setAbsoluteTrigger:"

