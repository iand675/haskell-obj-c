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
  , alarmSelector
  , setAcknowledgedSelector
  , acknowledgedSelector
  , setRelatedToSelector
  , relatedToSelector
  , triggerDateRelativeToSelector
  , actionSelector
  , setActionSelector
  , soundSelector
  , setSoundSelector
  , emailAddressSelector
  , setEmailAddressSelector
  , urlSelector
  , setUrlSelector
  , relativeTriggerSelector
  , setRelativeTriggerSelector
  , absoluteTriggerSelector
  , setAbsoluteTriggerSelector


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

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ alarm@
alarm :: IO RawId
alarm  =
  do
    cls' <- getRequiredClass "CalAlarm"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "alarm") (retPtr retVoid) []

-- | @- setAcknowledged:@
setAcknowledged :: (IsCalAlarm calAlarm, IsNSDate date) => calAlarm -> date -> IO ()
setAcknowledged calAlarm  date =
withObjCPtr date $ \raw_date ->
    sendMsg calAlarm (mkSelector "setAcknowledged:") retVoid [argPtr (castPtr raw_date :: Ptr ())]

-- | @- acknowledged@
acknowledged :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSDate)
acknowledged calAlarm  =
  sendMsg calAlarm (mkSelector "acknowledged") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRelatedTo:@
setRelatedTo :: (IsCalAlarm calAlarm, IsNSString relatedTo) => calAlarm -> relatedTo -> IO ()
setRelatedTo calAlarm  relatedTo =
withObjCPtr relatedTo $ \raw_relatedTo ->
    sendMsg calAlarm (mkSelector "setRelatedTo:") retVoid [argPtr (castPtr raw_relatedTo :: Ptr ())]

-- | @- relatedTo@
relatedTo :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSString)
relatedTo calAlarm  =
  sendMsg calAlarm (mkSelector "relatedTo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- triggerDateRelativeTo:@
triggerDateRelativeTo :: (IsCalAlarm calAlarm, IsNSDate date) => calAlarm -> date -> IO (Id NSDate)
triggerDateRelativeTo calAlarm  date =
withObjCPtr date $ \raw_date ->
    sendMsg calAlarm (mkSelector "triggerDateRelativeTo:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @- action@
action :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSString)
action calAlarm  =
  sendMsg calAlarm (mkSelector "action") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAction:@
setAction :: (IsCalAlarm calAlarm, IsNSString value) => calAlarm -> value -> IO ()
setAction calAlarm  value =
withObjCPtr value $ \raw_value ->
    sendMsg calAlarm (mkSelector "setAction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sound@
sound :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSString)
sound calAlarm  =
  sendMsg calAlarm (mkSelector "sound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSound:@
setSound :: (IsCalAlarm calAlarm, IsNSString value) => calAlarm -> value -> IO ()
setSound calAlarm  value =
withObjCPtr value $ \raw_value ->
    sendMsg calAlarm (mkSelector "setSound:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- emailAddress@
emailAddress :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSString)
emailAddress calAlarm  =
  sendMsg calAlarm (mkSelector "emailAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmailAddress:@
setEmailAddress :: (IsCalAlarm calAlarm, IsNSString value) => calAlarm -> value -> IO ()
setEmailAddress calAlarm  value =
withObjCPtr value $ \raw_value ->
    sendMsg calAlarm (mkSelector "setEmailAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- url@
url :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSURL)
url calAlarm  =
  sendMsg calAlarm (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUrl:@
setUrl :: (IsCalAlarm calAlarm, IsNSURL value) => calAlarm -> value -> IO ()
setUrl calAlarm  value =
withObjCPtr value $ \raw_value ->
    sendMsg calAlarm (mkSelector "setUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- relativeTrigger@
relativeTrigger :: IsCalAlarm calAlarm => calAlarm -> IO CDouble
relativeTrigger calAlarm  =
  sendMsg calAlarm (mkSelector "relativeTrigger") retCDouble []

-- | @- setRelativeTrigger:@
setRelativeTrigger :: IsCalAlarm calAlarm => calAlarm -> CDouble -> IO ()
setRelativeTrigger calAlarm  value =
  sendMsg calAlarm (mkSelector "setRelativeTrigger:") retVoid [argCDouble (fromIntegral value)]

-- | @- absoluteTrigger@
absoluteTrigger :: IsCalAlarm calAlarm => calAlarm -> IO (Id NSDate)
absoluteTrigger calAlarm  =
  sendMsg calAlarm (mkSelector "absoluteTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAbsoluteTrigger:@
setAbsoluteTrigger :: (IsCalAlarm calAlarm, IsNSDate value) => calAlarm -> value -> IO ()
setAbsoluteTrigger calAlarm  value =
withObjCPtr value $ \raw_value ->
    sendMsg calAlarm (mkSelector "setAbsoluteTrigger:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarm@
alarmSelector :: Selector
alarmSelector = mkSelector "alarm"

-- | @Selector@ for @setAcknowledged:@
setAcknowledgedSelector :: Selector
setAcknowledgedSelector = mkSelector "setAcknowledged:"

-- | @Selector@ for @acknowledged@
acknowledgedSelector :: Selector
acknowledgedSelector = mkSelector "acknowledged"

-- | @Selector@ for @setRelatedTo:@
setRelatedToSelector :: Selector
setRelatedToSelector = mkSelector "setRelatedTo:"

-- | @Selector@ for @relatedTo@
relatedToSelector :: Selector
relatedToSelector = mkSelector "relatedTo"

-- | @Selector@ for @triggerDateRelativeTo:@
triggerDateRelativeToSelector :: Selector
triggerDateRelativeToSelector = mkSelector "triggerDateRelativeTo:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @sound@
soundSelector :: Selector
soundSelector = mkSelector "sound"

-- | @Selector@ for @setSound:@
setSoundSelector :: Selector
setSoundSelector = mkSelector "setSound:"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @setEmailAddress:@
setEmailAddressSelector :: Selector
setEmailAddressSelector = mkSelector "setEmailAddress:"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector
setUrlSelector = mkSelector "setUrl:"

-- | @Selector@ for @relativeTrigger@
relativeTriggerSelector :: Selector
relativeTriggerSelector = mkSelector "relativeTrigger"

-- | @Selector@ for @setRelativeTrigger:@
setRelativeTriggerSelector :: Selector
setRelativeTriggerSelector = mkSelector "setRelativeTrigger:"

-- | @Selector@ for @absoluteTrigger@
absoluteTriggerSelector :: Selector
absoluteTriggerSelector = mkSelector "absoluteTrigger"

-- | @Selector@ for @setAbsoluteTrigger:@
setAbsoluteTriggerSelector :: Selector
setAbsoluteTriggerSelector = mkSelector "setAbsoluteTrigger:"

