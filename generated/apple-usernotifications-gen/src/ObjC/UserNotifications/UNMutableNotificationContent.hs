{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNMutableNotificationContent@.
module ObjC.UserNotifications.UNMutableNotificationContent
  ( UNMutableNotificationContent
  , IsUNMutableNotificationContent(..)
  , attachments
  , setAttachments
  , badge
  , setBadge
  , body
  , setBody
  , categoryIdentifier
  , setCategoryIdentifier
  , launchImageName
  , setLaunchImageName
  , sound
  , setSound
  , subtitle
  , setSubtitle
  , threadIdentifier
  , setThreadIdentifier
  , title
  , setTitle
  , userInfo
  , setUserInfo
  , summaryArgument
  , setSummaryArgument
  , summaryArgumentCount
  , setSummaryArgumentCount
  , targetContentIdentifier
  , setTargetContentIdentifier
  , interruptionLevel
  , setInterruptionLevel
  , relevanceScore
  , setRelevanceScore
  , filterCriteria
  , setFilterCriteria
  , attachmentsSelector
  , setAttachmentsSelector
  , badgeSelector
  , setBadgeSelector
  , bodySelector
  , setBodySelector
  , categoryIdentifierSelector
  , setCategoryIdentifierSelector
  , launchImageNameSelector
  , setLaunchImageNameSelector
  , soundSelector
  , setSoundSelector
  , subtitleSelector
  , setSubtitleSelector
  , threadIdentifierSelector
  , setThreadIdentifierSelector
  , titleSelector
  , setTitleSelector
  , userInfoSelector
  , setUserInfoSelector
  , summaryArgumentSelector
  , setSummaryArgumentSelector
  , summaryArgumentCountSelector
  , setSummaryArgumentCountSelector
  , targetContentIdentifierSelector
  , setTargetContentIdentifierSelector
  , interruptionLevelSelector
  , setInterruptionLevelSelector
  , relevanceScoreSelector
  , setRelevanceScoreSelector
  , filterCriteriaSelector
  , setFilterCriteriaSelector

  -- * Enum types
  , UNNotificationInterruptionLevel(UNNotificationInterruptionLevel)
  , pattern UNNotificationInterruptionLevelPassive
  , pattern UNNotificationInterruptionLevelActive
  , pattern UNNotificationInterruptionLevelTimeSensitive
  , pattern UNNotificationInterruptionLevelCritical

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
import ObjC.UserNotifications.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- attachments@
attachments :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSArray)
attachments unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "attachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttachments:@
setAttachments :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSArray value) => unMutableNotificationContent -> value -> IO ()
setAttachments unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setAttachments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- badge@
badge :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSNumber)
badge unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "badge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBadge:@
setBadge :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSNumber value) => unMutableNotificationContent -> value -> IO ()
setBadge unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setBadge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- body@
body :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
body unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "body") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBody:@
setBody :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setBody unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setBody:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- categoryIdentifier@
categoryIdentifier :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
categoryIdentifier unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "categoryIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCategoryIdentifier:@
setCategoryIdentifier :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setCategoryIdentifier unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setCategoryIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- launchImageName@
launchImageName :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
launchImageName unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "launchImageName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLaunchImageName:@
setLaunchImageName :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setLaunchImageName unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setLaunchImageName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sound@
sound :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id UNNotificationSound)
sound unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "sound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSound:@
setSound :: (IsUNMutableNotificationContent unMutableNotificationContent, IsUNNotificationSound value) => unMutableNotificationContent -> value -> IO ()
setSound unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setSound:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subtitle@
subtitle :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
subtitle unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubtitle:@
setSubtitle :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setSubtitle unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- threadIdentifier@
threadIdentifier :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
threadIdentifier unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "threadIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setThreadIdentifier:@
setThreadIdentifier :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setThreadIdentifier unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setThreadIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- title@
title :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
title unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setTitle unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userInfo@
userInfo :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSDictionary)
userInfo unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserInfo:@
setUserInfo :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSDictionary value) => unMutableNotificationContent -> value -> IO ()
setUserInfo unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setUserInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The argument to be inserted in the summary for this notification.
--
-- ObjC selector: @- summaryArgument@
summaryArgument :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
summaryArgument unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "summaryArgument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The argument to be inserted in the summary for this notification.
--
-- ObjC selector: @- setSummaryArgument:@
setSummaryArgument :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setSummaryArgument unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setSummaryArgument:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A number that indicates how many items in the summary are represented in the summary. For example if a podcast app sends one notification for 3 new episodes in a show, the argument should be the name of the show and the count should be 3. Default is 1 and cannot be 0.
--
-- ObjC selector: @- summaryArgumentCount@
summaryArgumentCount :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO CULong
summaryArgumentCount unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "summaryArgumentCount") retCULong []

-- | A number that indicates how many items in the summary are represented in the summary. For example if a podcast app sends one notification for 3 new episodes in a show, the argument should be the name of the show and the count should be 3. Default is 1 and cannot be 0.
--
-- ObjC selector: @- setSummaryArgumentCount:@
setSummaryArgumentCount :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> CULong -> IO ()
setSummaryArgumentCount unMutableNotificationContent  value =
    sendMsg unMutableNotificationContent (mkSelector "setSummaryArgumentCount:") retVoid [argCULong value]

-- | @- targetContentIdentifier@
targetContentIdentifier :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
targetContentIdentifier unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "targetContentIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetContentIdentifier:@
setTargetContentIdentifier :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setTargetContentIdentifier unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setTargetContentIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- interruptionLevel@
interruptionLevel :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO UNNotificationInterruptionLevel
interruptionLevel unMutableNotificationContent  =
    fmap (coerce :: CULong -> UNNotificationInterruptionLevel) $ sendMsg unMutableNotificationContent (mkSelector "interruptionLevel") retCULong []

-- | @- setInterruptionLevel:@
setInterruptionLevel :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> UNNotificationInterruptionLevel -> IO ()
setInterruptionLevel unMutableNotificationContent  value =
    sendMsg unMutableNotificationContent (mkSelector "setInterruptionLevel:") retVoid [argCULong (coerce value)]

-- | @- relevanceScore@
relevanceScore :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO CDouble
relevanceScore unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "relevanceScore") retCDouble []

-- | @- setRelevanceScore:@
setRelevanceScore :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> CDouble -> IO ()
setRelevanceScore unMutableNotificationContent  value =
    sendMsg unMutableNotificationContent (mkSelector "setRelevanceScore:") retVoid [argCDouble value]

-- | @- filterCriteria@
filterCriteria :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
filterCriteria unMutableNotificationContent  =
    sendMsg unMutableNotificationContent (mkSelector "filterCriteria") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFilterCriteria:@
setFilterCriteria :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setFilterCriteria unMutableNotificationContent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg unMutableNotificationContent (mkSelector "setFilterCriteria:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attachments@
attachmentsSelector :: Selector
attachmentsSelector = mkSelector "attachments"

-- | @Selector@ for @setAttachments:@
setAttachmentsSelector :: Selector
setAttachmentsSelector = mkSelector "setAttachments:"

-- | @Selector@ for @badge@
badgeSelector :: Selector
badgeSelector = mkSelector "badge"

-- | @Selector@ for @setBadge:@
setBadgeSelector :: Selector
setBadgeSelector = mkSelector "setBadge:"

-- | @Selector@ for @body@
bodySelector :: Selector
bodySelector = mkSelector "body"

-- | @Selector@ for @setBody:@
setBodySelector :: Selector
setBodySelector = mkSelector "setBody:"

-- | @Selector@ for @categoryIdentifier@
categoryIdentifierSelector :: Selector
categoryIdentifierSelector = mkSelector "categoryIdentifier"

-- | @Selector@ for @setCategoryIdentifier:@
setCategoryIdentifierSelector :: Selector
setCategoryIdentifierSelector = mkSelector "setCategoryIdentifier:"

-- | @Selector@ for @launchImageName@
launchImageNameSelector :: Selector
launchImageNameSelector = mkSelector "launchImageName"

-- | @Selector@ for @setLaunchImageName:@
setLaunchImageNameSelector :: Selector
setLaunchImageNameSelector = mkSelector "setLaunchImageName:"

-- | @Selector@ for @sound@
soundSelector :: Selector
soundSelector = mkSelector "sound"

-- | @Selector@ for @setSound:@
setSoundSelector :: Selector
setSoundSelector = mkSelector "setSound:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @threadIdentifier@
threadIdentifierSelector :: Selector
threadIdentifierSelector = mkSelector "threadIdentifier"

-- | @Selector@ for @setThreadIdentifier:@
setThreadIdentifierSelector :: Selector
setThreadIdentifierSelector = mkSelector "setThreadIdentifier:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @summaryArgument@
summaryArgumentSelector :: Selector
summaryArgumentSelector = mkSelector "summaryArgument"

-- | @Selector@ for @setSummaryArgument:@
setSummaryArgumentSelector :: Selector
setSummaryArgumentSelector = mkSelector "setSummaryArgument:"

-- | @Selector@ for @summaryArgumentCount@
summaryArgumentCountSelector :: Selector
summaryArgumentCountSelector = mkSelector "summaryArgumentCount"

-- | @Selector@ for @setSummaryArgumentCount:@
setSummaryArgumentCountSelector :: Selector
setSummaryArgumentCountSelector = mkSelector "setSummaryArgumentCount:"

-- | @Selector@ for @targetContentIdentifier@
targetContentIdentifierSelector :: Selector
targetContentIdentifierSelector = mkSelector "targetContentIdentifier"

-- | @Selector@ for @setTargetContentIdentifier:@
setTargetContentIdentifierSelector :: Selector
setTargetContentIdentifierSelector = mkSelector "setTargetContentIdentifier:"

-- | @Selector@ for @interruptionLevel@
interruptionLevelSelector :: Selector
interruptionLevelSelector = mkSelector "interruptionLevel"

-- | @Selector@ for @setInterruptionLevel:@
setInterruptionLevelSelector :: Selector
setInterruptionLevelSelector = mkSelector "setInterruptionLevel:"

-- | @Selector@ for @relevanceScore@
relevanceScoreSelector :: Selector
relevanceScoreSelector = mkSelector "relevanceScore"

-- | @Selector@ for @setRelevanceScore:@
setRelevanceScoreSelector :: Selector
setRelevanceScoreSelector = mkSelector "setRelevanceScore:"

-- | @Selector@ for @filterCriteria@
filterCriteriaSelector :: Selector
filterCriteriaSelector = mkSelector "filterCriteria"

-- | @Selector@ for @setFilterCriteria:@
setFilterCriteriaSelector :: Selector
setFilterCriteriaSelector = mkSelector "setFilterCriteria:"

