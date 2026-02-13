{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , badgeSelector
  , bodySelector
  , categoryIdentifierSelector
  , filterCriteriaSelector
  , interruptionLevelSelector
  , launchImageNameSelector
  , relevanceScoreSelector
  , setAttachmentsSelector
  , setBadgeSelector
  , setBodySelector
  , setCategoryIdentifierSelector
  , setFilterCriteriaSelector
  , setInterruptionLevelSelector
  , setLaunchImageNameSelector
  , setRelevanceScoreSelector
  , setSoundSelector
  , setSubtitleSelector
  , setSummaryArgumentCountSelector
  , setSummaryArgumentSelector
  , setTargetContentIdentifierSelector
  , setThreadIdentifierSelector
  , setTitleSelector
  , setUserInfoSelector
  , soundSelector
  , subtitleSelector
  , summaryArgumentCountSelector
  , summaryArgumentSelector
  , targetContentIdentifierSelector
  , threadIdentifierSelector
  , titleSelector
  , userInfoSelector

  -- * Enum types
  , UNNotificationInterruptionLevel(UNNotificationInterruptionLevel)
  , pattern UNNotificationInterruptionLevelPassive
  , pattern UNNotificationInterruptionLevelActive
  , pattern UNNotificationInterruptionLevelTimeSensitive
  , pattern UNNotificationInterruptionLevelCritical

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.UserNotifications.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- attachments@
attachments :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSArray)
attachments unMutableNotificationContent =
  sendMessage unMutableNotificationContent attachmentsSelector

-- | @- setAttachments:@
setAttachments :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSArray value) => unMutableNotificationContent -> value -> IO ()
setAttachments unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setAttachmentsSelector (toNSArray value)

-- | @- badge@
badge :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSNumber)
badge unMutableNotificationContent =
  sendMessage unMutableNotificationContent badgeSelector

-- | @- setBadge:@
setBadge :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSNumber value) => unMutableNotificationContent -> value -> IO ()
setBadge unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setBadgeSelector (toNSNumber value)

-- | @- body@
body :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
body unMutableNotificationContent =
  sendMessage unMutableNotificationContent bodySelector

-- | @- setBody:@
setBody :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setBody unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setBodySelector (toNSString value)

-- | @- categoryIdentifier@
categoryIdentifier :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
categoryIdentifier unMutableNotificationContent =
  sendMessage unMutableNotificationContent categoryIdentifierSelector

-- | @- setCategoryIdentifier:@
setCategoryIdentifier :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setCategoryIdentifier unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setCategoryIdentifierSelector (toNSString value)

-- | @- launchImageName@
launchImageName :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
launchImageName unMutableNotificationContent =
  sendMessage unMutableNotificationContent launchImageNameSelector

-- | @- setLaunchImageName:@
setLaunchImageName :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setLaunchImageName unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setLaunchImageNameSelector (toNSString value)

-- | @- sound@
sound :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id UNNotificationSound)
sound unMutableNotificationContent =
  sendMessage unMutableNotificationContent soundSelector

-- | @- setSound:@
setSound :: (IsUNMutableNotificationContent unMutableNotificationContent, IsUNNotificationSound value) => unMutableNotificationContent -> value -> IO ()
setSound unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setSoundSelector (toUNNotificationSound value)

-- | @- subtitle@
subtitle :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
subtitle unMutableNotificationContent =
  sendMessage unMutableNotificationContent subtitleSelector

-- | @- setSubtitle:@
setSubtitle :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setSubtitle unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setSubtitleSelector (toNSString value)

-- | @- threadIdentifier@
threadIdentifier :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
threadIdentifier unMutableNotificationContent =
  sendMessage unMutableNotificationContent threadIdentifierSelector

-- | @- setThreadIdentifier:@
setThreadIdentifier :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setThreadIdentifier unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setThreadIdentifierSelector (toNSString value)

-- | @- title@
title :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
title unMutableNotificationContent =
  sendMessage unMutableNotificationContent titleSelector

-- | @- setTitle:@
setTitle :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setTitle unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setTitleSelector (toNSString value)

-- | @- userInfo@
userInfo :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSDictionary)
userInfo unMutableNotificationContent =
  sendMessage unMutableNotificationContent userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSDictionary value) => unMutableNotificationContent -> value -> IO ()
setUserInfo unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setUserInfoSelector (toNSDictionary value)

-- | The argument to be inserted in the summary for this notification.
--
-- ObjC selector: @- summaryArgument@
summaryArgument :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
summaryArgument unMutableNotificationContent =
  sendMessage unMutableNotificationContent summaryArgumentSelector

-- | The argument to be inserted in the summary for this notification.
--
-- ObjC selector: @- setSummaryArgument:@
setSummaryArgument :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setSummaryArgument unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setSummaryArgumentSelector (toNSString value)

-- | A number that indicates how many items in the summary are represented in the summary. For example if a podcast app sends one notification for 3 new episodes in a show, the argument should be the name of the show and the count should be 3. Default is 1 and cannot be 0.
--
-- ObjC selector: @- summaryArgumentCount@
summaryArgumentCount :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO CULong
summaryArgumentCount unMutableNotificationContent =
  sendMessage unMutableNotificationContent summaryArgumentCountSelector

-- | A number that indicates how many items in the summary are represented in the summary. For example if a podcast app sends one notification for 3 new episodes in a show, the argument should be the name of the show and the count should be 3. Default is 1 and cannot be 0.
--
-- ObjC selector: @- setSummaryArgumentCount:@
setSummaryArgumentCount :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> CULong -> IO ()
setSummaryArgumentCount unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setSummaryArgumentCountSelector value

-- | @- targetContentIdentifier@
targetContentIdentifier :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
targetContentIdentifier unMutableNotificationContent =
  sendMessage unMutableNotificationContent targetContentIdentifierSelector

-- | @- setTargetContentIdentifier:@
setTargetContentIdentifier :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setTargetContentIdentifier unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setTargetContentIdentifierSelector (toNSString value)

-- | @- interruptionLevel@
interruptionLevel :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO UNNotificationInterruptionLevel
interruptionLevel unMutableNotificationContent =
  sendMessage unMutableNotificationContent interruptionLevelSelector

-- | @- setInterruptionLevel:@
setInterruptionLevel :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> UNNotificationInterruptionLevel -> IO ()
setInterruptionLevel unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setInterruptionLevelSelector value

-- | @- relevanceScore@
relevanceScore :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO CDouble
relevanceScore unMutableNotificationContent =
  sendMessage unMutableNotificationContent relevanceScoreSelector

-- | @- setRelevanceScore:@
setRelevanceScore :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> CDouble -> IO ()
setRelevanceScore unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setRelevanceScoreSelector value

-- | @- filterCriteria@
filterCriteria :: IsUNMutableNotificationContent unMutableNotificationContent => unMutableNotificationContent -> IO (Id NSString)
filterCriteria unMutableNotificationContent =
  sendMessage unMutableNotificationContent filterCriteriaSelector

-- | @- setFilterCriteria:@
setFilterCriteria :: (IsUNMutableNotificationContent unMutableNotificationContent, IsNSString value) => unMutableNotificationContent -> value -> IO ()
setFilterCriteria unMutableNotificationContent value =
  sendMessage unMutableNotificationContent setFilterCriteriaSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attachments@
attachmentsSelector :: Selector '[] (Id NSArray)
attachmentsSelector = mkSelector "attachments"

-- | @Selector@ for @setAttachments:@
setAttachmentsSelector :: Selector '[Id NSArray] ()
setAttachmentsSelector = mkSelector "setAttachments:"

-- | @Selector@ for @badge@
badgeSelector :: Selector '[] (Id NSNumber)
badgeSelector = mkSelector "badge"

-- | @Selector@ for @setBadge:@
setBadgeSelector :: Selector '[Id NSNumber] ()
setBadgeSelector = mkSelector "setBadge:"

-- | @Selector@ for @body@
bodySelector :: Selector '[] (Id NSString)
bodySelector = mkSelector "body"

-- | @Selector@ for @setBody:@
setBodySelector :: Selector '[Id NSString] ()
setBodySelector = mkSelector "setBody:"

-- | @Selector@ for @categoryIdentifier@
categoryIdentifierSelector :: Selector '[] (Id NSString)
categoryIdentifierSelector = mkSelector "categoryIdentifier"

-- | @Selector@ for @setCategoryIdentifier:@
setCategoryIdentifierSelector :: Selector '[Id NSString] ()
setCategoryIdentifierSelector = mkSelector "setCategoryIdentifier:"

-- | @Selector@ for @launchImageName@
launchImageNameSelector :: Selector '[] (Id NSString)
launchImageNameSelector = mkSelector "launchImageName"

-- | @Selector@ for @setLaunchImageName:@
setLaunchImageNameSelector :: Selector '[Id NSString] ()
setLaunchImageNameSelector = mkSelector "setLaunchImageName:"

-- | @Selector@ for @sound@
soundSelector :: Selector '[] (Id UNNotificationSound)
soundSelector = mkSelector "sound"

-- | @Selector@ for @setSound:@
setSoundSelector :: Selector '[Id UNNotificationSound] ()
setSoundSelector = mkSelector "setSound:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @threadIdentifier@
threadIdentifierSelector :: Selector '[] (Id NSString)
threadIdentifierSelector = mkSelector "threadIdentifier"

-- | @Selector@ for @setThreadIdentifier:@
setThreadIdentifierSelector :: Selector '[Id NSString] ()
setThreadIdentifierSelector = mkSelector "setThreadIdentifier:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @summaryArgument@
summaryArgumentSelector :: Selector '[] (Id NSString)
summaryArgumentSelector = mkSelector "summaryArgument"

-- | @Selector@ for @setSummaryArgument:@
setSummaryArgumentSelector :: Selector '[Id NSString] ()
setSummaryArgumentSelector = mkSelector "setSummaryArgument:"

-- | @Selector@ for @summaryArgumentCount@
summaryArgumentCountSelector :: Selector '[] CULong
summaryArgumentCountSelector = mkSelector "summaryArgumentCount"

-- | @Selector@ for @setSummaryArgumentCount:@
setSummaryArgumentCountSelector :: Selector '[CULong] ()
setSummaryArgumentCountSelector = mkSelector "setSummaryArgumentCount:"

-- | @Selector@ for @targetContentIdentifier@
targetContentIdentifierSelector :: Selector '[] (Id NSString)
targetContentIdentifierSelector = mkSelector "targetContentIdentifier"

-- | @Selector@ for @setTargetContentIdentifier:@
setTargetContentIdentifierSelector :: Selector '[Id NSString] ()
setTargetContentIdentifierSelector = mkSelector "setTargetContentIdentifier:"

-- | @Selector@ for @interruptionLevel@
interruptionLevelSelector :: Selector '[] UNNotificationInterruptionLevel
interruptionLevelSelector = mkSelector "interruptionLevel"

-- | @Selector@ for @setInterruptionLevel:@
setInterruptionLevelSelector :: Selector '[UNNotificationInterruptionLevel] ()
setInterruptionLevelSelector = mkSelector "setInterruptionLevel:"

-- | @Selector@ for @relevanceScore@
relevanceScoreSelector :: Selector '[] CDouble
relevanceScoreSelector = mkSelector "relevanceScore"

-- | @Selector@ for @setRelevanceScore:@
setRelevanceScoreSelector :: Selector '[CDouble] ()
setRelevanceScoreSelector = mkSelector "setRelevanceScore:"

-- | @Selector@ for @filterCriteria@
filterCriteriaSelector :: Selector '[] (Id NSString)
filterCriteriaSelector = mkSelector "filterCriteria"

-- | @Selector@ for @setFilterCriteria:@
setFilterCriteriaSelector :: Selector '[Id NSString] ()
setFilterCriteriaSelector = mkSelector "setFilterCriteria:"

