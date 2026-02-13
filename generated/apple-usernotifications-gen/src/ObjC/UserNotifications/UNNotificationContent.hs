{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationContent@.
module ObjC.UserNotifications.UNNotificationContent
  ( UNNotificationContent
  , IsUNNotificationContent(..)
  , contentByUpdatingWithProvider_error
  , attachments
  , badge
  , body
  , categoryIdentifier
  , launchImageName
  , sound
  , subtitle
  , threadIdentifier
  , title
  , userInfo
  , summaryArgument
  , summaryArgumentCount
  , targetContentIdentifier
  , interruptionLevel
  , relevanceScore
  , filterCriteria
  , attachmentsSelector
  , badgeSelector
  , bodySelector
  , categoryIdentifierSelector
  , contentByUpdatingWithProvider_errorSelector
  , filterCriteriaSelector
  , interruptionLevelSelector
  , launchImageNameSelector
  , relevanceScoreSelector
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

-- | Contextualizes your UNNotificationContent object with other Apple SDK objects conforming to UNNotificationContentProviding. This will specialize the notification and decorate its look and behavior accordingly. For example, the notification will be treated as a message with an avatar and be promoted to the top of notification center if the object passed in is a valid INSendMessageIntent<UNNotificationContentProviding>. This throws an error with a UNErrorCode found in UNError.h if the UNNotificationContentProviding object is invalid. A valid UNNotificationContent result should not be mutated and be passed directly to UNUserNotificationCenter.
--
-- This should be called in the UNNotificationServiceExtension in didReceiveNotificationRequest:withContentHandler: and the returned UNNotificationContent should be passed to the contentHandler for incoming push notifications.
--
-- ObjC selector: @- contentByUpdatingWithProvider:error:@
contentByUpdatingWithProvider_error :: (IsUNNotificationContent unNotificationContent, IsNSError outError) => unNotificationContent -> RawId -> outError -> IO (Id UNNotificationContent)
contentByUpdatingWithProvider_error unNotificationContent provider outError =
  sendMessage unNotificationContent contentByUpdatingWithProvider_errorSelector provider (toNSError outError)

-- | @- attachments@
attachments :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSArray)
attachments unNotificationContent =
  sendMessage unNotificationContent attachmentsSelector

-- | @- badge@
badge :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSNumber)
badge unNotificationContent =
  sendMessage unNotificationContent badgeSelector

-- | @- body@
body :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
body unNotificationContent =
  sendMessage unNotificationContent bodySelector

-- | @- categoryIdentifier@
categoryIdentifier :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
categoryIdentifier unNotificationContent =
  sendMessage unNotificationContent categoryIdentifierSelector

-- | @- launchImageName@
launchImageName :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
launchImageName unNotificationContent =
  sendMessage unNotificationContent launchImageNameSelector

-- | @- sound@
sound :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id UNNotificationSound)
sound unNotificationContent =
  sendMessage unNotificationContent soundSelector

-- | @- subtitle@
subtitle :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
subtitle unNotificationContent =
  sendMessage unNotificationContent subtitleSelector

-- | @- threadIdentifier@
threadIdentifier :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
threadIdentifier unNotificationContent =
  sendMessage unNotificationContent threadIdentifierSelector

-- | @- title@
title :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
title unNotificationContent =
  sendMessage unNotificationContent titleSelector

-- | @- userInfo@
userInfo :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSDictionary)
userInfo unNotificationContent =
  sendMessage unNotificationContent userInfoSelector

-- | The argument to be inserted in the summary for this notification.
--
-- ObjC selector: @- summaryArgument@
summaryArgument :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
summaryArgument unNotificationContent =
  sendMessage unNotificationContent summaryArgumentSelector

-- | A number that indicates how many items in the summary are represented in the summary. For example if a podcast app sends one notification for 3 new episodes in a show, the argument should be the name of the show and the count should be 3. Default is 1 and cannot be 0.
--
-- ObjC selector: @- summaryArgumentCount@
summaryArgumentCount :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO CULong
summaryArgumentCount unNotificationContent =
  sendMessage unNotificationContent summaryArgumentCountSelector

-- | @- targetContentIdentifier@
targetContentIdentifier :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
targetContentIdentifier unNotificationContent =
  sendMessage unNotificationContent targetContentIdentifierSelector

-- | @- interruptionLevel@
interruptionLevel :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO UNNotificationInterruptionLevel
interruptionLevel unNotificationContent =
  sendMessage unNotificationContent interruptionLevelSelector

-- | @- relevanceScore@
relevanceScore :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO CDouble
relevanceScore unNotificationContent =
  sendMessage unNotificationContent relevanceScoreSelector

-- | @- filterCriteria@
filterCriteria :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
filterCriteria unNotificationContent =
  sendMessage unNotificationContent filterCriteriaSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentByUpdatingWithProvider:error:@
contentByUpdatingWithProvider_errorSelector :: Selector '[RawId, Id NSError] (Id UNNotificationContent)
contentByUpdatingWithProvider_errorSelector = mkSelector "contentByUpdatingWithProvider:error:"

-- | @Selector@ for @attachments@
attachmentsSelector :: Selector '[] (Id NSArray)
attachmentsSelector = mkSelector "attachments"

-- | @Selector@ for @badge@
badgeSelector :: Selector '[] (Id NSNumber)
badgeSelector = mkSelector "badge"

-- | @Selector@ for @body@
bodySelector :: Selector '[] (Id NSString)
bodySelector = mkSelector "body"

-- | @Selector@ for @categoryIdentifier@
categoryIdentifierSelector :: Selector '[] (Id NSString)
categoryIdentifierSelector = mkSelector "categoryIdentifier"

-- | @Selector@ for @launchImageName@
launchImageNameSelector :: Selector '[] (Id NSString)
launchImageNameSelector = mkSelector "launchImageName"

-- | @Selector@ for @sound@
soundSelector :: Selector '[] (Id UNNotificationSound)
soundSelector = mkSelector "sound"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @threadIdentifier@
threadIdentifierSelector :: Selector '[] (Id NSString)
threadIdentifierSelector = mkSelector "threadIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @summaryArgument@
summaryArgumentSelector :: Selector '[] (Id NSString)
summaryArgumentSelector = mkSelector "summaryArgument"

-- | @Selector@ for @summaryArgumentCount@
summaryArgumentCountSelector :: Selector '[] CULong
summaryArgumentCountSelector = mkSelector "summaryArgumentCount"

-- | @Selector@ for @targetContentIdentifier@
targetContentIdentifierSelector :: Selector '[] (Id NSString)
targetContentIdentifierSelector = mkSelector "targetContentIdentifier"

-- | @Selector@ for @interruptionLevel@
interruptionLevelSelector :: Selector '[] UNNotificationInterruptionLevel
interruptionLevelSelector = mkSelector "interruptionLevel"

-- | @Selector@ for @relevanceScore@
relevanceScoreSelector :: Selector '[] CDouble
relevanceScoreSelector = mkSelector "relevanceScore"

-- | @Selector@ for @filterCriteria@
filterCriteriaSelector :: Selector '[] (Id NSString)
filterCriteriaSelector = mkSelector "filterCriteria"

