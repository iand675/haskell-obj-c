{-# LANGUAGE PatternSynonyms #-}
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
  , contentByUpdatingWithProvider_errorSelector
  , attachmentsSelector
  , badgeSelector
  , bodySelector
  , categoryIdentifierSelector
  , launchImageNameSelector
  , soundSelector
  , subtitleSelector
  , threadIdentifierSelector
  , titleSelector
  , userInfoSelector
  , summaryArgumentSelector
  , summaryArgumentCountSelector
  , targetContentIdentifierSelector
  , interruptionLevelSelector
  , relevanceScoreSelector
  , filterCriteriaSelector

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

-- | Contextualizes your UNNotificationContent object with other Apple SDK objects conforming to UNNotificationContentProviding. This will specialize the notification and decorate its look and behavior accordingly. For example, the notification will be treated as a message with an avatar and be promoted to the top of notification center if the object passed in is a valid INSendMessageIntent<UNNotificationContentProviding>. This throws an error with a UNErrorCode found in UNError.h if the UNNotificationContentProviding object is invalid. A valid UNNotificationContent result should not be mutated and be passed directly to UNUserNotificationCenter.
--
-- This should be called in the UNNotificationServiceExtension in didReceiveNotificationRequest:withContentHandler: and the returned UNNotificationContent should be passed to the contentHandler for incoming push notifications.
--
-- ObjC selector: @- contentByUpdatingWithProvider:error:@
contentByUpdatingWithProvider_error :: (IsUNNotificationContent unNotificationContent, IsNSError outError) => unNotificationContent -> RawId -> outError -> IO (Id UNNotificationContent)
contentByUpdatingWithProvider_error unNotificationContent  provider outError =
  withObjCPtr outError $ \raw_outError ->
      sendMsg unNotificationContent (mkSelector "contentByUpdatingWithProvider:error:") (retPtr retVoid) [argPtr (castPtr (unRawId provider) :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- attachments@
attachments :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSArray)
attachments unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "attachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- badge@
badge :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSNumber)
badge unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "badge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- body@
body :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
body unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "body") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- categoryIdentifier@
categoryIdentifier :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
categoryIdentifier unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "categoryIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- launchImageName@
launchImageName :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
launchImageName unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "launchImageName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sound@
sound :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id UNNotificationSound)
sound unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "sound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subtitle@
subtitle :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
subtitle unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- threadIdentifier@
threadIdentifier :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
threadIdentifier unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "threadIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
title unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userInfo@
userInfo :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSDictionary)
userInfo unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The argument to be inserted in the summary for this notification.
--
-- ObjC selector: @- summaryArgument@
summaryArgument :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
summaryArgument unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "summaryArgument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A number that indicates how many items in the summary are represented in the summary. For example if a podcast app sends one notification for 3 new episodes in a show, the argument should be the name of the show and the count should be 3. Default is 1 and cannot be 0.
--
-- ObjC selector: @- summaryArgumentCount@
summaryArgumentCount :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO CULong
summaryArgumentCount unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "summaryArgumentCount") retCULong []

-- | @- targetContentIdentifier@
targetContentIdentifier :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
targetContentIdentifier unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "targetContentIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- interruptionLevel@
interruptionLevel :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO UNNotificationInterruptionLevel
interruptionLevel unNotificationContent  =
    fmap (coerce :: CULong -> UNNotificationInterruptionLevel) $ sendMsg unNotificationContent (mkSelector "interruptionLevel") retCULong []

-- | @- relevanceScore@
relevanceScore :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO CDouble
relevanceScore unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "relevanceScore") retCDouble []

-- | @- filterCriteria@
filterCriteria :: IsUNNotificationContent unNotificationContent => unNotificationContent -> IO (Id NSString)
filterCriteria unNotificationContent  =
    sendMsg unNotificationContent (mkSelector "filterCriteria") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentByUpdatingWithProvider:error:@
contentByUpdatingWithProvider_errorSelector :: Selector
contentByUpdatingWithProvider_errorSelector = mkSelector "contentByUpdatingWithProvider:error:"

-- | @Selector@ for @attachments@
attachmentsSelector :: Selector
attachmentsSelector = mkSelector "attachments"

-- | @Selector@ for @badge@
badgeSelector :: Selector
badgeSelector = mkSelector "badge"

-- | @Selector@ for @body@
bodySelector :: Selector
bodySelector = mkSelector "body"

-- | @Selector@ for @categoryIdentifier@
categoryIdentifierSelector :: Selector
categoryIdentifierSelector = mkSelector "categoryIdentifier"

-- | @Selector@ for @launchImageName@
launchImageNameSelector :: Selector
launchImageNameSelector = mkSelector "launchImageName"

-- | @Selector@ for @sound@
soundSelector :: Selector
soundSelector = mkSelector "sound"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @threadIdentifier@
threadIdentifierSelector :: Selector
threadIdentifierSelector = mkSelector "threadIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @summaryArgument@
summaryArgumentSelector :: Selector
summaryArgumentSelector = mkSelector "summaryArgument"

-- | @Selector@ for @summaryArgumentCount@
summaryArgumentCountSelector :: Selector
summaryArgumentCountSelector = mkSelector "summaryArgumentCount"

-- | @Selector@ for @targetContentIdentifier@
targetContentIdentifierSelector :: Selector
targetContentIdentifierSelector = mkSelector "targetContentIdentifier"

-- | @Selector@ for @interruptionLevel@
interruptionLevelSelector :: Selector
interruptionLevelSelector = mkSelector "interruptionLevel"

-- | @Selector@ for @relevanceScore@
relevanceScoreSelector :: Selector
relevanceScoreSelector = mkSelector "relevanceScore"

-- | @Selector@ for @filterCriteria@
filterCriteriaSelector :: Selector
filterCriteriaSelector = mkSelector "filterCriteria"

