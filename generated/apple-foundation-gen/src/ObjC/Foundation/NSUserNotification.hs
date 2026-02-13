{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserNotification@.
module ObjC.Foundation.NSUserNotification
  ( NSUserNotification
  , IsNSUserNotification(..)
  , init_
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , informativeText
  , setInformativeText
  , actionButtonTitle
  , setActionButtonTitle
  , userInfo
  , setUserInfo
  , deliveryDate
  , setDeliveryDate
  , deliveryTimeZone
  , setDeliveryTimeZone
  , deliveryRepeatInterval
  , setDeliveryRepeatInterval
  , actualDeliveryDate
  , presented
  , remote
  , soundName
  , setSoundName
  , hasActionButton
  , setHasActionButton
  , activationType
  , otherButtonTitle
  , setOtherButtonTitle
  , identifier
  , setIdentifier
  , contentImage
  , setContentImage
  , hasReplyButton
  , setHasReplyButton
  , responsePlaceholder
  , setResponsePlaceholder
  , response
  , additionalActions
  , setAdditionalActions
  , additionalActivationAction
  , actionButtonTitleSelector
  , activationTypeSelector
  , actualDeliveryDateSelector
  , additionalActionsSelector
  , additionalActivationActionSelector
  , contentImageSelector
  , deliveryDateSelector
  , deliveryRepeatIntervalSelector
  , deliveryTimeZoneSelector
  , hasActionButtonSelector
  , hasReplyButtonSelector
  , identifierSelector
  , informativeTextSelector
  , initSelector
  , otherButtonTitleSelector
  , presentedSelector
  , remoteSelector
  , responsePlaceholderSelector
  , responseSelector
  , setActionButtonTitleSelector
  , setAdditionalActionsSelector
  , setContentImageSelector
  , setDeliveryDateSelector
  , setDeliveryRepeatIntervalSelector
  , setDeliveryTimeZoneSelector
  , setHasActionButtonSelector
  , setHasReplyButtonSelector
  , setIdentifierSelector
  , setInformativeTextSelector
  , setOtherButtonTitleSelector
  , setResponsePlaceholderSelector
  , setSoundNameSelector
  , setSubtitleSelector
  , setTitleSelector
  , setUserInfoSelector
  , soundNameSelector
  , subtitleSelector
  , titleSelector
  , userInfoSelector

  -- * Enum types
  , NSUserNotificationActivationType(NSUserNotificationActivationType)
  , pattern NSUserNotificationActivationTypeNone
  , pattern NSUserNotificationActivationTypeContentsClicked
  , pattern NSUserNotificationActivationTypeActionButtonClicked
  , pattern NSUserNotificationActivationTypeReplied
  , pattern NSUserNotificationActivationTypeAdditionalActionClicked

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSUserNotification)
init_ nsUserNotification =
  sendOwnedMessage nsUserNotification initSelector

-- | @- title@
title :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
title nsUserNotification =
  sendMessage nsUserNotification titleSelector

-- | @- setTitle:@
setTitle :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setTitle nsUserNotification value =
  sendMessage nsUserNotification setTitleSelector (toNSString value)

-- | @- subtitle@
subtitle :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
subtitle nsUserNotification =
  sendMessage nsUserNotification subtitleSelector

-- | @- setSubtitle:@
setSubtitle :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setSubtitle nsUserNotification value =
  sendMessage nsUserNotification setSubtitleSelector (toNSString value)

-- | @- informativeText@
informativeText :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
informativeText nsUserNotification =
  sendMessage nsUserNotification informativeTextSelector

-- | @- setInformativeText:@
setInformativeText :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setInformativeText nsUserNotification value =
  sendMessage nsUserNotification setInformativeTextSelector (toNSString value)

-- | @- actionButtonTitle@
actionButtonTitle :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
actionButtonTitle nsUserNotification =
  sendMessage nsUserNotification actionButtonTitleSelector

-- | @- setActionButtonTitle:@
setActionButtonTitle :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setActionButtonTitle nsUserNotification value =
  sendMessage nsUserNotification setActionButtonTitleSelector (toNSString value)

-- | @- userInfo@
userInfo :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSDictionary)
userInfo nsUserNotification =
  sendMessage nsUserNotification userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: (IsNSUserNotification nsUserNotification, IsNSDictionary value) => nsUserNotification -> value -> IO ()
setUserInfo nsUserNotification value =
  sendMessage nsUserNotification setUserInfoSelector (toNSDictionary value)

-- | @- deliveryDate@
deliveryDate :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSDate)
deliveryDate nsUserNotification =
  sendMessage nsUserNotification deliveryDateSelector

-- | @- setDeliveryDate:@
setDeliveryDate :: (IsNSUserNotification nsUserNotification, IsNSDate value) => nsUserNotification -> value -> IO ()
setDeliveryDate nsUserNotification value =
  sendMessage nsUserNotification setDeliveryDateSelector (toNSDate value)

-- | @- deliveryTimeZone@
deliveryTimeZone :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSTimeZone)
deliveryTimeZone nsUserNotification =
  sendMessage nsUserNotification deliveryTimeZoneSelector

-- | @- setDeliveryTimeZone:@
setDeliveryTimeZone :: (IsNSUserNotification nsUserNotification, IsNSTimeZone value) => nsUserNotification -> value -> IO ()
setDeliveryTimeZone nsUserNotification value =
  sendMessage nsUserNotification setDeliveryTimeZoneSelector (toNSTimeZone value)

-- | @- deliveryRepeatInterval@
deliveryRepeatInterval :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSDateComponents)
deliveryRepeatInterval nsUserNotification =
  sendMessage nsUserNotification deliveryRepeatIntervalSelector

-- | @- setDeliveryRepeatInterval:@
setDeliveryRepeatInterval :: (IsNSUserNotification nsUserNotification, IsNSDateComponents value) => nsUserNotification -> value -> IO ()
setDeliveryRepeatInterval nsUserNotification value =
  sendMessage nsUserNotification setDeliveryRepeatIntervalSelector (toNSDateComponents value)

-- | @- actualDeliveryDate@
actualDeliveryDate :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSDate)
actualDeliveryDate nsUserNotification =
  sendMessage nsUserNotification actualDeliveryDateSelector

-- | @- presented@
presented :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO Bool
presented nsUserNotification =
  sendMessage nsUserNotification presentedSelector

-- | @- remote@
remote :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO Bool
remote nsUserNotification =
  sendMessage nsUserNotification remoteSelector

-- | @- soundName@
soundName :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
soundName nsUserNotification =
  sendMessage nsUserNotification soundNameSelector

-- | @- setSoundName:@
setSoundName :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setSoundName nsUserNotification value =
  sendMessage nsUserNotification setSoundNameSelector (toNSString value)

-- | @- hasActionButton@
hasActionButton :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO Bool
hasActionButton nsUserNotification =
  sendMessage nsUserNotification hasActionButtonSelector

-- | @- setHasActionButton:@
setHasActionButton :: IsNSUserNotification nsUserNotification => nsUserNotification -> Bool -> IO ()
setHasActionButton nsUserNotification value =
  sendMessage nsUserNotification setHasActionButtonSelector value

-- | @- activationType@
activationType :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO NSUserNotificationActivationType
activationType nsUserNotification =
  sendMessage nsUserNotification activationTypeSelector

-- | @- otherButtonTitle@
otherButtonTitle :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
otherButtonTitle nsUserNotification =
  sendMessage nsUserNotification otherButtonTitleSelector

-- | @- setOtherButtonTitle:@
setOtherButtonTitle :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setOtherButtonTitle nsUserNotification value =
  sendMessage nsUserNotification setOtherButtonTitleSelector (toNSString value)

-- | @- identifier@
identifier :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
identifier nsUserNotification =
  sendMessage nsUserNotification identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setIdentifier nsUserNotification value =
  sendMessage nsUserNotification setIdentifierSelector (toNSString value)

-- | @- contentImage@
contentImage :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO RawId
contentImage nsUserNotification =
  sendMessage nsUserNotification contentImageSelector

-- | @- setContentImage:@
setContentImage :: IsNSUserNotification nsUserNotification => nsUserNotification -> RawId -> IO ()
setContentImage nsUserNotification value =
  sendMessage nsUserNotification setContentImageSelector value

-- | @- hasReplyButton@
hasReplyButton :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO Bool
hasReplyButton nsUserNotification =
  sendMessage nsUserNotification hasReplyButtonSelector

-- | @- setHasReplyButton:@
setHasReplyButton :: IsNSUserNotification nsUserNotification => nsUserNotification -> Bool -> IO ()
setHasReplyButton nsUserNotification value =
  sendMessage nsUserNotification setHasReplyButtonSelector value

-- | @- responsePlaceholder@
responsePlaceholder :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
responsePlaceholder nsUserNotification =
  sendMessage nsUserNotification responsePlaceholderSelector

-- | @- setResponsePlaceholder:@
setResponsePlaceholder :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setResponsePlaceholder nsUserNotification value =
  sendMessage nsUserNotification setResponsePlaceholderSelector (toNSString value)

-- | @- response@
response :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSAttributedString)
response nsUserNotification =
  sendMessage nsUserNotification responseSelector

-- | @- additionalActions@
additionalActions :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSArray)
additionalActions nsUserNotification =
  sendMessage nsUserNotification additionalActionsSelector

-- | @- setAdditionalActions:@
setAdditionalActions :: (IsNSUserNotification nsUserNotification, IsNSArray value) => nsUserNotification -> value -> IO ()
setAdditionalActions nsUserNotification value =
  sendMessage nsUserNotification setAdditionalActionsSelector (toNSArray value)

-- | @- additionalActivationAction@
additionalActivationAction :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSUserNotificationAction)
additionalActivationAction nsUserNotification =
  sendMessage nsUserNotification additionalActivationActionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSUserNotification)
initSelector = mkSelector "init"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @informativeText@
informativeTextSelector :: Selector '[] (Id NSString)
informativeTextSelector = mkSelector "informativeText"

-- | @Selector@ for @setInformativeText:@
setInformativeTextSelector :: Selector '[Id NSString] ()
setInformativeTextSelector = mkSelector "setInformativeText:"

-- | @Selector@ for @actionButtonTitle@
actionButtonTitleSelector :: Selector '[] (Id NSString)
actionButtonTitleSelector = mkSelector "actionButtonTitle"

-- | @Selector@ for @setActionButtonTitle:@
setActionButtonTitleSelector :: Selector '[Id NSString] ()
setActionButtonTitleSelector = mkSelector "setActionButtonTitle:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @deliveryDate@
deliveryDateSelector :: Selector '[] (Id NSDate)
deliveryDateSelector = mkSelector "deliveryDate"

-- | @Selector@ for @setDeliveryDate:@
setDeliveryDateSelector :: Selector '[Id NSDate] ()
setDeliveryDateSelector = mkSelector "setDeliveryDate:"

-- | @Selector@ for @deliveryTimeZone@
deliveryTimeZoneSelector :: Selector '[] (Id NSTimeZone)
deliveryTimeZoneSelector = mkSelector "deliveryTimeZone"

-- | @Selector@ for @setDeliveryTimeZone:@
setDeliveryTimeZoneSelector :: Selector '[Id NSTimeZone] ()
setDeliveryTimeZoneSelector = mkSelector "setDeliveryTimeZone:"

-- | @Selector@ for @deliveryRepeatInterval@
deliveryRepeatIntervalSelector :: Selector '[] (Id NSDateComponents)
deliveryRepeatIntervalSelector = mkSelector "deliveryRepeatInterval"

-- | @Selector@ for @setDeliveryRepeatInterval:@
setDeliveryRepeatIntervalSelector :: Selector '[Id NSDateComponents] ()
setDeliveryRepeatIntervalSelector = mkSelector "setDeliveryRepeatInterval:"

-- | @Selector@ for @actualDeliveryDate@
actualDeliveryDateSelector :: Selector '[] (Id NSDate)
actualDeliveryDateSelector = mkSelector "actualDeliveryDate"

-- | @Selector@ for @presented@
presentedSelector :: Selector '[] Bool
presentedSelector = mkSelector "presented"

-- | @Selector@ for @remote@
remoteSelector :: Selector '[] Bool
remoteSelector = mkSelector "remote"

-- | @Selector@ for @soundName@
soundNameSelector :: Selector '[] (Id NSString)
soundNameSelector = mkSelector "soundName"

-- | @Selector@ for @setSoundName:@
setSoundNameSelector :: Selector '[Id NSString] ()
setSoundNameSelector = mkSelector "setSoundName:"

-- | @Selector@ for @hasActionButton@
hasActionButtonSelector :: Selector '[] Bool
hasActionButtonSelector = mkSelector "hasActionButton"

-- | @Selector@ for @setHasActionButton:@
setHasActionButtonSelector :: Selector '[Bool] ()
setHasActionButtonSelector = mkSelector "setHasActionButton:"

-- | @Selector@ for @activationType@
activationTypeSelector :: Selector '[] NSUserNotificationActivationType
activationTypeSelector = mkSelector "activationType"

-- | @Selector@ for @otherButtonTitle@
otherButtonTitleSelector :: Selector '[] (Id NSString)
otherButtonTitleSelector = mkSelector "otherButtonTitle"

-- | @Selector@ for @setOtherButtonTitle:@
setOtherButtonTitleSelector :: Selector '[Id NSString] ()
setOtherButtonTitleSelector = mkSelector "setOtherButtonTitle:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @contentImage@
contentImageSelector :: Selector '[] RawId
contentImageSelector = mkSelector "contentImage"

-- | @Selector@ for @setContentImage:@
setContentImageSelector :: Selector '[RawId] ()
setContentImageSelector = mkSelector "setContentImage:"

-- | @Selector@ for @hasReplyButton@
hasReplyButtonSelector :: Selector '[] Bool
hasReplyButtonSelector = mkSelector "hasReplyButton"

-- | @Selector@ for @setHasReplyButton:@
setHasReplyButtonSelector :: Selector '[Bool] ()
setHasReplyButtonSelector = mkSelector "setHasReplyButton:"

-- | @Selector@ for @responsePlaceholder@
responsePlaceholderSelector :: Selector '[] (Id NSString)
responsePlaceholderSelector = mkSelector "responsePlaceholder"

-- | @Selector@ for @setResponsePlaceholder:@
setResponsePlaceholderSelector :: Selector '[Id NSString] ()
setResponsePlaceholderSelector = mkSelector "setResponsePlaceholder:"

-- | @Selector@ for @response@
responseSelector :: Selector '[] (Id NSAttributedString)
responseSelector = mkSelector "response"

-- | @Selector@ for @additionalActions@
additionalActionsSelector :: Selector '[] (Id NSArray)
additionalActionsSelector = mkSelector "additionalActions"

-- | @Selector@ for @setAdditionalActions:@
setAdditionalActionsSelector :: Selector '[Id NSArray] ()
setAdditionalActionsSelector = mkSelector "setAdditionalActions:"

-- | @Selector@ for @additionalActivationAction@
additionalActivationActionSelector :: Selector '[] (Id NSUserNotificationAction)
additionalActivationActionSelector = mkSelector "additionalActivationAction"

