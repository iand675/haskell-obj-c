{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , titleSelector
  , setTitleSelector
  , subtitleSelector
  , setSubtitleSelector
  , informativeTextSelector
  , setInformativeTextSelector
  , actionButtonTitleSelector
  , setActionButtonTitleSelector
  , userInfoSelector
  , setUserInfoSelector
  , deliveryDateSelector
  , setDeliveryDateSelector
  , deliveryTimeZoneSelector
  , setDeliveryTimeZoneSelector
  , deliveryRepeatIntervalSelector
  , setDeliveryRepeatIntervalSelector
  , actualDeliveryDateSelector
  , presentedSelector
  , remoteSelector
  , soundNameSelector
  , setSoundNameSelector
  , hasActionButtonSelector
  , setHasActionButtonSelector
  , activationTypeSelector
  , otherButtonTitleSelector
  , setOtherButtonTitleSelector
  , identifierSelector
  , setIdentifierSelector
  , contentImageSelector
  , setContentImageSelector
  , hasReplyButtonSelector
  , setHasReplyButtonSelector
  , responsePlaceholderSelector
  , setResponsePlaceholderSelector
  , responseSelector
  , additionalActionsSelector
  , setAdditionalActionsSelector
  , additionalActivationActionSelector

  -- * Enum types
  , NSUserNotificationActivationType(NSUserNotificationActivationType)
  , pattern NSUserNotificationActivationTypeNone
  , pattern NSUserNotificationActivationTypeContentsClicked
  , pattern NSUserNotificationActivationTypeActionButtonClicked
  , pattern NSUserNotificationActivationTypeReplied
  , pattern NSUserNotificationActivationTypeAdditionalActionClicked

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSUserNotification)
init_ nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- title@
title :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
title nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setTitle nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subtitle@
subtitle :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
subtitle nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubtitle:@
setSubtitle :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setSubtitle nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- informativeText@
informativeText :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
informativeText nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "informativeText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInformativeText:@
setInformativeText :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setInformativeText nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setInformativeText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- actionButtonTitle@
actionButtonTitle :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
actionButtonTitle nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "actionButtonTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActionButtonTitle:@
setActionButtonTitle :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setActionButtonTitle nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setActionButtonTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userInfo@
userInfo :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSDictionary)
userInfo nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserInfo:@
setUserInfo :: (IsNSUserNotification nsUserNotification, IsNSDictionary value) => nsUserNotification -> value -> IO ()
setUserInfo nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setUserInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deliveryDate@
deliveryDate :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSDate)
deliveryDate nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "deliveryDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeliveryDate:@
setDeliveryDate :: (IsNSUserNotification nsUserNotification, IsNSDate value) => nsUserNotification -> value -> IO ()
setDeliveryDate nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setDeliveryDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deliveryTimeZone@
deliveryTimeZone :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSTimeZone)
deliveryTimeZone nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "deliveryTimeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeliveryTimeZone:@
setDeliveryTimeZone :: (IsNSUserNotification nsUserNotification, IsNSTimeZone value) => nsUserNotification -> value -> IO ()
setDeliveryTimeZone nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setDeliveryTimeZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deliveryRepeatInterval@
deliveryRepeatInterval :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSDateComponents)
deliveryRepeatInterval nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "deliveryRepeatInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeliveryRepeatInterval:@
setDeliveryRepeatInterval :: (IsNSUserNotification nsUserNotification, IsNSDateComponents value) => nsUserNotification -> value -> IO ()
setDeliveryRepeatInterval nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setDeliveryRepeatInterval:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- actualDeliveryDate@
actualDeliveryDate :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSDate)
actualDeliveryDate nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "actualDeliveryDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- presented@
presented :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO Bool
presented nsUserNotification  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserNotification (mkSelector "presented") retCULong []

-- | @- remote@
remote :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO Bool
remote nsUserNotification  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserNotification (mkSelector "remote") retCULong []

-- | @- soundName@
soundName :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
soundName nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "soundName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSoundName:@
setSoundName :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setSoundName nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setSoundName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hasActionButton@
hasActionButton :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO Bool
hasActionButton nsUserNotification  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserNotification (mkSelector "hasActionButton") retCULong []

-- | @- setHasActionButton:@
setHasActionButton :: IsNSUserNotification nsUserNotification => nsUserNotification -> Bool -> IO ()
setHasActionButton nsUserNotification  value =
    sendMsg nsUserNotification (mkSelector "setHasActionButton:") retVoid [argCULong (if value then 1 else 0)]

-- | @- activationType@
activationType :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO NSUserNotificationActivationType
activationType nsUserNotification  =
    fmap (coerce :: CLong -> NSUserNotificationActivationType) $ sendMsg nsUserNotification (mkSelector "activationType") retCLong []

-- | @- otherButtonTitle@
otherButtonTitle :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
otherButtonTitle nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "otherButtonTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOtherButtonTitle:@
setOtherButtonTitle :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setOtherButtonTitle nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setOtherButtonTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- identifier@
identifier :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
identifier nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setIdentifier nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentImage@
contentImage :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO RawId
contentImage nsUserNotification  =
    fmap (RawId . castPtr) $ sendMsg nsUserNotification (mkSelector "contentImage") (retPtr retVoid) []

-- | @- setContentImage:@
setContentImage :: IsNSUserNotification nsUserNotification => nsUserNotification -> RawId -> IO ()
setContentImage nsUserNotification  value =
    sendMsg nsUserNotification (mkSelector "setContentImage:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- hasReplyButton@
hasReplyButton :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO Bool
hasReplyButton nsUserNotification  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserNotification (mkSelector "hasReplyButton") retCULong []

-- | @- setHasReplyButton:@
setHasReplyButton :: IsNSUserNotification nsUserNotification => nsUserNotification -> Bool -> IO ()
setHasReplyButton nsUserNotification  value =
    sendMsg nsUserNotification (mkSelector "setHasReplyButton:") retVoid [argCULong (if value then 1 else 0)]

-- | @- responsePlaceholder@
responsePlaceholder :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSString)
responsePlaceholder nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "responsePlaceholder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setResponsePlaceholder:@
setResponsePlaceholder :: (IsNSUserNotification nsUserNotification, IsNSString value) => nsUserNotification -> value -> IO ()
setResponsePlaceholder nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setResponsePlaceholder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- response@
response :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSAttributedString)
response nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "response") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- additionalActions@
additionalActions :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSArray)
additionalActions nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "additionalActions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAdditionalActions:@
setAdditionalActions :: (IsNSUserNotification nsUserNotification, IsNSArray value) => nsUserNotification -> value -> IO ()
setAdditionalActions nsUserNotification  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsUserNotification (mkSelector "setAdditionalActions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- additionalActivationAction@
additionalActivationAction :: IsNSUserNotification nsUserNotification => nsUserNotification -> IO (Id NSUserNotificationAction)
additionalActivationAction nsUserNotification  =
    sendMsg nsUserNotification (mkSelector "additionalActivationAction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @informativeText@
informativeTextSelector :: Selector
informativeTextSelector = mkSelector "informativeText"

-- | @Selector@ for @setInformativeText:@
setInformativeTextSelector :: Selector
setInformativeTextSelector = mkSelector "setInformativeText:"

-- | @Selector@ for @actionButtonTitle@
actionButtonTitleSelector :: Selector
actionButtonTitleSelector = mkSelector "actionButtonTitle"

-- | @Selector@ for @setActionButtonTitle:@
setActionButtonTitleSelector :: Selector
setActionButtonTitleSelector = mkSelector "setActionButtonTitle:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @deliveryDate@
deliveryDateSelector :: Selector
deliveryDateSelector = mkSelector "deliveryDate"

-- | @Selector@ for @setDeliveryDate:@
setDeliveryDateSelector :: Selector
setDeliveryDateSelector = mkSelector "setDeliveryDate:"

-- | @Selector@ for @deliveryTimeZone@
deliveryTimeZoneSelector :: Selector
deliveryTimeZoneSelector = mkSelector "deliveryTimeZone"

-- | @Selector@ for @setDeliveryTimeZone:@
setDeliveryTimeZoneSelector :: Selector
setDeliveryTimeZoneSelector = mkSelector "setDeliveryTimeZone:"

-- | @Selector@ for @deliveryRepeatInterval@
deliveryRepeatIntervalSelector :: Selector
deliveryRepeatIntervalSelector = mkSelector "deliveryRepeatInterval"

-- | @Selector@ for @setDeliveryRepeatInterval:@
setDeliveryRepeatIntervalSelector :: Selector
setDeliveryRepeatIntervalSelector = mkSelector "setDeliveryRepeatInterval:"

-- | @Selector@ for @actualDeliveryDate@
actualDeliveryDateSelector :: Selector
actualDeliveryDateSelector = mkSelector "actualDeliveryDate"

-- | @Selector@ for @presented@
presentedSelector :: Selector
presentedSelector = mkSelector "presented"

-- | @Selector@ for @remote@
remoteSelector :: Selector
remoteSelector = mkSelector "remote"

-- | @Selector@ for @soundName@
soundNameSelector :: Selector
soundNameSelector = mkSelector "soundName"

-- | @Selector@ for @setSoundName:@
setSoundNameSelector :: Selector
setSoundNameSelector = mkSelector "setSoundName:"

-- | @Selector@ for @hasActionButton@
hasActionButtonSelector :: Selector
hasActionButtonSelector = mkSelector "hasActionButton"

-- | @Selector@ for @setHasActionButton:@
setHasActionButtonSelector :: Selector
setHasActionButtonSelector = mkSelector "setHasActionButton:"

-- | @Selector@ for @activationType@
activationTypeSelector :: Selector
activationTypeSelector = mkSelector "activationType"

-- | @Selector@ for @otherButtonTitle@
otherButtonTitleSelector :: Selector
otherButtonTitleSelector = mkSelector "otherButtonTitle"

-- | @Selector@ for @setOtherButtonTitle:@
setOtherButtonTitleSelector :: Selector
setOtherButtonTitleSelector = mkSelector "setOtherButtonTitle:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @contentImage@
contentImageSelector :: Selector
contentImageSelector = mkSelector "contentImage"

-- | @Selector@ for @setContentImage:@
setContentImageSelector :: Selector
setContentImageSelector = mkSelector "setContentImage:"

-- | @Selector@ for @hasReplyButton@
hasReplyButtonSelector :: Selector
hasReplyButtonSelector = mkSelector "hasReplyButton"

-- | @Selector@ for @setHasReplyButton:@
setHasReplyButtonSelector :: Selector
setHasReplyButtonSelector = mkSelector "setHasReplyButton:"

-- | @Selector@ for @responsePlaceholder@
responsePlaceholderSelector :: Selector
responsePlaceholderSelector = mkSelector "responsePlaceholder"

-- | @Selector@ for @setResponsePlaceholder:@
setResponsePlaceholderSelector :: Selector
setResponsePlaceholderSelector = mkSelector "setResponsePlaceholder:"

-- | @Selector@ for @response@
responseSelector :: Selector
responseSelector = mkSelector "response"

-- | @Selector@ for @additionalActions@
additionalActionsSelector :: Selector
additionalActionsSelector = mkSelector "additionalActions"

-- | @Selector@ for @setAdditionalActions:@
setAdditionalActionsSelector :: Selector
setAdditionalActionsSelector = mkSelector "setAdditionalActions:"

-- | @Selector@ for @additionalActivationAction@
additionalActivationActionSelector :: Selector
additionalActivationActionSelector = mkSelector "additionalActivationAction"

