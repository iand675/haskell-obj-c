{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNTextInputNotificationAction@.
module ObjC.UserNotifications.UNTextInputNotificationAction
  ( UNTextInputNotificationAction
  , IsUNTextInputNotificationAction(..)
  , actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholder
  , actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholder
  , textInputButtonTitle
  , textInputPlaceholder
  , actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholderSelector
  , actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholderSelector
  , textInputButtonTitleSelector
  , textInputPlaceholderSelector

  -- * Enum types
  , UNNotificationActionOptions(UNNotificationActionOptions)
  , pattern UNNotificationActionOptionAuthenticationRequired
  , pattern UNNotificationActionOptionDestructive
  , pattern UNNotificationActionOptionForeground

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

-- | @+ actionWithIdentifier:title:options:textInputButtonTitle:textInputPlaceholder:@
actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholder :: (IsNSString identifier, IsNSString title, IsNSString textInputButtonTitle, IsNSString textInputPlaceholder) => identifier -> title -> UNNotificationActionOptions -> textInputButtonTitle -> textInputPlaceholder -> IO (Id UNTextInputNotificationAction)
actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholder identifier title options textInputButtonTitle textInputPlaceholder =
  do
    cls' <- getRequiredClass "UNTextInputNotificationAction"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr title $ \raw_title ->
        withObjCPtr textInputButtonTitle $ \raw_textInputButtonTitle ->
          withObjCPtr textInputPlaceholder $ \raw_textInputPlaceholder ->
            sendClassMsg cls' (mkSelector "actionWithIdentifier:title:options:textInputButtonTitle:textInputPlaceholder:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_textInputButtonTitle :: Ptr ()), argPtr (castPtr raw_textInputPlaceholder :: Ptr ())] >>= retainedObject . castPtr

-- | @+ actionWithIdentifier:title:options:icon:textInputButtonTitle:textInputPlaceholder:@
actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholder :: (IsNSString identifier, IsNSString title, IsUNNotificationActionIcon icon, IsNSString textInputButtonTitle, IsNSString textInputPlaceholder) => identifier -> title -> UNNotificationActionOptions -> icon -> textInputButtonTitle -> textInputPlaceholder -> IO (Id UNTextInputNotificationAction)
actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholder identifier title options icon textInputButtonTitle textInputPlaceholder =
  do
    cls' <- getRequiredClass "UNTextInputNotificationAction"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr title $ \raw_title ->
        withObjCPtr icon $ \raw_icon ->
          withObjCPtr textInputButtonTitle $ \raw_textInputButtonTitle ->
            withObjCPtr textInputPlaceholder $ \raw_textInputPlaceholder ->
              sendClassMsg cls' (mkSelector "actionWithIdentifier:title:options:icon:textInputButtonTitle:textInputPlaceholder:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_icon :: Ptr ()), argPtr (castPtr raw_textInputButtonTitle :: Ptr ()), argPtr (castPtr raw_textInputPlaceholder :: Ptr ())] >>= retainedObject . castPtr

-- | @- textInputButtonTitle@
textInputButtonTitle :: IsUNTextInputNotificationAction unTextInputNotificationAction => unTextInputNotificationAction -> IO (Id NSString)
textInputButtonTitle unTextInputNotificationAction  =
  sendMsg unTextInputNotificationAction (mkSelector "textInputButtonTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textInputPlaceholder@
textInputPlaceholder :: IsUNTextInputNotificationAction unTextInputNotificationAction => unTextInputNotificationAction -> IO (Id NSString)
textInputPlaceholder unTextInputNotificationAction  =
  sendMsg unTextInputNotificationAction (mkSelector "textInputPlaceholder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionWithIdentifier:title:options:textInputButtonTitle:textInputPlaceholder:@
actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholderSelector :: Selector
actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholderSelector = mkSelector "actionWithIdentifier:title:options:textInputButtonTitle:textInputPlaceholder:"

-- | @Selector@ for @actionWithIdentifier:title:options:icon:textInputButtonTitle:textInputPlaceholder:@
actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholderSelector :: Selector
actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholderSelector = mkSelector "actionWithIdentifier:title:options:icon:textInputButtonTitle:textInputPlaceholder:"

-- | @Selector@ for @textInputButtonTitle@
textInputButtonTitleSelector :: Selector
textInputButtonTitleSelector = mkSelector "textInputButtonTitle"

-- | @Selector@ for @textInputPlaceholder@
textInputPlaceholderSelector :: Selector
textInputPlaceholderSelector = mkSelector "textInputPlaceholder"

