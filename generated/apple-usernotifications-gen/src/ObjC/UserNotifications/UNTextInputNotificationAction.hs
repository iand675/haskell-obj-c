{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholderSelector
  , actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholderSelector
  , textInputButtonTitleSelector
  , textInputPlaceholderSelector

  -- * Enum types
  , UNNotificationActionOptions(UNNotificationActionOptions)
  , pattern UNNotificationActionOptionAuthenticationRequired
  , pattern UNNotificationActionOptionDestructive
  , pattern UNNotificationActionOptionForeground

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

-- | @+ actionWithIdentifier:title:options:textInputButtonTitle:textInputPlaceholder:@
actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholder :: (IsNSString identifier, IsNSString title, IsNSString textInputButtonTitle, IsNSString textInputPlaceholder) => identifier -> title -> UNNotificationActionOptions -> textInputButtonTitle -> textInputPlaceholder -> IO (Id UNTextInputNotificationAction)
actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholder identifier title options textInputButtonTitle textInputPlaceholder =
  do
    cls' <- getRequiredClass "UNTextInputNotificationAction"
    sendClassMessage cls' actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholderSelector (toNSString identifier) (toNSString title) options (toNSString textInputButtonTitle) (toNSString textInputPlaceholder)

-- | @+ actionWithIdentifier:title:options:icon:textInputButtonTitle:textInputPlaceholder:@
actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholder :: (IsNSString identifier, IsNSString title, IsUNNotificationActionIcon icon, IsNSString textInputButtonTitle, IsNSString textInputPlaceholder) => identifier -> title -> UNNotificationActionOptions -> icon -> textInputButtonTitle -> textInputPlaceholder -> IO (Id UNTextInputNotificationAction)
actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholder identifier title options icon textInputButtonTitle textInputPlaceholder =
  do
    cls' <- getRequiredClass "UNTextInputNotificationAction"
    sendClassMessage cls' actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholderSelector (toNSString identifier) (toNSString title) options (toUNNotificationActionIcon icon) (toNSString textInputButtonTitle) (toNSString textInputPlaceholder)

-- | @- textInputButtonTitle@
textInputButtonTitle :: IsUNTextInputNotificationAction unTextInputNotificationAction => unTextInputNotificationAction -> IO (Id NSString)
textInputButtonTitle unTextInputNotificationAction =
  sendMessage unTextInputNotificationAction textInputButtonTitleSelector

-- | @- textInputPlaceholder@
textInputPlaceholder :: IsUNTextInputNotificationAction unTextInputNotificationAction => unTextInputNotificationAction -> IO (Id NSString)
textInputPlaceholder unTextInputNotificationAction =
  sendMessage unTextInputNotificationAction textInputPlaceholderSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionWithIdentifier:title:options:textInputButtonTitle:textInputPlaceholder:@
actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholderSelector :: Selector '[Id NSString, Id NSString, UNNotificationActionOptions, Id NSString, Id NSString] (Id UNTextInputNotificationAction)
actionWithIdentifier_title_options_textInputButtonTitle_textInputPlaceholderSelector = mkSelector "actionWithIdentifier:title:options:textInputButtonTitle:textInputPlaceholder:"

-- | @Selector@ for @actionWithIdentifier:title:options:icon:textInputButtonTitle:textInputPlaceholder:@
actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholderSelector :: Selector '[Id NSString, Id NSString, UNNotificationActionOptions, Id UNNotificationActionIcon, Id NSString, Id NSString] (Id UNTextInputNotificationAction)
actionWithIdentifier_title_options_icon_textInputButtonTitle_textInputPlaceholderSelector = mkSelector "actionWithIdentifier:title:options:icon:textInputButtonTitle:textInputPlaceholder:"

-- | @Selector@ for @textInputButtonTitle@
textInputButtonTitleSelector :: Selector '[] (Id NSString)
textInputButtonTitleSelector = mkSelector "textInputButtonTitle"

-- | @Selector@ for @textInputPlaceholder@
textInputPlaceholderSelector :: Selector '[] (Id NSString)
textInputPlaceholderSelector = mkSelector "textInputPlaceholder"

