{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationAction@.
module ObjC.UserNotifications.UNNotificationAction
  ( UNNotificationAction
  , IsUNNotificationAction(..)
  , actionWithIdentifier_title_options
  , actionWithIdentifier_title_options_icon
  , init_
  , identifier
  , title
  , options
  , icon
  , actionWithIdentifier_title_optionsSelector
  , actionWithIdentifier_title_options_iconSelector
  , iconSelector
  , identifierSelector
  , initSelector
  , optionsSelector
  , titleSelector

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

-- | @+ actionWithIdentifier:title:options:@
actionWithIdentifier_title_options :: (IsNSString identifier, IsNSString title) => identifier -> title -> UNNotificationActionOptions -> IO (Id UNNotificationAction)
actionWithIdentifier_title_options identifier title options =
  do
    cls' <- getRequiredClass "UNNotificationAction"
    sendClassMessage cls' actionWithIdentifier_title_optionsSelector (toNSString identifier) (toNSString title) options

-- | @+ actionWithIdentifier:title:options:icon:@
actionWithIdentifier_title_options_icon :: (IsNSString identifier, IsNSString title, IsUNNotificationActionIcon icon) => identifier -> title -> UNNotificationActionOptions -> icon -> IO (Id UNNotificationAction)
actionWithIdentifier_title_options_icon identifier title options icon =
  do
    cls' <- getRequiredClass "UNNotificationAction"
    sendClassMessage cls' actionWithIdentifier_title_options_iconSelector (toNSString identifier) (toNSString title) options (toUNNotificationActionIcon icon)

-- | @- init@
init_ :: IsUNNotificationAction unNotificationAction => unNotificationAction -> IO (Id UNNotificationAction)
init_ unNotificationAction =
  sendOwnedMessage unNotificationAction initSelector

-- | @- identifier@
identifier :: IsUNNotificationAction unNotificationAction => unNotificationAction -> IO (Id NSString)
identifier unNotificationAction =
  sendMessage unNotificationAction identifierSelector

-- | @- title@
title :: IsUNNotificationAction unNotificationAction => unNotificationAction -> IO (Id NSString)
title unNotificationAction =
  sendMessage unNotificationAction titleSelector

-- | @- options@
options :: IsUNNotificationAction unNotificationAction => unNotificationAction -> IO UNNotificationActionOptions
options unNotificationAction =
  sendMessage unNotificationAction optionsSelector

-- | @- icon@
icon :: IsUNNotificationAction unNotificationAction => unNotificationAction -> IO (Id UNNotificationActionIcon)
icon unNotificationAction =
  sendMessage unNotificationAction iconSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionWithIdentifier:title:options:@
actionWithIdentifier_title_optionsSelector :: Selector '[Id NSString, Id NSString, UNNotificationActionOptions] (Id UNNotificationAction)
actionWithIdentifier_title_optionsSelector = mkSelector "actionWithIdentifier:title:options:"

-- | @Selector@ for @actionWithIdentifier:title:options:icon:@
actionWithIdentifier_title_options_iconSelector :: Selector '[Id NSString, Id NSString, UNNotificationActionOptions, Id UNNotificationActionIcon] (Id UNNotificationAction)
actionWithIdentifier_title_options_iconSelector = mkSelector "actionWithIdentifier:title:options:icon:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotificationAction)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] UNNotificationActionOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @icon@
iconSelector :: Selector '[] (Id UNNotificationActionIcon)
iconSelector = mkSelector "icon"

