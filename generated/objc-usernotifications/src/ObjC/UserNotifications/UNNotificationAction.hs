{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , identifierSelector
  , titleSelector
  , optionsSelector
  , iconSelector

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

-- | @+ actionWithIdentifier:title:options:@
actionWithIdentifier_title_options :: (IsNSString identifier, IsNSString title) => identifier -> title -> UNNotificationActionOptions -> IO (Id UNNotificationAction)
actionWithIdentifier_title_options identifier title options =
  do
    cls' <- getRequiredClass "UNNotificationAction"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr title $ \raw_title ->
        sendClassMsg cls' (mkSelector "actionWithIdentifier:title:options:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @+ actionWithIdentifier:title:options:icon:@
actionWithIdentifier_title_options_icon :: (IsNSString identifier, IsNSString title, IsUNNotificationActionIcon icon) => identifier -> title -> UNNotificationActionOptions -> icon -> IO (Id UNNotificationAction)
actionWithIdentifier_title_options_icon identifier title options icon =
  do
    cls' <- getRequiredClass "UNNotificationAction"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr title $ \raw_title ->
        withObjCPtr icon $ \raw_icon ->
          sendClassMsg cls' (mkSelector "actionWithIdentifier:title:options:icon:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_icon :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsUNNotificationAction unNotificationAction => unNotificationAction -> IO (Id UNNotificationAction)
init_ unNotificationAction  =
  sendMsg unNotificationAction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsUNNotificationAction unNotificationAction => unNotificationAction -> IO (Id NSString)
identifier unNotificationAction  =
  sendMsg unNotificationAction (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsUNNotificationAction unNotificationAction => unNotificationAction -> IO (Id NSString)
title unNotificationAction  =
  sendMsg unNotificationAction (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- options@
options :: IsUNNotificationAction unNotificationAction => unNotificationAction -> IO UNNotificationActionOptions
options unNotificationAction  =
  fmap (coerce :: CULong -> UNNotificationActionOptions) $ sendMsg unNotificationAction (mkSelector "options") retCULong []

-- | @- icon@
icon :: IsUNNotificationAction unNotificationAction => unNotificationAction -> IO (Id UNNotificationActionIcon)
icon unNotificationAction  =
  sendMsg unNotificationAction (mkSelector "icon") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionWithIdentifier:title:options:@
actionWithIdentifier_title_optionsSelector :: Selector
actionWithIdentifier_title_optionsSelector = mkSelector "actionWithIdentifier:title:options:"

-- | @Selector@ for @actionWithIdentifier:title:options:icon:@
actionWithIdentifier_title_options_iconSelector :: Selector
actionWithIdentifier_title_options_iconSelector = mkSelector "actionWithIdentifier:title:options:icon:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @icon@
iconSelector :: Selector
iconSelector = mkSelector "icon"

