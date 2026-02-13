{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationAttachment@.
module ObjC.UserNotifications.UNNotificationAttachment
  ( UNNotificationAttachment
  , IsUNNotificationAttachment(..)
  , attachmentWithIdentifier_URL_options_error
  , init_
  , identifier
  , url
  , type_
  , attachmentWithIdentifier_URL_options_errorSelector
  , identifierSelector
  , initSelector
  , typeSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ attachmentWithIdentifier:URL:options:error:@
attachmentWithIdentifier_URL_options_error :: (IsNSString identifier, IsNSURL url, IsNSDictionary options, IsNSError error_) => identifier -> url -> options -> error_ -> IO (Id UNNotificationAttachment)
attachmentWithIdentifier_URL_options_error identifier url options error_ =
  do
    cls' <- getRequiredClass "UNNotificationAttachment"
    sendClassMessage cls' attachmentWithIdentifier_URL_options_errorSelector (toNSString identifier) (toNSURL url) (toNSDictionary options) (toNSError error_)

-- | @- init@
init_ :: IsUNNotificationAttachment unNotificationAttachment => unNotificationAttachment -> IO (Id UNNotificationAttachment)
init_ unNotificationAttachment =
  sendOwnedMessage unNotificationAttachment initSelector

-- | @- identifier@
identifier :: IsUNNotificationAttachment unNotificationAttachment => unNotificationAttachment -> IO (Id NSString)
identifier unNotificationAttachment =
  sendMessage unNotificationAttachment identifierSelector

-- | @- URL@
url :: IsUNNotificationAttachment unNotificationAttachment => unNotificationAttachment -> IO (Id NSURL)
url unNotificationAttachment =
  sendMessage unNotificationAttachment urlSelector

-- | @- type@
type_ :: IsUNNotificationAttachment unNotificationAttachment => unNotificationAttachment -> IO (Id NSString)
type_ unNotificationAttachment =
  sendMessage unNotificationAttachment typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attachmentWithIdentifier:URL:options:error:@
attachmentWithIdentifier_URL_options_errorSelector :: Selector '[Id NSString, Id NSURL, Id NSDictionary, Id NSError] (Id UNNotificationAttachment)
attachmentWithIdentifier_URL_options_errorSelector = mkSelector "attachmentWithIdentifier:URL:options:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotificationAttachment)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

