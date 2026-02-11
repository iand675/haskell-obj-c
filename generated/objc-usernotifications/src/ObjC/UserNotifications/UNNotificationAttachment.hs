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
  , initSelector
  , identifierSelector
  , urlSelector
  , typeSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ attachmentWithIdentifier:URL:options:error:@
attachmentWithIdentifier_URL_options_error :: (IsNSString identifier, IsNSURL url, IsNSDictionary options, IsNSError error_) => identifier -> url -> options -> error_ -> IO (Id UNNotificationAttachment)
attachmentWithIdentifier_URL_options_error identifier url options error_ =
  do
    cls' <- getRequiredClass "UNNotificationAttachment"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr url $ \raw_url ->
        withObjCPtr options $ \raw_options ->
          withObjCPtr error_ $ \raw_error_ ->
            sendClassMsg cls' (mkSelector "attachmentWithIdentifier:URL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsUNNotificationAttachment unNotificationAttachment => unNotificationAttachment -> IO (Id UNNotificationAttachment)
init_ unNotificationAttachment  =
  sendMsg unNotificationAttachment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsUNNotificationAttachment unNotificationAttachment => unNotificationAttachment -> IO (Id NSString)
identifier unNotificationAttachment  =
  sendMsg unNotificationAttachment (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URL@
url :: IsUNNotificationAttachment unNotificationAttachment => unNotificationAttachment -> IO (Id NSURL)
url unNotificationAttachment  =
  sendMsg unNotificationAttachment (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsUNNotificationAttachment unNotificationAttachment => unNotificationAttachment -> IO (Id NSString)
type_ unNotificationAttachment  =
  sendMsg unNotificationAttachment (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attachmentWithIdentifier:URL:options:error:@
attachmentWithIdentifier_URL_options_errorSelector :: Selector
attachmentWithIdentifier_URL_options_errorSelector = mkSelector "attachmentWithIdentifier:URL:options:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

