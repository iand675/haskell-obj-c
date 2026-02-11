{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationRequest@.
module ObjC.UserNotifications.UNNotificationRequest
  ( UNNotificationRequest
  , IsUNNotificationRequest(..)
  , requestWithIdentifier_content_trigger
  , init_
  , identifier
  , content
  , trigger
  , requestWithIdentifier_content_triggerSelector
  , initSelector
  , identifierSelector
  , contentSelector
  , triggerSelector


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
import ObjC.Intents.Internal.Classes

-- | @+ requestWithIdentifier:content:trigger:@
requestWithIdentifier_content_trigger :: (IsNSString identifier, IsUNNotificationContent content, IsUNNotificationTrigger trigger) => identifier -> content -> trigger -> IO (Id UNNotificationRequest)
requestWithIdentifier_content_trigger identifier content trigger =
  do
    cls' <- getRequiredClass "UNNotificationRequest"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr content $ \raw_content ->
        withObjCPtr trigger $ \raw_trigger ->
          sendClassMsg cls' (mkSelector "requestWithIdentifier:content:trigger:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_trigger :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsUNNotificationRequest unNotificationRequest => unNotificationRequest -> IO (Id UNNotificationRequest)
init_ unNotificationRequest  =
  sendMsg unNotificationRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsUNNotificationRequest unNotificationRequest => unNotificationRequest -> IO (Id NSString)
identifier unNotificationRequest  =
  sendMsg unNotificationRequest (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- content@
content :: IsUNNotificationRequest unNotificationRequest => unNotificationRequest -> IO (Id UNNotificationContent)
content unNotificationRequest  =
  sendMsg unNotificationRequest (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trigger@
trigger :: IsUNNotificationRequest unNotificationRequest => unNotificationRequest -> IO (Id UNNotificationTrigger)
trigger unNotificationRequest  =
  sendMsg unNotificationRequest (mkSelector "trigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestWithIdentifier:content:trigger:@
requestWithIdentifier_content_triggerSelector :: Selector
requestWithIdentifier_content_triggerSelector = mkSelector "requestWithIdentifier:content:trigger:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @trigger@
triggerSelector :: Selector
triggerSelector = mkSelector "trigger"

