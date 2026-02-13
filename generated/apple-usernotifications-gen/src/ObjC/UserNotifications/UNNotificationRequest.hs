{-# LANGUAGE DataKinds #-}
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
  , contentSelector
  , identifierSelector
  , initSelector
  , requestWithIdentifier_content_triggerSelector
  , triggerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ requestWithIdentifier:content:trigger:@
requestWithIdentifier_content_trigger :: (IsNSString identifier, IsUNNotificationContent content, IsUNNotificationTrigger trigger) => identifier -> content -> trigger -> IO (Id UNNotificationRequest)
requestWithIdentifier_content_trigger identifier content trigger =
  do
    cls' <- getRequiredClass "UNNotificationRequest"
    sendClassMessage cls' requestWithIdentifier_content_triggerSelector (toNSString identifier) (toUNNotificationContent content) (toUNNotificationTrigger trigger)

-- | @- init@
init_ :: IsUNNotificationRequest unNotificationRequest => unNotificationRequest -> IO (Id UNNotificationRequest)
init_ unNotificationRequest =
  sendOwnedMessage unNotificationRequest initSelector

-- | @- identifier@
identifier :: IsUNNotificationRequest unNotificationRequest => unNotificationRequest -> IO (Id NSString)
identifier unNotificationRequest =
  sendMessage unNotificationRequest identifierSelector

-- | @- content@
content :: IsUNNotificationRequest unNotificationRequest => unNotificationRequest -> IO (Id UNNotificationContent)
content unNotificationRequest =
  sendMessage unNotificationRequest contentSelector

-- | @- trigger@
trigger :: IsUNNotificationRequest unNotificationRequest => unNotificationRequest -> IO (Id UNNotificationTrigger)
trigger unNotificationRequest =
  sendMessage unNotificationRequest triggerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestWithIdentifier:content:trigger:@
requestWithIdentifier_content_triggerSelector :: Selector '[Id NSString, Id UNNotificationContent, Id UNNotificationTrigger] (Id UNNotificationRequest)
requestWithIdentifier_content_triggerSelector = mkSelector "requestWithIdentifier:content:trigger:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotificationRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id UNNotificationContent)
contentSelector = mkSelector "content"

-- | @Selector@ for @trigger@
triggerSelector :: Selector '[] (Id UNNotificationTrigger)
triggerSelector = mkSelector "trigger"

