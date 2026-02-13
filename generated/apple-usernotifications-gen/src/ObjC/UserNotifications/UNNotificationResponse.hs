{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationResponse@.
module ObjC.UserNotifications.UNNotificationResponse
  ( UNNotificationResponse
  , IsUNNotificationResponse(..)
  , init_
  , notification
  , actionIdentifier
  , actionIdentifierSelector
  , initSelector
  , notificationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsUNNotificationResponse unNotificationResponse => unNotificationResponse -> IO (Id UNNotificationResponse)
init_ unNotificationResponse =
  sendOwnedMessage unNotificationResponse initSelector

-- | @- notification@
notification :: IsUNNotificationResponse unNotificationResponse => unNotificationResponse -> IO (Id UNNotification)
notification unNotificationResponse =
  sendMessage unNotificationResponse notificationSelector

-- | @- actionIdentifier@
actionIdentifier :: IsUNNotificationResponse unNotificationResponse => unNotificationResponse -> IO (Id NSString)
actionIdentifier unNotificationResponse =
  sendMessage unNotificationResponse actionIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotificationResponse)
initSelector = mkSelector "init"

-- | @Selector@ for @notification@
notificationSelector :: Selector '[] (Id UNNotification)
notificationSelector = mkSelector "notification"

-- | @Selector@ for @actionIdentifier@
actionIdentifierSelector :: Selector '[] (Id NSString)
actionIdentifierSelector = mkSelector "actionIdentifier"

