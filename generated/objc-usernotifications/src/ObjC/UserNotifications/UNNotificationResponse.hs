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
  , initSelector
  , notificationSelector
  , actionIdentifierSelector


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

-- | @- init@
init_ :: IsUNNotificationResponse unNotificationResponse => unNotificationResponse -> IO (Id UNNotificationResponse)
init_ unNotificationResponse  =
  sendMsg unNotificationResponse (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- notification@
notification :: IsUNNotificationResponse unNotificationResponse => unNotificationResponse -> IO (Id UNNotification)
notification unNotificationResponse  =
  sendMsg unNotificationResponse (mkSelector "notification") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- actionIdentifier@
actionIdentifier :: IsUNNotificationResponse unNotificationResponse => unNotificationResponse -> IO (Id NSString)
actionIdentifier unNotificationResponse  =
  sendMsg unNotificationResponse (mkSelector "actionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @notification@
notificationSelector :: Selector
notificationSelector = mkSelector "notification"

-- | @Selector@ for @actionIdentifier@
actionIdentifierSelector :: Selector
actionIdentifierSelector = mkSelector "actionIdentifier"

