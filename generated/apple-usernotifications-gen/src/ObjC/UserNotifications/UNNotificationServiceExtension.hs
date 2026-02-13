{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationServiceExtension@.
module ObjC.UserNotifications.UNNotificationServiceExtension
  ( UNNotificationServiceExtension
  , IsUNNotificationServiceExtension(..)
  , didReceiveNotificationRequest_withContentHandler
  , serviceExtensionTimeWillExpire
  , didReceiveNotificationRequest_withContentHandlerSelector
  , serviceExtensionTimeWillExpireSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- didReceiveNotificationRequest:withContentHandler:@
didReceiveNotificationRequest_withContentHandler :: (IsUNNotificationServiceExtension unNotificationServiceExtension, IsUNNotificationRequest request) => unNotificationServiceExtension -> request -> Ptr () -> IO ()
didReceiveNotificationRequest_withContentHandler unNotificationServiceExtension request contentHandler =
  sendMessage unNotificationServiceExtension didReceiveNotificationRequest_withContentHandlerSelector (toUNNotificationRequest request) contentHandler

-- | @- serviceExtensionTimeWillExpire@
serviceExtensionTimeWillExpire :: IsUNNotificationServiceExtension unNotificationServiceExtension => unNotificationServiceExtension -> IO ()
serviceExtensionTimeWillExpire unNotificationServiceExtension =
  sendMessage unNotificationServiceExtension serviceExtensionTimeWillExpireSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @didReceiveNotificationRequest:withContentHandler:@
didReceiveNotificationRequest_withContentHandlerSelector :: Selector '[Id UNNotificationRequest, Ptr ()] ()
didReceiveNotificationRequest_withContentHandlerSelector = mkSelector "didReceiveNotificationRequest:withContentHandler:"

-- | @Selector@ for @serviceExtensionTimeWillExpire@
serviceExtensionTimeWillExpireSelector :: Selector '[] ()
serviceExtensionTimeWillExpireSelector = mkSelector "serviceExtensionTimeWillExpire"

