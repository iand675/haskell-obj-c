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

-- | @- didReceiveNotificationRequest:withContentHandler:@
didReceiveNotificationRequest_withContentHandler :: (IsUNNotificationServiceExtension unNotificationServiceExtension, IsUNNotificationRequest request) => unNotificationServiceExtension -> request -> Ptr () -> IO ()
didReceiveNotificationRequest_withContentHandler unNotificationServiceExtension  request contentHandler =
withObjCPtr request $ \raw_request ->
    sendMsg unNotificationServiceExtension (mkSelector "didReceiveNotificationRequest:withContentHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr contentHandler :: Ptr ())]

-- | @- serviceExtensionTimeWillExpire@
serviceExtensionTimeWillExpire :: IsUNNotificationServiceExtension unNotificationServiceExtension => unNotificationServiceExtension -> IO ()
serviceExtensionTimeWillExpire unNotificationServiceExtension  =
  sendMsg unNotificationServiceExtension (mkSelector "serviceExtensionTimeWillExpire") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @didReceiveNotificationRequest:withContentHandler:@
didReceiveNotificationRequest_withContentHandlerSelector :: Selector
didReceiveNotificationRequest_withContentHandlerSelector = mkSelector "didReceiveNotificationRequest:withContentHandler:"

-- | @Selector@ for @serviceExtensionTimeWillExpire@
serviceExtensionTimeWillExpireSelector :: Selector
serviceExtensionTimeWillExpireSelector = mkSelector "serviceExtensionTimeWillExpire"

