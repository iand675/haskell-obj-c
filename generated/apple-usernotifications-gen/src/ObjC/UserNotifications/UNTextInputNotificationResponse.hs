{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNTextInputNotificationResponse@.
module ObjC.UserNotifications.UNTextInputNotificationResponse
  ( UNTextInputNotificationResponse
  , IsUNTextInputNotificationResponse(..)
  , userText
  , userTextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- userText@
userText :: IsUNTextInputNotificationResponse unTextInputNotificationResponse => unTextInputNotificationResponse -> IO (Id NSString)
userText unTextInputNotificationResponse =
  sendMessage unTextInputNotificationResponse userTextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @userText@
userTextSelector :: Selector '[] (Id NSString)
userTextSelector = mkSelector "userText"

