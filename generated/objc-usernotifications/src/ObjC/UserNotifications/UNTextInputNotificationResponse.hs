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

-- | @- userText@
userText :: IsUNTextInputNotificationResponse unTextInputNotificationResponse => unTextInputNotificationResponse -> IO (Id NSString)
userText unTextInputNotificationResponse  =
  sendMsg unTextInputNotificationResponse (mkSelector "userText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @userText@
userTextSelector :: Selector
userTextSelector = mkSelector "userText"

