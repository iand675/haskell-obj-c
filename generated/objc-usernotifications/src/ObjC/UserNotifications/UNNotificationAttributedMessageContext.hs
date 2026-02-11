{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationAttributedMessageContext@.
module ObjC.UserNotifications.UNNotificationAttributedMessageContext
  ( UNNotificationAttributedMessageContext
  , IsUNNotificationAttributedMessageContext(..)
  , contextWithSendMessageIntent_attributedContent
  , init_
  , contextWithSendMessageIntent_attributedContentSelector
  , initSelector


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

-- | @+ contextWithSendMessageIntent:attributedContent:@
contextWithSendMessageIntent_attributedContent :: (IsINSendMessageIntent sendMessageIntent, IsNSAttributedString attributedContent) => sendMessageIntent -> attributedContent -> IO (Id UNNotificationAttributedMessageContext)
contextWithSendMessageIntent_attributedContent sendMessageIntent attributedContent =
  do
    cls' <- getRequiredClass "UNNotificationAttributedMessageContext"
    withObjCPtr sendMessageIntent $ \raw_sendMessageIntent ->
      withObjCPtr attributedContent $ \raw_attributedContent ->
        sendClassMsg cls' (mkSelector "contextWithSendMessageIntent:attributedContent:") (retPtr retVoid) [argPtr (castPtr raw_sendMessageIntent :: Ptr ()), argPtr (castPtr raw_attributedContent :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsUNNotificationAttributedMessageContext unNotificationAttributedMessageContext => unNotificationAttributedMessageContext -> IO (Id UNNotificationAttributedMessageContext)
init_ unNotificationAttributedMessageContext  =
  sendMsg unNotificationAttributedMessageContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contextWithSendMessageIntent:attributedContent:@
contextWithSendMessageIntent_attributedContentSelector :: Selector
contextWithSendMessageIntent_attributedContentSelector = mkSelector "contextWithSendMessageIntent:attributedContent:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

