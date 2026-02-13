{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' contextWithSendMessageIntent_attributedContentSelector (toINSendMessageIntent sendMessageIntent) (toNSAttributedString attributedContent)

-- | @- init@
init_ :: IsUNNotificationAttributedMessageContext unNotificationAttributedMessageContext => unNotificationAttributedMessageContext -> IO (Id UNNotificationAttributedMessageContext)
init_ unNotificationAttributedMessageContext =
  sendOwnedMessage unNotificationAttributedMessageContext initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contextWithSendMessageIntent:attributedContent:@
contextWithSendMessageIntent_attributedContentSelector :: Selector '[Id INSendMessageIntent, Id NSAttributedString] (Id UNNotificationAttributedMessageContext)
contextWithSendMessageIntent_attributedContentSelector = mkSelector "contextWithSendMessageIntent:attributedContent:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotificationAttributedMessageContext)
initSelector = mkSelector "init"

