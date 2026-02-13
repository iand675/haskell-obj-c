{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendMessageIntentDonationMetadata@.
module ObjC.Intents.INSendMessageIntentDonationMetadata
  ( INSendMessageIntentDonationMetadata
  , IsINSendMessageIntentDonationMetadata(..)
  , init_
  , mentionsCurrentUser
  , setMentionsCurrentUser
  , replyToCurrentUser
  , setReplyToCurrentUser
  , notifyRecipientAnyway
  , setNotifyRecipientAnyway
  , recipientCount
  , setRecipientCount
  , initSelector
  , mentionsCurrentUserSelector
  , notifyRecipientAnywaySelector
  , recipientCountSelector
  , replyToCurrentUserSelector
  , setMentionsCurrentUserSelector
  , setNotifyRecipientAnywaySelector
  , setRecipientCountSelector
  , setReplyToCurrentUserSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> IO (Id INSendMessageIntentDonationMetadata)
init_ inSendMessageIntentDonationMetadata =
  sendOwnedMessage inSendMessageIntentDonationMetadata initSelector

-- | @- mentionsCurrentUser@
mentionsCurrentUser :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> IO Bool
mentionsCurrentUser inSendMessageIntentDonationMetadata =
  sendMessage inSendMessageIntentDonationMetadata mentionsCurrentUserSelector

-- | @- setMentionsCurrentUser:@
setMentionsCurrentUser :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> Bool -> IO ()
setMentionsCurrentUser inSendMessageIntentDonationMetadata value =
  sendMessage inSendMessageIntentDonationMetadata setMentionsCurrentUserSelector value

-- | @- replyToCurrentUser@
replyToCurrentUser :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> IO Bool
replyToCurrentUser inSendMessageIntentDonationMetadata =
  sendMessage inSendMessageIntentDonationMetadata replyToCurrentUserSelector

-- | @- setReplyToCurrentUser:@
setReplyToCurrentUser :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> Bool -> IO ()
setReplyToCurrentUser inSendMessageIntentDonationMetadata value =
  sendMessage inSendMessageIntentDonationMetadata setReplyToCurrentUserSelector value

-- | @- notifyRecipientAnyway@
notifyRecipientAnyway :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> IO Bool
notifyRecipientAnyway inSendMessageIntentDonationMetadata =
  sendMessage inSendMessageIntentDonationMetadata notifyRecipientAnywaySelector

-- | @- setNotifyRecipientAnyway:@
setNotifyRecipientAnyway :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> Bool -> IO ()
setNotifyRecipientAnyway inSendMessageIntentDonationMetadata value =
  sendMessage inSendMessageIntentDonationMetadata setNotifyRecipientAnywaySelector value

-- | @- recipientCount@
recipientCount :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> IO CULong
recipientCount inSendMessageIntentDonationMetadata =
  sendMessage inSendMessageIntentDonationMetadata recipientCountSelector

-- | @- setRecipientCount:@
setRecipientCount :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> CULong -> IO ()
setRecipientCount inSendMessageIntentDonationMetadata value =
  sendMessage inSendMessageIntentDonationMetadata setRecipientCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INSendMessageIntentDonationMetadata)
initSelector = mkSelector "init"

-- | @Selector@ for @mentionsCurrentUser@
mentionsCurrentUserSelector :: Selector '[] Bool
mentionsCurrentUserSelector = mkSelector "mentionsCurrentUser"

-- | @Selector@ for @setMentionsCurrentUser:@
setMentionsCurrentUserSelector :: Selector '[Bool] ()
setMentionsCurrentUserSelector = mkSelector "setMentionsCurrentUser:"

-- | @Selector@ for @replyToCurrentUser@
replyToCurrentUserSelector :: Selector '[] Bool
replyToCurrentUserSelector = mkSelector "replyToCurrentUser"

-- | @Selector@ for @setReplyToCurrentUser:@
setReplyToCurrentUserSelector :: Selector '[Bool] ()
setReplyToCurrentUserSelector = mkSelector "setReplyToCurrentUser:"

-- | @Selector@ for @notifyRecipientAnyway@
notifyRecipientAnywaySelector :: Selector '[] Bool
notifyRecipientAnywaySelector = mkSelector "notifyRecipientAnyway"

-- | @Selector@ for @setNotifyRecipientAnyway:@
setNotifyRecipientAnywaySelector :: Selector '[Bool] ()
setNotifyRecipientAnywaySelector = mkSelector "setNotifyRecipientAnyway:"

-- | @Selector@ for @recipientCount@
recipientCountSelector :: Selector '[] CULong
recipientCountSelector = mkSelector "recipientCount"

-- | @Selector@ for @setRecipientCount:@
setRecipientCountSelector :: Selector '[CULong] ()
setRecipientCountSelector = mkSelector "setRecipientCount:"

