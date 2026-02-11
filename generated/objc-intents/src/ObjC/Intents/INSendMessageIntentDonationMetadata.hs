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
  , setMentionsCurrentUserSelector
  , replyToCurrentUserSelector
  , setReplyToCurrentUserSelector
  , notifyRecipientAnywaySelector
  , setNotifyRecipientAnywaySelector
  , recipientCountSelector
  , setRecipientCountSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> IO (Id INSendMessageIntentDonationMetadata)
init_ inSendMessageIntentDonationMetadata  =
  sendMsg inSendMessageIntentDonationMetadata (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- mentionsCurrentUser@
mentionsCurrentUser :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> IO Bool
mentionsCurrentUser inSendMessageIntentDonationMetadata  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inSendMessageIntentDonationMetadata (mkSelector "mentionsCurrentUser") retCULong []

-- | @- setMentionsCurrentUser:@
setMentionsCurrentUser :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> Bool -> IO ()
setMentionsCurrentUser inSendMessageIntentDonationMetadata  value =
  sendMsg inSendMessageIntentDonationMetadata (mkSelector "setMentionsCurrentUser:") retVoid [argCULong (if value then 1 else 0)]

-- | @- replyToCurrentUser@
replyToCurrentUser :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> IO Bool
replyToCurrentUser inSendMessageIntentDonationMetadata  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inSendMessageIntentDonationMetadata (mkSelector "replyToCurrentUser") retCULong []

-- | @- setReplyToCurrentUser:@
setReplyToCurrentUser :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> Bool -> IO ()
setReplyToCurrentUser inSendMessageIntentDonationMetadata  value =
  sendMsg inSendMessageIntentDonationMetadata (mkSelector "setReplyToCurrentUser:") retVoid [argCULong (if value then 1 else 0)]

-- | @- notifyRecipientAnyway@
notifyRecipientAnyway :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> IO Bool
notifyRecipientAnyway inSendMessageIntentDonationMetadata  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inSendMessageIntentDonationMetadata (mkSelector "notifyRecipientAnyway") retCULong []

-- | @- setNotifyRecipientAnyway:@
setNotifyRecipientAnyway :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> Bool -> IO ()
setNotifyRecipientAnyway inSendMessageIntentDonationMetadata  value =
  sendMsg inSendMessageIntentDonationMetadata (mkSelector "setNotifyRecipientAnyway:") retVoid [argCULong (if value then 1 else 0)]

-- | @- recipientCount@
recipientCount :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> IO CULong
recipientCount inSendMessageIntentDonationMetadata  =
  sendMsg inSendMessageIntentDonationMetadata (mkSelector "recipientCount") retCULong []

-- | @- setRecipientCount:@
setRecipientCount :: IsINSendMessageIntentDonationMetadata inSendMessageIntentDonationMetadata => inSendMessageIntentDonationMetadata -> CULong -> IO ()
setRecipientCount inSendMessageIntentDonationMetadata  value =
  sendMsg inSendMessageIntentDonationMetadata (mkSelector "setRecipientCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @mentionsCurrentUser@
mentionsCurrentUserSelector :: Selector
mentionsCurrentUserSelector = mkSelector "mentionsCurrentUser"

-- | @Selector@ for @setMentionsCurrentUser:@
setMentionsCurrentUserSelector :: Selector
setMentionsCurrentUserSelector = mkSelector "setMentionsCurrentUser:"

-- | @Selector@ for @replyToCurrentUser@
replyToCurrentUserSelector :: Selector
replyToCurrentUserSelector = mkSelector "replyToCurrentUser"

-- | @Selector@ for @setReplyToCurrentUser:@
setReplyToCurrentUserSelector :: Selector
setReplyToCurrentUserSelector = mkSelector "setReplyToCurrentUser:"

-- | @Selector@ for @notifyRecipientAnyway@
notifyRecipientAnywaySelector :: Selector
notifyRecipientAnywaySelector = mkSelector "notifyRecipientAnyway"

-- | @Selector@ for @setNotifyRecipientAnyway:@
setNotifyRecipientAnywaySelector :: Selector
setNotifyRecipientAnywaySelector = mkSelector "setNotifyRecipientAnyway:"

-- | @Selector@ for @recipientCount@
recipientCountSelector :: Selector
recipientCountSelector = mkSelector "recipientCount"

-- | @Selector@ for @setRecipientCount:@
setRecipientCountSelector :: Selector
setRecipientCountSelector = mkSelector "setRecipientCount:"

