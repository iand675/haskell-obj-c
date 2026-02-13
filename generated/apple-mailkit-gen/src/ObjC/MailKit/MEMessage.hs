{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains information about a mail message on which actions can be performed.
--
-- Generated bindings for @MEMessage@.
module ObjC.MailKit.MEMessage
  ( MEMessage
  , IsMEMessage(..)
  , init_
  , new
  , state
  , encryptionState
  , subject
  , fromAddress
  , toAddresses
  , ccAddresses
  , bccAddresses
  , replyToAddresses
  , allRecipientAddresses
  , dateSent
  , dateReceived
  , headers
  , rawData
  , allRecipientAddressesSelector
  , bccAddressesSelector
  , ccAddressesSelector
  , dateReceivedSelector
  , dateSentSelector
  , encryptionStateSelector
  , fromAddressSelector
  , headersSelector
  , initSelector
  , newSelector
  , rawDataSelector
  , replyToAddressesSelector
  , stateSelector
  , subjectSelector
  , toAddressesSelector

  -- * Enum types
  , MEMessageEncryptionState(MEMessageEncryptionState)
  , pattern MEMessageEncryptionStateUnknown
  , pattern MEMessageEncryptionStateNotEncrypted
  , pattern MEMessageEncryptionStateEncrypted
  , MEMessageState(MEMessageState)
  , pattern MEMessageStateReceived
  , pattern MEMessageStateDraft
  , pattern MEMessageStateSending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.MailKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMEMessage meMessage => meMessage -> IO (Id MEMessage)
init_ meMessage =
  sendOwnedMessage meMessage initSelector

-- | @+ new@
new :: IO (Id MEMessage)
new  =
  do
    cls' <- getRequiredClass "MEMessage"
    sendOwnedClassMessage cls' newSelector

-- | The state of the mail message.
--
-- ObjC selector: @- state@
state :: IsMEMessage meMessage => meMessage -> IO MEMessageState
state meMessage =
  sendMessage meMessage stateSelector

-- | The encryption state of the mail message.
--
-- ObjC selector: @- encryptionState@
encryptionState :: IsMEMessage meMessage => meMessage -> IO MEMessageEncryptionState
encryptionState meMessage =
  sendMessage meMessage encryptionStateSelector

-- | The subject of the mail message.
--
-- ObjC selector: @- subject@
subject :: IsMEMessage meMessage => meMessage -> IO (Id NSString)
subject meMessage =
  sendMessage meMessage subjectSelector

-- | Message sender's email address.
--
-- ObjC selector: @- fromAddress@
fromAddress :: IsMEMessage meMessage => meMessage -> IO (Id MEEmailAddress)
fromAddress meMessage =
  sendMessage meMessage fromAddressSelector

-- | Recipient email addresses in the "To" address field of the message.
--
-- ObjC selector: @- toAddresses@
toAddresses :: IsMEMessage meMessage => meMessage -> IO (Id NSArray)
toAddresses meMessage =
  sendMessage meMessage toAddressesSelector

-- | Recipient email addresses in the "Cc" address field of the message.
--
-- ObjC selector: @- ccAddresses@
ccAddresses :: IsMEMessage meMessage => meMessage -> IO (Id NSArray)
ccAddresses meMessage =
  sendMessage meMessage ccAddressesSelector

-- | Recipient email addresses in the "Bcc" address field of the message.
--
-- ObjC selector: @- bccAddresses@
bccAddresses :: IsMEMessage meMessage => meMessage -> IO (Id NSArray)
bccAddresses meMessage =
  sendMessage meMessage bccAddressesSelector

-- | Recipient email addresses in the "Reply-To" field of the message.
--
-- ObjC selector: @- replyToAddresses@
replyToAddresses :: IsMEMessage meMessage => meMessage -> IO (Id NSArray)
replyToAddresses meMessage =
  sendMessage meMessage replyToAddressesSelector

-- | An array containing all recipients of the message.
--
-- ObjC selector: @- allRecipientAddresses@
allRecipientAddresses :: IsMEMessage meMessage => meMessage -> IO (Id NSArray)
allRecipientAddresses meMessage =
  sendMessage meMessage allRecipientAddressesSelector

-- | The date the mail message was sent. Optionally set by the by the sender.
--
-- ObjC selector: @- dateSent@
dateSent :: IsMEMessage meMessage => meMessage -> IO (Id NSDate)
dateSent meMessage =
  sendMessage meMessage dateSentSelector

-- | The date the mail message was received. Only present if the message has been received.
--
-- ObjC selector: @- dateReceived@
dateReceived :: IsMEMessage meMessage => meMessage -> IO (Id NSDate)
dateReceived meMessage =
  sendMessage meMessage dateReceivedSelector

-- | The headers for the message. Might only be a subset if the full body has not been downloaded.
--
-- ObjC selector: @- headers@
headers :: IsMEMessage meMessage => meMessage -> IO (Id NSDictionary)
headers meMessage =
  sendMessage meMessage headersSelector

-- | The full raw RFC822 message data if it has been downloaded and the extension has permissions to access.
--
-- ObjC selector: @- rawData@
rawData :: IsMEMessage meMessage => meMessage -> IO (Id NSData)
rawData meMessage =
  sendMessage meMessage rawDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEMessage)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEMessage)
newSelector = mkSelector "new"

-- | @Selector@ for @state@
stateSelector :: Selector '[] MEMessageState
stateSelector = mkSelector "state"

-- | @Selector@ for @encryptionState@
encryptionStateSelector :: Selector '[] MEMessageEncryptionState
encryptionStateSelector = mkSelector "encryptionState"

-- | @Selector@ for @subject@
subjectSelector :: Selector '[] (Id NSString)
subjectSelector = mkSelector "subject"

-- | @Selector@ for @fromAddress@
fromAddressSelector :: Selector '[] (Id MEEmailAddress)
fromAddressSelector = mkSelector "fromAddress"

-- | @Selector@ for @toAddresses@
toAddressesSelector :: Selector '[] (Id NSArray)
toAddressesSelector = mkSelector "toAddresses"

-- | @Selector@ for @ccAddresses@
ccAddressesSelector :: Selector '[] (Id NSArray)
ccAddressesSelector = mkSelector "ccAddresses"

-- | @Selector@ for @bccAddresses@
bccAddressesSelector :: Selector '[] (Id NSArray)
bccAddressesSelector = mkSelector "bccAddresses"

-- | @Selector@ for @replyToAddresses@
replyToAddressesSelector :: Selector '[] (Id NSArray)
replyToAddressesSelector = mkSelector "replyToAddresses"

-- | @Selector@ for @allRecipientAddresses@
allRecipientAddressesSelector :: Selector '[] (Id NSArray)
allRecipientAddressesSelector = mkSelector "allRecipientAddresses"

-- | @Selector@ for @dateSent@
dateSentSelector :: Selector '[] (Id NSDate)
dateSentSelector = mkSelector "dateSent"

-- | @Selector@ for @dateReceived@
dateReceivedSelector :: Selector '[] (Id NSDate)
dateReceivedSelector = mkSelector "dateReceived"

-- | @Selector@ for @headers@
headersSelector :: Selector '[] (Id NSDictionary)
headersSelector = mkSelector "headers"

-- | @Selector@ for @rawData@
rawDataSelector :: Selector '[] (Id NSData)
rawDataSelector = mkSelector "rawData"

