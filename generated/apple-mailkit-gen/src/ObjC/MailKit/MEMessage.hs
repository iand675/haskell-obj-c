{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , stateSelector
  , encryptionStateSelector
  , subjectSelector
  , fromAddressSelector
  , toAddressesSelector
  , ccAddressesSelector
  , bccAddressesSelector
  , replyToAddressesSelector
  , allRecipientAddressesSelector
  , dateSentSelector
  , dateReceivedSelector
  , headersSelector
  , rawDataSelector

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

import ObjC.MailKit.Internal.Classes
import ObjC.MailKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMEMessage meMessage => meMessage -> IO (Id MEMessage)
init_ meMessage  =
    sendMsg meMessage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MEMessage)
new  =
  do
    cls' <- getRequiredClass "MEMessage"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The state of the mail message.
--
-- ObjC selector: @- state@
state :: IsMEMessage meMessage => meMessage -> IO MEMessageState
state meMessage  =
    fmap (coerce :: CLong -> MEMessageState) $ sendMsg meMessage (mkSelector "state") retCLong []

-- | The encryption state of the mail message.
--
-- ObjC selector: @- encryptionState@
encryptionState :: IsMEMessage meMessage => meMessage -> IO MEMessageEncryptionState
encryptionState meMessage  =
    fmap (coerce :: CLong -> MEMessageEncryptionState) $ sendMsg meMessage (mkSelector "encryptionState") retCLong []

-- | The subject of the mail message.
--
-- ObjC selector: @- subject@
subject :: IsMEMessage meMessage => meMessage -> IO (Id NSString)
subject meMessage  =
    sendMsg meMessage (mkSelector "subject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Message sender's email address.
--
-- ObjC selector: @- fromAddress@
fromAddress :: IsMEMessage meMessage => meMessage -> IO (Id MEEmailAddress)
fromAddress meMessage  =
    sendMsg meMessage (mkSelector "fromAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Recipient email addresses in the "To" address field of the message.
--
-- ObjC selector: @- toAddresses@
toAddresses :: IsMEMessage meMessage => meMessage -> IO (Id NSArray)
toAddresses meMessage  =
    sendMsg meMessage (mkSelector "toAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Recipient email addresses in the "Cc" address field of the message.
--
-- ObjC selector: @- ccAddresses@
ccAddresses :: IsMEMessage meMessage => meMessage -> IO (Id NSArray)
ccAddresses meMessage  =
    sendMsg meMessage (mkSelector "ccAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Recipient email addresses in the "Bcc" address field of the message.
--
-- ObjC selector: @- bccAddresses@
bccAddresses :: IsMEMessage meMessage => meMessage -> IO (Id NSArray)
bccAddresses meMessage  =
    sendMsg meMessage (mkSelector "bccAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Recipient email addresses in the "Reply-To" field of the message.
--
-- ObjC selector: @- replyToAddresses@
replyToAddresses :: IsMEMessage meMessage => meMessage -> IO (Id NSArray)
replyToAddresses meMessage  =
    sendMsg meMessage (mkSelector "replyToAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array containing all recipients of the message.
--
-- ObjC selector: @- allRecipientAddresses@
allRecipientAddresses :: IsMEMessage meMessage => meMessage -> IO (Id NSArray)
allRecipientAddresses meMessage  =
    sendMsg meMessage (mkSelector "allRecipientAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date the mail message was sent. Optionally set by the by the sender.
--
-- ObjC selector: @- dateSent@
dateSent :: IsMEMessage meMessage => meMessage -> IO (Id NSDate)
dateSent meMessage  =
    sendMsg meMessage (mkSelector "dateSent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date the mail message was received. Only present if the message has been received.
--
-- ObjC selector: @- dateReceived@
dateReceived :: IsMEMessage meMessage => meMessage -> IO (Id NSDate)
dateReceived meMessage  =
    sendMsg meMessage (mkSelector "dateReceived") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The headers for the message. Might only be a subset if the full body has not been downloaded.
--
-- ObjC selector: @- headers@
headers :: IsMEMessage meMessage => meMessage -> IO (Id NSDictionary)
headers meMessage  =
    sendMsg meMessage (mkSelector "headers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The full raw RFC822 message data if it has been downloaded and the extension has permissions to access.
--
-- ObjC selector: @- rawData@
rawData :: IsMEMessage meMessage => meMessage -> IO (Id NSData)
rawData meMessage  =
    sendMsg meMessage (mkSelector "rawData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @encryptionState@
encryptionStateSelector :: Selector
encryptionStateSelector = mkSelector "encryptionState"

-- | @Selector@ for @subject@
subjectSelector :: Selector
subjectSelector = mkSelector "subject"

-- | @Selector@ for @fromAddress@
fromAddressSelector :: Selector
fromAddressSelector = mkSelector "fromAddress"

-- | @Selector@ for @toAddresses@
toAddressesSelector :: Selector
toAddressesSelector = mkSelector "toAddresses"

-- | @Selector@ for @ccAddresses@
ccAddressesSelector :: Selector
ccAddressesSelector = mkSelector "ccAddresses"

-- | @Selector@ for @bccAddresses@
bccAddressesSelector :: Selector
bccAddressesSelector = mkSelector "bccAddresses"

-- | @Selector@ for @replyToAddresses@
replyToAddressesSelector :: Selector
replyToAddressesSelector = mkSelector "replyToAddresses"

-- | @Selector@ for @allRecipientAddresses@
allRecipientAddressesSelector :: Selector
allRecipientAddressesSelector = mkSelector "allRecipientAddresses"

-- | @Selector@ for @dateSent@
dateSentSelector :: Selector
dateSentSelector = mkSelector "dateSent"

-- | @Selector@ for @dateReceived@
dateReceivedSelector :: Selector
dateReceivedSelector = mkSelector "dateReceived"

-- | @Selector@ for @headers@
headersSelector :: Selector
headersSelector = mkSelector "headers"

-- | @Selector@ for @rawData@
rawDataSelector :: Selector
rawDataSelector = mkSelector "rawData"

