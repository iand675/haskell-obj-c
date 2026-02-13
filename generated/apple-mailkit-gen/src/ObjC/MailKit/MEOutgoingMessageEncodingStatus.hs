{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains information about any security measures that will be applied to a mail message when it is sent or any errrors that occurred while verifying security status.
--
-- Generated bindings for @MEOutgoingMessageEncodingStatus@.
module ObjC.MailKit.MEOutgoingMessageEncodingStatus
  ( MEOutgoingMessageEncodingStatus
  , IsMEOutgoingMessageEncodingStatus(..)
  , new
  , init_
  , initWithCanSign_canEncrypt_securityError_addressesFailingEncryption
  , canSign
  , canEncrypt
  , securityError
  , addressesFailingEncryption
  , addressesFailingEncryptionSelector
  , canEncryptSelector
  , canSignSelector
  , initSelector
  , initWithCanSign_canEncrypt_securityError_addressesFailingEncryptionSelector
  , newSelector
  , securityErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MEOutgoingMessageEncodingStatus)
new  =
  do
    cls' <- getRequiredClass "MEOutgoingMessageEncodingStatus"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus => meOutgoingMessageEncodingStatus -> IO (Id MEOutgoingMessageEncodingStatus)
init_ meOutgoingMessageEncodingStatus =
  sendOwnedMessage meOutgoingMessageEncodingStatus initSelector

-- | @- initWithCanSign:canEncrypt:securityError:addressesFailingEncryption:@
initWithCanSign_canEncrypt_securityError_addressesFailingEncryption :: (IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus, IsNSError securityError, IsNSArray addressesFailingEncryption) => meOutgoingMessageEncodingStatus -> Bool -> Bool -> securityError -> addressesFailingEncryption -> IO (Id MEOutgoingMessageEncodingStatus)
initWithCanSign_canEncrypt_securityError_addressesFailingEncryption meOutgoingMessageEncodingStatus canSign canEncrypt securityError addressesFailingEncryption =
  sendOwnedMessage meOutgoingMessageEncodingStatus initWithCanSign_canEncrypt_securityError_addressesFailingEncryptionSelector canSign canEncrypt (toNSError securityError) (toNSArray addressesFailingEncryption)

-- | Whether or not the message can be signed.
--
-- ObjC selector: @- canSign@
canSign :: IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus => meOutgoingMessageEncodingStatus -> IO Bool
canSign meOutgoingMessageEncodingStatus =
  sendMessage meOutgoingMessageEncodingStatus canSignSelector

-- | Whether or not the message can be encrypted.
--
-- ObjC selector: @- canEncrypt@
canEncrypt :: IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus => meOutgoingMessageEncodingStatus -> IO Bool
canEncrypt meOutgoingMessageEncodingStatus =
  sendMessage meOutgoingMessageEncodingStatus canEncryptSelector

-- | Any error that occurred while verifying the security status for the outgoing mail message.
--
-- ObjC selector: @- securityError@
securityError :: IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus => meOutgoingMessageEncodingStatus -> IO (Id NSError)
securityError meOutgoingMessageEncodingStatus =
  sendMessage meOutgoingMessageEncodingStatus securityErrorSelector

-- | A list of any recipients for which the message should be encrypted but an error occurred. This could include missing the public key for the recipient.
--
-- ObjC selector: @- addressesFailingEncryption@
addressesFailingEncryption :: IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus => meOutgoingMessageEncodingStatus -> IO (Id NSArray)
addressesFailingEncryption meOutgoingMessageEncodingStatus =
  sendMessage meOutgoingMessageEncodingStatus addressesFailingEncryptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEOutgoingMessageEncodingStatus)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEOutgoingMessageEncodingStatus)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCanSign:canEncrypt:securityError:addressesFailingEncryption:@
initWithCanSign_canEncrypt_securityError_addressesFailingEncryptionSelector :: Selector '[Bool, Bool, Id NSError, Id NSArray] (Id MEOutgoingMessageEncodingStatus)
initWithCanSign_canEncrypt_securityError_addressesFailingEncryptionSelector = mkSelector "initWithCanSign:canEncrypt:securityError:addressesFailingEncryption:"

-- | @Selector@ for @canSign@
canSignSelector :: Selector '[] Bool
canSignSelector = mkSelector "canSign"

-- | @Selector@ for @canEncrypt@
canEncryptSelector :: Selector '[] Bool
canEncryptSelector = mkSelector "canEncrypt"

-- | @Selector@ for @securityError@
securityErrorSelector :: Selector '[] (Id NSError)
securityErrorSelector = mkSelector "securityError"

-- | @Selector@ for @addressesFailingEncryption@
addressesFailingEncryptionSelector :: Selector '[] (Id NSArray)
addressesFailingEncryptionSelector = mkSelector "addressesFailingEncryption"

