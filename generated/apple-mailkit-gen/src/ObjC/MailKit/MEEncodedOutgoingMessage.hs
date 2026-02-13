{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MEEncodedOutgoingMessage@.
module ObjC.MailKit.MEEncodedOutgoingMessage
  ( MEEncodedOutgoingMessage
  , IsMEEncodedOutgoingMessage(..)
  , initWithRawData_isSigned_isEncrypted
  , rawData
  , isSigned
  , isEncrypted
  , initWithRawData_isSigned_isEncryptedSelector
  , isEncryptedSelector
  , isSignedSelector
  , rawDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithRawData:isSigned:isEncrypted:@
initWithRawData_isSigned_isEncrypted :: (IsMEEncodedOutgoingMessage meEncodedOutgoingMessage, IsNSData rawData) => meEncodedOutgoingMessage -> rawData -> Bool -> Bool -> IO (Id MEEncodedOutgoingMessage)
initWithRawData_isSigned_isEncrypted meEncodedOutgoingMessage rawData isSigned isEncrypted =
  sendOwnedMessage meEncodedOutgoingMessage initWithRawData_isSigned_isEncryptedSelector (toNSData rawData) isSigned isEncrypted

-- | The full encoded RFC822 message including headers and body.
--
-- ObjC selector: @- rawData@
rawData :: IsMEEncodedOutgoingMessage meEncodedOutgoingMessage => meEncodedOutgoingMessage -> IO (Id NSData)
rawData meEncodedOutgoingMessage =
  sendMessage meEncodedOutgoingMessage rawDataSelector

-- | Whether or not the encoded message is signed
--
-- ObjC selector: @- isSigned@
isSigned :: IsMEEncodedOutgoingMessage meEncodedOutgoingMessage => meEncodedOutgoingMessage -> IO Bool
isSigned meEncodedOutgoingMessage =
  sendMessage meEncodedOutgoingMessage isSignedSelector

-- | Whether or not the encoded message is encrypted
--
-- ObjC selector: @- isEncrypted@
isEncrypted :: IsMEEncodedOutgoingMessage meEncodedOutgoingMessage => meEncodedOutgoingMessage -> IO Bool
isEncrypted meEncodedOutgoingMessage =
  sendMessage meEncodedOutgoingMessage isEncryptedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRawData:isSigned:isEncrypted:@
initWithRawData_isSigned_isEncryptedSelector :: Selector '[Id NSData, Bool, Bool] (Id MEEncodedOutgoingMessage)
initWithRawData_isSigned_isEncryptedSelector = mkSelector "initWithRawData:isSigned:isEncrypted:"

-- | @Selector@ for @rawData@
rawDataSelector :: Selector '[] (Id NSData)
rawDataSelector = mkSelector "rawData"

-- | @Selector@ for @isSigned@
isSignedSelector :: Selector '[] Bool
isSignedSelector = mkSelector "isSigned"

-- | @Selector@ for @isEncrypted@
isEncryptedSelector :: Selector '[] Bool
isEncryptedSelector = mkSelector "isEncrypted"

