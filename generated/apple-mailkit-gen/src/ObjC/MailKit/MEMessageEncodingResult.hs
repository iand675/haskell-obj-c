{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains information about an outging mail message after any security measures have been applied.
--
-- Generated bindings for @MEMessageEncodingResult@.
module ObjC.MailKit.MEMessageEncodingResult
  ( MEMessageEncodingResult
  , IsMEMessageEncodingResult(..)
  , new
  , init_
  , initWithEncodedMessage_signingError_encryptionError
  , encodedMessage
  , signingError
  , encryptionError
  , encodedMessageSelector
  , encryptionErrorSelector
  , initSelector
  , initWithEncodedMessage_signingError_encryptionErrorSelector
  , newSelector
  , signingErrorSelector


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
new :: IO (Id MEMessageEncodingResult)
new  =
  do
    cls' <- getRequiredClass "MEMessageEncodingResult"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEMessageEncodingResult meMessageEncodingResult => meMessageEncodingResult -> IO (Id MEMessageEncodingResult)
init_ meMessageEncodingResult =
  sendOwnedMessage meMessageEncodingResult initSelector

-- | @- initWithEncodedMessage:signingError:encryptionError:@
initWithEncodedMessage_signingError_encryptionError :: (IsMEMessageEncodingResult meMessageEncodingResult, IsMEEncodedOutgoingMessage encodedMessage, IsNSError signingError, IsNSError encryptionError) => meMessageEncodingResult -> encodedMessage -> signingError -> encryptionError -> IO (Id MEMessageEncodingResult)
initWithEncodedMessage_signingError_encryptionError meMessageEncodingResult encodedMessage signingError encryptionError =
  sendOwnedMessage meMessageEncodingResult initWithEncodedMessage_signingError_encryptionErrorSelector (toMEEncodedOutgoingMessage encodedMessage) (toNSError signingError) (toNSError encryptionError)

-- | The encoded message. Nil if no need to encode or an error occured while encoding
--
-- ObjC selector: @- encodedMessage@
encodedMessage :: IsMEMessageEncodingResult meMessageEncodingResult => meMessageEncodingResult -> IO (Id MEEncodedOutgoingMessage)
encodedMessage meMessageEncodingResult =
  sendMessage meMessageEncodingResult encodedMessageSelector

-- | Any error that occured while attempting to sign the outgoing message.
--
-- ObjC selector: @- signingError@
signingError :: IsMEMessageEncodingResult meMessageEncodingResult => meMessageEncodingResult -> IO (Id NSError)
signingError meMessageEncodingResult =
  sendMessage meMessageEncodingResult signingErrorSelector

-- | Any error that occured while attempting to encrypt the outgoing message.
--
-- ObjC selector: @- encryptionError@
encryptionError :: IsMEMessageEncodingResult meMessageEncodingResult => meMessageEncodingResult -> IO (Id NSError)
encryptionError meMessageEncodingResult =
  sendMessage meMessageEncodingResult encryptionErrorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEMessageEncodingResult)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEMessageEncodingResult)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEncodedMessage:signingError:encryptionError:@
initWithEncodedMessage_signingError_encryptionErrorSelector :: Selector '[Id MEEncodedOutgoingMessage, Id NSError, Id NSError] (Id MEMessageEncodingResult)
initWithEncodedMessage_signingError_encryptionErrorSelector = mkSelector "initWithEncodedMessage:signingError:encryptionError:"

-- | @Selector@ for @encodedMessage@
encodedMessageSelector :: Selector '[] (Id MEEncodedOutgoingMessage)
encodedMessageSelector = mkSelector "encodedMessage"

-- | @Selector@ for @signingError@
signingErrorSelector :: Selector '[] (Id NSError)
signingErrorSelector = mkSelector "signingError"

-- | @Selector@ for @encryptionError@
encryptionErrorSelector :: Selector '[] (Id NSError)
encryptionErrorSelector = mkSelector "encryptionError"

