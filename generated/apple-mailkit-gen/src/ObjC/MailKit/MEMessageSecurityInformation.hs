{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains security information about a decoded message
--
-- Generated bindings for @MEMessageSecurityInformation@.
module ObjC.MailKit.MEMessageSecurityInformation
  ( MEMessageSecurityInformation
  , IsMEMessageSecurityInformation(..)
  , new
  , init_
  , initWithSigners_isEncrypted_signingError_encryptionError
  , initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReason
  , signers
  , isEncrypted
  , signingError
  , encryptionError
  , shouldBlockRemoteContent
  , localizedRemoteContentBlockingReason
  , encryptionErrorSelector
  , initSelector
  , initWithSigners_isEncrypted_signingError_encryptionErrorSelector
  , initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReasonSelector
  , isEncryptedSelector
  , localizedRemoteContentBlockingReasonSelector
  , newSelector
  , shouldBlockRemoteContentSelector
  , signersSelector
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
new :: IO (Id MEMessageSecurityInformation)
new  =
  do
    cls' <- getRequiredClass "MEMessageSecurityInformation"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO (Id MEMessageSecurityInformation)
init_ meMessageSecurityInformation =
  sendOwnedMessage meMessageSecurityInformation initSelector

-- | @- initWithSigners:isEncrypted:signingError:encryptionError:@
initWithSigners_isEncrypted_signingError_encryptionError :: (IsMEMessageSecurityInformation meMessageSecurityInformation, IsNSArray signers, IsNSError signingError, IsNSError encryptionError) => meMessageSecurityInformation -> signers -> Bool -> signingError -> encryptionError -> IO (Id MEMessageSecurityInformation)
initWithSigners_isEncrypted_signingError_encryptionError meMessageSecurityInformation signers isEncrypted signingError encryptionError =
  sendOwnedMessage meMessageSecurityInformation initWithSigners_isEncrypted_signingError_encryptionErrorSelector (toNSArray signers) isEncrypted (toNSError signingError) (toNSError encryptionError)

-- | @- initWithSigners:isEncrypted:signingError:encryptionError:shouldBlockRemoteContent:localizedRemoteContentBlockingReason:@
initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReason :: (IsMEMessageSecurityInformation meMessageSecurityInformation, IsNSArray signers, IsNSError signingError, IsNSError encryptionError, IsNSString localizedRemoteContentBlockingReason) => meMessageSecurityInformation -> signers -> Bool -> signingError -> encryptionError -> Bool -> localizedRemoteContentBlockingReason -> IO (Id MEMessageSecurityInformation)
initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReason meMessageSecurityInformation signers isEncrypted signingError encryptionError shouldBlockRemoteContent localizedRemoteContentBlockingReason =
  sendOwnedMessage meMessageSecurityInformation initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReasonSelector (toNSArray signers) isEncrypted (toNSError signingError) (toNSError encryptionError) shouldBlockRemoteContent (toNSString localizedRemoteContentBlockingReason)

-- | The signers of the message
--
-- ObjC selector: @- signers@
signers :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO (Id NSArray)
signers meMessageSecurityInformation =
  sendMessage meMessageSecurityInformation signersSelector

-- | Whether or not the message was encrypted.
--
-- ObjC selector: @- isEncrypted@
isEncrypted :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO Bool
isEncrypted meMessageSecurityInformation =
  sendMessage meMessageSecurityInformation isEncryptedSelector

-- | Any signing error that occured when decoding the message.
--
-- ObjC selector: @- signingError@
signingError :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO (Id NSError)
signingError meMessageSecurityInformation =
  sendMessage meMessageSecurityInformation signingErrorSelector

-- | Any encryption error that occured when decoding the message.
--
-- ObjC selector: @- encryptionError@
encryptionError :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO (Id NSError)
encryptionError meMessageSecurityInformation =
  sendMessage meMessageSecurityInformation encryptionErrorSelector

-- | Whether or not Mail should block loading remote content for the message by default. The user will have the option to load remote content manually.
--
-- ObjC selector: @- shouldBlockRemoteContent@
shouldBlockRemoteContent :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO Bool
shouldBlockRemoteContent meMessageSecurityInformation =
  sendMessage meMessageSecurityInformation shouldBlockRemoteContentSelector

-- | A localized string containing the reason for blocking remote content.
--
-- ObjC selector: @- localizedRemoteContentBlockingReason@
localizedRemoteContentBlockingReason :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO (Id NSString)
localizedRemoteContentBlockingReason meMessageSecurityInformation =
  sendMessage meMessageSecurityInformation localizedRemoteContentBlockingReasonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEMessageSecurityInformation)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEMessageSecurityInformation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSigners:isEncrypted:signingError:encryptionError:@
initWithSigners_isEncrypted_signingError_encryptionErrorSelector :: Selector '[Id NSArray, Bool, Id NSError, Id NSError] (Id MEMessageSecurityInformation)
initWithSigners_isEncrypted_signingError_encryptionErrorSelector = mkSelector "initWithSigners:isEncrypted:signingError:encryptionError:"

-- | @Selector@ for @initWithSigners:isEncrypted:signingError:encryptionError:shouldBlockRemoteContent:localizedRemoteContentBlockingReason:@
initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReasonSelector :: Selector '[Id NSArray, Bool, Id NSError, Id NSError, Bool, Id NSString] (Id MEMessageSecurityInformation)
initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReasonSelector = mkSelector "initWithSigners:isEncrypted:signingError:encryptionError:shouldBlockRemoteContent:localizedRemoteContentBlockingReason:"

-- | @Selector@ for @signers@
signersSelector :: Selector '[] (Id NSArray)
signersSelector = mkSelector "signers"

-- | @Selector@ for @isEncrypted@
isEncryptedSelector :: Selector '[] Bool
isEncryptedSelector = mkSelector "isEncrypted"

-- | @Selector@ for @signingError@
signingErrorSelector :: Selector '[] (Id NSError)
signingErrorSelector = mkSelector "signingError"

-- | @Selector@ for @encryptionError@
encryptionErrorSelector :: Selector '[] (Id NSError)
encryptionErrorSelector = mkSelector "encryptionError"

-- | @Selector@ for @shouldBlockRemoteContent@
shouldBlockRemoteContentSelector :: Selector '[] Bool
shouldBlockRemoteContentSelector = mkSelector "shouldBlockRemoteContent"

-- | @Selector@ for @localizedRemoteContentBlockingReason@
localizedRemoteContentBlockingReasonSelector :: Selector '[] (Id NSString)
localizedRemoteContentBlockingReasonSelector = mkSelector "localizedRemoteContentBlockingReason"

