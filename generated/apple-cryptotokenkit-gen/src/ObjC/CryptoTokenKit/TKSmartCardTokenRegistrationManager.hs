{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Provides a centralized management system for registering and unregistering smartcards using their token IDs.
--
-- @Registered smartcard@ keeps its itself accessible via Keychain and system will automatically invoke an NFC slot when a cryptographic operation is required and asks to provide the registered card.
--
-- Generated bindings for @TKSmartCardTokenRegistrationManager@.
module ObjC.CryptoTokenKit.TKSmartCardTokenRegistrationManager
  ( TKSmartCardTokenRegistrationManager
  , IsTKSmartCardTokenRegistrationManager(..)
  , init_
  , registerSmartCardWithTokenID_promptMessage_error
  , unregisterSmartCardWithTokenID_error
  , defaultManager
  , registeredSmartCardTokens
  , defaultManagerSelector
  , initSelector
  , registerSmartCardWithTokenID_promptMessage_errorSelector
  , registeredSmartCardTokensSelector
  , unregisterSmartCardWithTokenID_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsTKSmartCardTokenRegistrationManager tkSmartCardTokenRegistrationManager => tkSmartCardTokenRegistrationManager -> IO (Id TKSmartCardTokenRegistrationManager)
init_ tkSmartCardTokenRegistrationManager =
  sendOwnedMessage tkSmartCardTokenRegistrationManager initSelector

-- | Registers a smartcard with a specific token ID.
--
-- @tokenID@ — ID of the smartcard
--
-- @promptMessage@ — Message that will be shown in the presented system UI when an operation with this smartcard is requested.
--
-- @error@ — On failure, this parameter is set to error describing the failure. On success, it is set to 'nil'.
--
-- In case the same tokenID is already registered, the registration data are overwritten.     In case the smartcard with provided tokenID isn't found in the system, failure is returned.
--
-- ObjC selector: @- registerSmartCardWithTokenID:promptMessage:error:@
registerSmartCardWithTokenID_promptMessage_error :: (IsTKSmartCardTokenRegistrationManager tkSmartCardTokenRegistrationManager, IsNSString tokenID, IsNSString promptMessage, IsNSError error_) => tkSmartCardTokenRegistrationManager -> tokenID -> promptMessage -> error_ -> IO Bool
registerSmartCardWithTokenID_promptMessage_error tkSmartCardTokenRegistrationManager tokenID promptMessage error_ =
  sendMessage tkSmartCardTokenRegistrationManager registerSmartCardWithTokenID_promptMessage_errorSelector (toNSString tokenID) (toNSString promptMessage) (toNSError error_)

-- | Unregisters a smartcard for the provided token ID.
--
-- @tokenID@ — ID of the smartcard
--
-- @error@ — On failure, this parameter is set to error describing the failure. On success, it is set to 'nil'.
--
-- In case the tokenID is not found, failure is returned.
--
-- ObjC selector: @- unregisterSmartCardWithTokenID:error:@
unregisterSmartCardWithTokenID_error :: (IsTKSmartCardTokenRegistrationManager tkSmartCardTokenRegistrationManager, IsNSString tokenID, IsNSError error_) => tkSmartCardTokenRegistrationManager -> tokenID -> error_ -> IO Bool
unregisterSmartCardWithTokenID_error tkSmartCardTokenRegistrationManager tokenID error_ =
  sendMessage tkSmartCardTokenRegistrationManager unregisterSmartCardWithTokenID_errorSelector (toNSString tokenID) (toNSError error_)

-- | Default instance of registration manager
--
-- ObjC selector: @+ defaultManager@
defaultManager :: IO (Id TKSmartCardTokenRegistrationManager)
defaultManager  =
  do
    cls' <- getRequiredClass "TKSmartCardTokenRegistrationManager"
    sendClassMessage cls' defaultManagerSelector

-- | Returns the tokenIDs of all currently registered smart card tokens
--
-- ObjC selector: @- registeredSmartCardTokens@
registeredSmartCardTokens :: IsTKSmartCardTokenRegistrationManager tkSmartCardTokenRegistrationManager => tkSmartCardTokenRegistrationManager -> IO (Id NSArray)
registeredSmartCardTokens tkSmartCardTokenRegistrationManager =
  sendMessage tkSmartCardTokenRegistrationManager registeredSmartCardTokensSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKSmartCardTokenRegistrationManager)
initSelector = mkSelector "init"

-- | @Selector@ for @registerSmartCardWithTokenID:promptMessage:error:@
registerSmartCardWithTokenID_promptMessage_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] Bool
registerSmartCardWithTokenID_promptMessage_errorSelector = mkSelector "registerSmartCardWithTokenID:promptMessage:error:"

-- | @Selector@ for @unregisterSmartCardWithTokenID:error:@
unregisterSmartCardWithTokenID_errorSelector :: Selector '[Id NSString, Id NSError] Bool
unregisterSmartCardWithTokenID_errorSelector = mkSelector "unregisterSmartCardWithTokenID:error:"

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector '[] (Id TKSmartCardTokenRegistrationManager)
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @registeredSmartCardTokens@
registeredSmartCardTokensSelector :: Selector '[] (Id NSArray)
registeredSmartCardTokensSelector = mkSelector "registeredSmartCardTokens"

