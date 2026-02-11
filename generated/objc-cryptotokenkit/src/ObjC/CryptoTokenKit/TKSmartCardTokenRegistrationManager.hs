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
  , initSelector
  , registerSmartCardWithTokenID_promptMessage_errorSelector
  , unregisterSmartCardWithTokenID_errorSelector
  , defaultManagerSelector
  , registeredSmartCardTokensSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsTKSmartCardTokenRegistrationManager tkSmartCardTokenRegistrationManager => tkSmartCardTokenRegistrationManager -> IO (Id TKSmartCardTokenRegistrationManager)
init_ tkSmartCardTokenRegistrationManager  =
  sendMsg tkSmartCardTokenRegistrationManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
registerSmartCardWithTokenID_promptMessage_error tkSmartCardTokenRegistrationManager  tokenID promptMessage error_ =
withObjCPtr tokenID $ \raw_tokenID ->
  withObjCPtr promptMessage $ \raw_promptMessage ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkSmartCardTokenRegistrationManager (mkSelector "registerSmartCardWithTokenID:promptMessage:error:") retCULong [argPtr (castPtr raw_tokenID :: Ptr ()), argPtr (castPtr raw_promptMessage :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

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
unregisterSmartCardWithTokenID_error tkSmartCardTokenRegistrationManager  tokenID error_ =
withObjCPtr tokenID $ \raw_tokenID ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkSmartCardTokenRegistrationManager (mkSelector "unregisterSmartCardWithTokenID:error:") retCULong [argPtr (castPtr raw_tokenID :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Default instance of registration manager
--
-- ObjC selector: @+ defaultManager@
defaultManager :: IO (Id TKSmartCardTokenRegistrationManager)
defaultManager  =
  do
    cls' <- getRequiredClass "TKSmartCardTokenRegistrationManager"
    sendClassMsg cls' (mkSelector "defaultManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the tokenIDs of all currently registered smart card tokens
--
-- ObjC selector: @- registeredSmartCardTokens@
registeredSmartCardTokens :: IsTKSmartCardTokenRegistrationManager tkSmartCardTokenRegistrationManager => tkSmartCardTokenRegistrationManager -> IO (Id NSArray)
registeredSmartCardTokens tkSmartCardTokenRegistrationManager  =
  sendMsg tkSmartCardTokenRegistrationManager (mkSelector "registeredSmartCardTokens") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @registerSmartCardWithTokenID:promptMessage:error:@
registerSmartCardWithTokenID_promptMessage_errorSelector :: Selector
registerSmartCardWithTokenID_promptMessage_errorSelector = mkSelector "registerSmartCardWithTokenID:promptMessage:error:"

-- | @Selector@ for @unregisterSmartCardWithTokenID:error:@
unregisterSmartCardWithTokenID_errorSelector :: Selector
unregisterSmartCardWithTokenID_errorSelector = mkSelector "unregisterSmartCardWithTokenID:error:"

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @registeredSmartCardTokens@
registeredSmartCardTokensSelector :: Selector
registeredSmartCardTokensSelector = mkSelector "registeredSmartCardTokens"

