{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASGeneratePasswordsRequest@.
module ObjC.AuthenticationServices.ASGeneratePasswordsRequest
  ( ASGeneratePasswordsRequest
  , IsASGeneratePasswordsRequest(..)
  , init_
  , new
  , initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirks
  , serviceIdentifier
  , passwordFieldPasswordRules
  , confirmPasswordFieldPasswordRules
  , passwordRulesFromQuirks
  , initSelector
  , newSelector
  , initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirksSelector
  , serviceIdentifierSelector
  , passwordFieldPasswordRulesSelector
  , confirmPasswordFieldPasswordRulesSelector
  , passwordRulesFromQuirksSelector


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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASGeneratePasswordsRequest asGeneratePasswordsRequest => asGeneratePasswordsRequest -> IO (Id ASGeneratePasswordsRequest)
init_ asGeneratePasswordsRequest  =
  sendMsg asGeneratePasswordsRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASGeneratePasswordsRequest)
new  =
  do
    cls' <- getRequiredClass "ASGeneratePasswordsRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithServiceIdentifier:passwordFieldPasswordRules:confirmPasswordFieldPasswordRules:passwordRulesFromQuirks:@
initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirks :: (IsASGeneratePasswordsRequest asGeneratePasswordsRequest, IsASCredentialServiceIdentifier serviceIdentifier, IsNSString passwordFieldPasswordRules, IsNSString confirmPasswordFieldPasswordRules, IsNSString passwordRulesFromQuirks) => asGeneratePasswordsRequest -> serviceIdentifier -> passwordFieldPasswordRules -> confirmPasswordFieldPasswordRules -> passwordRulesFromQuirks -> IO (Id ASGeneratePasswordsRequest)
initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirks asGeneratePasswordsRequest  serviceIdentifier passwordFieldPasswordRules confirmPasswordFieldPasswordRules passwordRulesFromQuirks =
withObjCPtr serviceIdentifier $ \raw_serviceIdentifier ->
  withObjCPtr passwordFieldPasswordRules $ \raw_passwordFieldPasswordRules ->
    withObjCPtr confirmPasswordFieldPasswordRules $ \raw_confirmPasswordFieldPasswordRules ->
      withObjCPtr passwordRulesFromQuirks $ \raw_passwordRulesFromQuirks ->
          sendMsg asGeneratePasswordsRequest (mkSelector "initWithServiceIdentifier:passwordFieldPasswordRules:confirmPasswordFieldPasswordRules:passwordRulesFromQuirks:") (retPtr retVoid) [argPtr (castPtr raw_serviceIdentifier :: Ptr ()), argPtr (castPtr raw_passwordFieldPasswordRules :: Ptr ()), argPtr (castPtr raw_confirmPasswordFieldPasswordRules :: Ptr ()), argPtr (castPtr raw_passwordRulesFromQuirks :: Ptr ())] >>= ownedObject . castPtr

-- | The identifier of the service for which the the credential would be associated.
--
-- ObjC selector: @- serviceIdentifier@
serviceIdentifier :: IsASGeneratePasswordsRequest asGeneratePasswordsRequest => asGeneratePasswordsRequest -> IO (Id ASCredentialServiceIdentifier)
serviceIdentifier asGeneratePasswordsRequest  =
  sendMsg asGeneratePasswordsRequest (mkSelector "serviceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Developer provided password rules.
--
-- ObjC selector: @- passwordFieldPasswordRules@
passwordFieldPasswordRules :: IsASGeneratePasswordsRequest asGeneratePasswordsRequest => asGeneratePasswordsRequest -> IO (Id NSString)
passwordFieldPasswordRules asGeneratePasswordsRequest  =
  sendMsg asGeneratePasswordsRequest (mkSelector "passwordFieldPasswordRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Developer provided password rules for a "confirm password" field.
--
-- This is only relevant in an HTML context.
--
-- ObjC selector: @- confirmPasswordFieldPasswordRules@
confirmPasswordFieldPasswordRules :: IsASGeneratePasswordsRequest asGeneratePasswordsRequest => asGeneratePasswordsRequest -> IO (Id NSString)
confirmPasswordFieldPasswordRules asGeneratePasswordsRequest  =
  sendMsg asGeneratePasswordsRequest (mkSelector "confirmPasswordFieldPasswordRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Password rules from https://github.com/apple/password-manager-resources
--
-- ObjC selector: @- passwordRulesFromQuirks@
passwordRulesFromQuirks :: IsASGeneratePasswordsRequest asGeneratePasswordsRequest => asGeneratePasswordsRequest -> IO (Id NSString)
passwordRulesFromQuirks asGeneratePasswordsRequest  =
  sendMsg asGeneratePasswordsRequest (mkSelector "passwordRulesFromQuirks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithServiceIdentifier:passwordFieldPasswordRules:confirmPasswordFieldPasswordRules:passwordRulesFromQuirks:@
initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirksSelector :: Selector
initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirksSelector = mkSelector "initWithServiceIdentifier:passwordFieldPasswordRules:confirmPasswordFieldPasswordRules:passwordRulesFromQuirks:"

-- | @Selector@ for @serviceIdentifier@
serviceIdentifierSelector :: Selector
serviceIdentifierSelector = mkSelector "serviceIdentifier"

-- | @Selector@ for @passwordFieldPasswordRules@
passwordFieldPasswordRulesSelector :: Selector
passwordFieldPasswordRulesSelector = mkSelector "passwordFieldPasswordRules"

-- | @Selector@ for @confirmPasswordFieldPasswordRules@
confirmPasswordFieldPasswordRulesSelector :: Selector
confirmPasswordFieldPasswordRulesSelector = mkSelector "confirmPasswordFieldPasswordRules"

-- | @Selector@ for @passwordRulesFromQuirks@
passwordRulesFromQuirksSelector :: Selector
passwordRulesFromQuirksSelector = mkSelector "passwordRulesFromQuirks"

