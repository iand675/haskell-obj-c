{-# LANGUAGE DataKinds #-}
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
  , confirmPasswordFieldPasswordRulesSelector
  , initSelector
  , initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirksSelector
  , newSelector
  , passwordFieldPasswordRulesSelector
  , passwordRulesFromQuirksSelector
  , serviceIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASGeneratePasswordsRequest asGeneratePasswordsRequest => asGeneratePasswordsRequest -> IO (Id ASGeneratePasswordsRequest)
init_ asGeneratePasswordsRequest =
  sendOwnedMessage asGeneratePasswordsRequest initSelector

-- | @+ new@
new :: IO (Id ASGeneratePasswordsRequest)
new  =
  do
    cls' <- getRequiredClass "ASGeneratePasswordsRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithServiceIdentifier:passwordFieldPasswordRules:confirmPasswordFieldPasswordRules:passwordRulesFromQuirks:@
initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirks :: (IsASGeneratePasswordsRequest asGeneratePasswordsRequest, IsASCredentialServiceIdentifier serviceIdentifier, IsNSString passwordFieldPasswordRules, IsNSString confirmPasswordFieldPasswordRules, IsNSString passwordRulesFromQuirks) => asGeneratePasswordsRequest -> serviceIdentifier -> passwordFieldPasswordRules -> confirmPasswordFieldPasswordRules -> passwordRulesFromQuirks -> IO (Id ASGeneratePasswordsRequest)
initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirks asGeneratePasswordsRequest serviceIdentifier passwordFieldPasswordRules confirmPasswordFieldPasswordRules passwordRulesFromQuirks =
  sendOwnedMessage asGeneratePasswordsRequest initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirksSelector (toASCredentialServiceIdentifier serviceIdentifier) (toNSString passwordFieldPasswordRules) (toNSString confirmPasswordFieldPasswordRules) (toNSString passwordRulesFromQuirks)

-- | The identifier of the service for which the the credential would be associated.
--
-- ObjC selector: @- serviceIdentifier@
serviceIdentifier :: IsASGeneratePasswordsRequest asGeneratePasswordsRequest => asGeneratePasswordsRequest -> IO (Id ASCredentialServiceIdentifier)
serviceIdentifier asGeneratePasswordsRequest =
  sendMessage asGeneratePasswordsRequest serviceIdentifierSelector

-- | Developer provided password rules.
--
-- ObjC selector: @- passwordFieldPasswordRules@
passwordFieldPasswordRules :: IsASGeneratePasswordsRequest asGeneratePasswordsRequest => asGeneratePasswordsRequest -> IO (Id NSString)
passwordFieldPasswordRules asGeneratePasswordsRequest =
  sendMessage asGeneratePasswordsRequest passwordFieldPasswordRulesSelector

-- | Developer provided password rules for a "confirm password" field.
--
-- This is only relevant in an HTML context.
--
-- ObjC selector: @- confirmPasswordFieldPasswordRules@
confirmPasswordFieldPasswordRules :: IsASGeneratePasswordsRequest asGeneratePasswordsRequest => asGeneratePasswordsRequest -> IO (Id NSString)
confirmPasswordFieldPasswordRules asGeneratePasswordsRequest =
  sendMessage asGeneratePasswordsRequest confirmPasswordFieldPasswordRulesSelector

-- | Password rules from https://github.com/apple/password-manager-resources
--
-- ObjC selector: @- passwordRulesFromQuirks@
passwordRulesFromQuirks :: IsASGeneratePasswordsRequest asGeneratePasswordsRequest => asGeneratePasswordsRequest -> IO (Id NSString)
passwordRulesFromQuirks asGeneratePasswordsRequest =
  sendMessage asGeneratePasswordsRequest passwordRulesFromQuirksSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASGeneratePasswordsRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASGeneratePasswordsRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithServiceIdentifier:passwordFieldPasswordRules:confirmPasswordFieldPasswordRules:passwordRulesFromQuirks:@
initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirksSelector :: Selector '[Id ASCredentialServiceIdentifier, Id NSString, Id NSString, Id NSString] (Id ASGeneratePasswordsRequest)
initWithServiceIdentifier_passwordFieldPasswordRules_confirmPasswordFieldPasswordRules_passwordRulesFromQuirksSelector = mkSelector "initWithServiceIdentifier:passwordFieldPasswordRules:confirmPasswordFieldPasswordRules:passwordRulesFromQuirks:"

-- | @Selector@ for @serviceIdentifier@
serviceIdentifierSelector :: Selector '[] (Id ASCredentialServiceIdentifier)
serviceIdentifierSelector = mkSelector "serviceIdentifier"

-- | @Selector@ for @passwordFieldPasswordRules@
passwordFieldPasswordRulesSelector :: Selector '[] (Id NSString)
passwordFieldPasswordRulesSelector = mkSelector "passwordFieldPasswordRules"

-- | @Selector@ for @confirmPasswordFieldPasswordRules@
confirmPasswordFieldPasswordRulesSelector :: Selector '[] (Id NSString)
confirmPasswordFieldPasswordRulesSelector = mkSelector "confirmPasswordFieldPasswordRules"

-- | @Selector@ for @passwordRulesFromQuirks@
passwordRulesFromQuirksSelector :: Selector '[] (Id NSString)
passwordRulesFromQuirksSelector = mkSelector "passwordRulesFromQuirks"

