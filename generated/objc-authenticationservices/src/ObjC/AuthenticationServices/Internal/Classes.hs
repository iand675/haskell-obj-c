{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AuthenticationServices.Internal.Classes (
    module ObjC.AuthenticationServices.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- ASAccountAuthenticationModificationController ----------

-- | Phantom type for @ASAccountAuthenticationModificationController@.
data ASAccountAuthenticationModificationController

instance IsObjCObject (Id ASAccountAuthenticationModificationController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAccountAuthenticationModificationController"

class IsNSObject a => IsASAccountAuthenticationModificationController a where
  toASAccountAuthenticationModificationController :: a -> Id ASAccountAuthenticationModificationController

instance IsASAccountAuthenticationModificationController (Id ASAccountAuthenticationModificationController) where
  toASAccountAuthenticationModificationController = unsafeCastId

instance IsNSObject (Id ASAccountAuthenticationModificationController) where
  toNSObject = unsafeCastId

-- ---------- ASAccountAuthenticationModificationRequest ----------

-- | Phantom type for @ASAccountAuthenticationModificationRequest@.
data ASAccountAuthenticationModificationRequest

instance IsObjCObject (Id ASAccountAuthenticationModificationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAccountAuthenticationModificationRequest"

class IsNSObject a => IsASAccountAuthenticationModificationRequest a where
  toASAccountAuthenticationModificationRequest :: a -> Id ASAccountAuthenticationModificationRequest

instance IsASAccountAuthenticationModificationRequest (Id ASAccountAuthenticationModificationRequest) where
  toASAccountAuthenticationModificationRequest = unsafeCastId

instance IsNSObject (Id ASAccountAuthenticationModificationRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorization ----------

-- | Phantom type for @ASAuthorization@.
data ASAuthorization

instance IsObjCObject (Id ASAuthorization) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorization"

class IsNSObject a => IsASAuthorization a where
  toASAuthorization :: a -> Id ASAuthorization

instance IsASAuthorization (Id ASAuthorization) where
  toASAuthorization = unsafeCastId

instance IsNSObject (Id ASAuthorization) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationAppleIDCredential ----------

-- | Phantom type for @ASAuthorizationAppleIDCredential@.
data ASAuthorizationAppleIDCredential

instance IsObjCObject (Id ASAuthorizationAppleIDCredential) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationAppleIDCredential"

class IsNSObject a => IsASAuthorizationAppleIDCredential a where
  toASAuthorizationAppleIDCredential :: a -> Id ASAuthorizationAppleIDCredential

instance IsASAuthorizationAppleIDCredential (Id ASAuthorizationAppleIDCredential) where
  toASAuthorizationAppleIDCredential = unsafeCastId

instance IsNSObject (Id ASAuthorizationAppleIDCredential) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationAppleIDProvider ----------

-- | Phantom type for @ASAuthorizationAppleIDProvider@.
data ASAuthorizationAppleIDProvider

instance IsObjCObject (Id ASAuthorizationAppleIDProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationAppleIDProvider"

class IsNSObject a => IsASAuthorizationAppleIDProvider a where
  toASAuthorizationAppleIDProvider :: a -> Id ASAuthorizationAppleIDProvider

instance IsASAuthorizationAppleIDProvider (Id ASAuthorizationAppleIDProvider) where
  toASAuthorizationAppleIDProvider = unsafeCastId

instance IsNSObject (Id ASAuthorizationAppleIDProvider) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationController ----------

-- | Phantom type for @ASAuthorizationController@.
data ASAuthorizationController

instance IsObjCObject (Id ASAuthorizationController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationController"

class IsNSObject a => IsASAuthorizationController a where
  toASAuthorizationController :: a -> Id ASAuthorizationController

instance IsASAuthorizationController (Id ASAuthorizationController) where
  toASAuthorizationController = unsafeCastId

instance IsNSObject (Id ASAuthorizationController) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPasswordProvider ----------

-- | Phantom type for @ASAuthorizationPasswordProvider@.
data ASAuthorizationPasswordProvider

instance IsObjCObject (Id ASAuthorizationPasswordProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPasswordProvider"

class IsNSObject a => IsASAuthorizationPasswordProvider a where
  toASAuthorizationPasswordProvider :: a -> Id ASAuthorizationPasswordProvider

instance IsASAuthorizationPasswordProvider (Id ASAuthorizationPasswordProvider) where
  toASAuthorizationPasswordProvider = unsafeCastId

instance IsNSObject (Id ASAuthorizationPasswordProvider) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPlatformPublicKeyCredentialAssertion ----------

-- | Phantom type for @ASAuthorizationPlatformPublicKeyCredentialAssertion@.
data ASAuthorizationPlatformPublicKeyCredentialAssertion

instance IsObjCObject (Id ASAuthorizationPlatformPublicKeyCredentialAssertion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialAssertion"

class IsNSObject a => IsASAuthorizationPlatformPublicKeyCredentialAssertion a where
  toASAuthorizationPlatformPublicKeyCredentialAssertion :: a -> Id ASAuthorizationPlatformPublicKeyCredentialAssertion

instance IsASAuthorizationPlatformPublicKeyCredentialAssertion (Id ASAuthorizationPlatformPublicKeyCredentialAssertion) where
  toASAuthorizationPlatformPublicKeyCredentialAssertion = unsafeCastId

instance IsNSObject (Id ASAuthorizationPlatformPublicKeyCredentialAssertion) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPlatformPublicKeyCredentialDescriptor ----------

-- | Phantom type for @ASAuthorizationPlatformPublicKeyCredentialDescriptor@.
data ASAuthorizationPlatformPublicKeyCredentialDescriptor

instance IsObjCObject (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialDescriptor"

class IsNSObject a => IsASAuthorizationPlatformPublicKeyCredentialDescriptor a where
  toASAuthorizationPlatformPublicKeyCredentialDescriptor :: a -> Id ASAuthorizationPlatformPublicKeyCredentialDescriptor

instance IsASAuthorizationPlatformPublicKeyCredentialDescriptor (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor) where
  toASAuthorizationPlatformPublicKeyCredentialDescriptor = unsafeCastId

instance IsNSObject (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPlatformPublicKeyCredentialProvider ----------

-- | Phantom type for @ASAuthorizationPlatformPublicKeyCredentialProvider@.
data ASAuthorizationPlatformPublicKeyCredentialProvider

instance IsObjCObject (Id ASAuthorizationPlatformPublicKeyCredentialProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialProvider"

class IsNSObject a => IsASAuthorizationPlatformPublicKeyCredentialProvider a where
  toASAuthorizationPlatformPublicKeyCredentialProvider :: a -> Id ASAuthorizationPlatformPublicKeyCredentialProvider

instance IsASAuthorizationPlatformPublicKeyCredentialProvider (Id ASAuthorizationPlatformPublicKeyCredentialProvider) where
  toASAuthorizationPlatformPublicKeyCredentialProvider = unsafeCastId

instance IsNSObject (Id ASAuthorizationPlatformPublicKeyCredentialProvider) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPlatformPublicKeyCredentialRegistration ----------

-- | Phantom type for @ASAuthorizationPlatformPublicKeyCredentialRegistration@.
data ASAuthorizationPlatformPublicKeyCredentialRegistration

instance IsObjCObject (Id ASAuthorizationPlatformPublicKeyCredentialRegistration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialRegistration"

class IsNSObject a => IsASAuthorizationPlatformPublicKeyCredentialRegistration a where
  toASAuthorizationPlatformPublicKeyCredentialRegistration :: a -> Id ASAuthorizationPlatformPublicKeyCredentialRegistration

instance IsASAuthorizationPlatformPublicKeyCredentialRegistration (Id ASAuthorizationPlatformPublicKeyCredentialRegistration) where
  toASAuthorizationPlatformPublicKeyCredentialRegistration = unsafeCastId

instance IsNSObject (Id ASAuthorizationPlatformPublicKeyCredentialRegistration) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationProviderExtensionAuthorizationRequest ----------

-- | Phantom type for @ASAuthorizationProviderExtensionAuthorizationRequest@.
data ASAuthorizationProviderExtensionAuthorizationRequest

instance IsObjCObject (Id ASAuthorizationProviderExtensionAuthorizationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationProviderExtensionAuthorizationRequest"

class IsNSObject a => IsASAuthorizationProviderExtensionAuthorizationRequest a where
  toASAuthorizationProviderExtensionAuthorizationRequest :: a -> Id ASAuthorizationProviderExtensionAuthorizationRequest

instance IsASAuthorizationProviderExtensionAuthorizationRequest (Id ASAuthorizationProviderExtensionAuthorizationRequest) where
  toASAuthorizationProviderExtensionAuthorizationRequest = unsafeCastId

instance IsNSObject (Id ASAuthorizationProviderExtensionAuthorizationRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationProviderExtensionAuthorizationResult ----------

-- | Phantom type for @ASAuthorizationProviderExtensionAuthorizationResult@.
data ASAuthorizationProviderExtensionAuthorizationResult

instance IsObjCObject (Id ASAuthorizationProviderExtensionAuthorizationResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationProviderExtensionAuthorizationResult"

class IsNSObject a => IsASAuthorizationProviderExtensionAuthorizationResult a where
  toASAuthorizationProviderExtensionAuthorizationResult :: a -> Id ASAuthorizationProviderExtensionAuthorizationResult

instance IsASAuthorizationProviderExtensionAuthorizationResult (Id ASAuthorizationProviderExtensionAuthorizationResult) where
  toASAuthorizationProviderExtensionAuthorizationResult = unsafeCastId

instance IsNSObject (Id ASAuthorizationProviderExtensionAuthorizationResult) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationProviderExtensionKerberosMapping ----------

-- | Phantom type for @ASAuthorizationProviderExtensionKerberosMapping@.
data ASAuthorizationProviderExtensionKerberosMapping

instance IsObjCObject (Id ASAuthorizationProviderExtensionKerberosMapping) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationProviderExtensionKerberosMapping"

class IsNSObject a => IsASAuthorizationProviderExtensionKerberosMapping a where
  toASAuthorizationProviderExtensionKerberosMapping :: a -> Id ASAuthorizationProviderExtensionKerberosMapping

instance IsASAuthorizationProviderExtensionKerberosMapping (Id ASAuthorizationProviderExtensionKerberosMapping) where
  toASAuthorizationProviderExtensionKerberosMapping = unsafeCastId

instance IsNSObject (Id ASAuthorizationProviderExtensionKerberosMapping) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationProviderExtensionLoginConfiguration ----------

-- | Phantom type for @ASAuthorizationProviderExtensionLoginConfiguration@.
data ASAuthorizationProviderExtensionLoginConfiguration

instance IsObjCObject (Id ASAuthorizationProviderExtensionLoginConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationProviderExtensionLoginConfiguration"

class IsNSObject a => IsASAuthorizationProviderExtensionLoginConfiguration a where
  toASAuthorizationProviderExtensionLoginConfiguration :: a -> Id ASAuthorizationProviderExtensionLoginConfiguration

instance IsASAuthorizationProviderExtensionLoginConfiguration (Id ASAuthorizationProviderExtensionLoginConfiguration) where
  toASAuthorizationProviderExtensionLoginConfiguration = unsafeCastId

instance IsNSObject (Id ASAuthorizationProviderExtensionLoginConfiguration) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationProviderExtensionLoginManager ----------

-- | Phantom type for @ASAuthorizationProviderExtensionLoginManager@.
data ASAuthorizationProviderExtensionLoginManager

instance IsObjCObject (Id ASAuthorizationProviderExtensionLoginManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationProviderExtensionLoginManager"

class IsNSObject a => IsASAuthorizationProviderExtensionLoginManager a where
  toASAuthorizationProviderExtensionLoginManager :: a -> Id ASAuthorizationProviderExtensionLoginManager

instance IsASAuthorizationProviderExtensionLoginManager (Id ASAuthorizationProviderExtensionLoginManager) where
  toASAuthorizationProviderExtensionLoginManager = unsafeCastId

instance IsNSObject (Id ASAuthorizationProviderExtensionLoginManager) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationProviderExtensionUserLoginConfiguration ----------

-- | Phantom type for @ASAuthorizationProviderExtensionUserLoginConfiguration@.
data ASAuthorizationProviderExtensionUserLoginConfiguration

instance IsObjCObject (Id ASAuthorizationProviderExtensionUserLoginConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationProviderExtensionUserLoginConfiguration"

class IsNSObject a => IsASAuthorizationProviderExtensionUserLoginConfiguration a where
  toASAuthorizationProviderExtensionUserLoginConfiguration :: a -> Id ASAuthorizationProviderExtensionUserLoginConfiguration

instance IsASAuthorizationProviderExtensionUserLoginConfiguration (Id ASAuthorizationProviderExtensionUserLoginConfiguration) where
  toASAuthorizationProviderExtensionUserLoginConfiguration = unsafeCastId

instance IsNSObject (Id ASAuthorizationProviderExtensionUserLoginConfiguration) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput ----------

-- | Phantom type for @ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput@.
data ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput

instance IsObjCObject (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput"

class IsNSObject a => IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput a where
  toASAuthorizationPublicKeyCredentialLargeBlobAssertionInput :: a -> Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput

instance IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput) where
  toASAuthorizationPublicKeyCredentialLargeBlobAssertionInput = unsafeCastId

instance IsNSObject (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput ----------

-- | Phantom type for @ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput@.
data ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput

instance IsObjCObject (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput"

class IsNSObject a => IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput a where
  toASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput :: a -> Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput

instance IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput) where
  toASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput = unsafeCastId

instance IsNSObject (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput ----------

-- | Phantom type for @ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput@.
data ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput

instance IsObjCObject (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput"

class IsNSObject a => IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput a where
  toASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput :: a -> Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput

instance IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput) where
  toASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput = unsafeCastId

instance IsNSObject (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput ----------

-- | Phantom type for @ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput@.
data ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput

instance IsObjCObject (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput"

class IsNSObject a => IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput a where
  toASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput :: a -> Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput

instance IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput) where
  toASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput = unsafeCastId

instance IsNSObject (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPublicKeyCredentialPRFAssertionInput ----------

-- | Phantom type for @ASAuthorizationPublicKeyCredentialPRFAssertionInput@.
data ASAuthorizationPublicKeyCredentialPRFAssertionInput

instance IsObjCObject (Id ASAuthorizationPublicKeyCredentialPRFAssertionInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPublicKeyCredentialPRFAssertionInput"

class IsNSObject a => IsASAuthorizationPublicKeyCredentialPRFAssertionInput a where
  toASAuthorizationPublicKeyCredentialPRFAssertionInput :: a -> Id ASAuthorizationPublicKeyCredentialPRFAssertionInput

instance IsASAuthorizationPublicKeyCredentialPRFAssertionInput (Id ASAuthorizationPublicKeyCredentialPRFAssertionInput) where
  toASAuthorizationPublicKeyCredentialPRFAssertionInput = unsafeCastId

instance IsNSObject (Id ASAuthorizationPublicKeyCredentialPRFAssertionInput) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPublicKeyCredentialPRFAssertionInputValues ----------

-- | Phantom type for @ASAuthorizationPublicKeyCredentialPRFAssertionInputValues@.
data ASAuthorizationPublicKeyCredentialPRFAssertionInputValues

instance IsObjCObject (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPublicKeyCredentialPRFAssertionInputValues"

class IsNSObject a => IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues a where
  toASAuthorizationPublicKeyCredentialPRFAssertionInputValues :: a -> Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues

instance IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues) where
  toASAuthorizationPublicKeyCredentialPRFAssertionInputValues = unsafeCastId

instance IsNSObject (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPublicKeyCredentialPRFAssertionOutput ----------

-- | Phantom type for @ASAuthorizationPublicKeyCredentialPRFAssertionOutput@.
data ASAuthorizationPublicKeyCredentialPRFAssertionOutput

instance IsObjCObject (Id ASAuthorizationPublicKeyCredentialPRFAssertionOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPublicKeyCredentialPRFAssertionOutput"

class IsNSObject a => IsASAuthorizationPublicKeyCredentialPRFAssertionOutput a where
  toASAuthorizationPublicKeyCredentialPRFAssertionOutput :: a -> Id ASAuthorizationPublicKeyCredentialPRFAssertionOutput

instance IsASAuthorizationPublicKeyCredentialPRFAssertionOutput (Id ASAuthorizationPublicKeyCredentialPRFAssertionOutput) where
  toASAuthorizationPublicKeyCredentialPRFAssertionOutput = unsafeCastId

instance IsNSObject (Id ASAuthorizationPublicKeyCredentialPRFAssertionOutput) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPublicKeyCredentialPRFRegistrationInput ----------

-- | Phantom type for @ASAuthorizationPublicKeyCredentialPRFRegistrationInput@.
data ASAuthorizationPublicKeyCredentialPRFRegistrationInput

instance IsObjCObject (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPublicKeyCredentialPRFRegistrationInput"

class IsNSObject a => IsASAuthorizationPublicKeyCredentialPRFRegistrationInput a where
  toASAuthorizationPublicKeyCredentialPRFRegistrationInput :: a -> Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput

instance IsASAuthorizationPublicKeyCredentialPRFRegistrationInput (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput) where
  toASAuthorizationPublicKeyCredentialPRFRegistrationInput = unsafeCastId

instance IsNSObject (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPublicKeyCredentialPRFRegistrationOutput ----------

-- | Phantom type for @ASAuthorizationPublicKeyCredentialPRFRegistrationOutput@.
data ASAuthorizationPublicKeyCredentialPRFRegistrationOutput

instance IsObjCObject (Id ASAuthorizationPublicKeyCredentialPRFRegistrationOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPublicKeyCredentialPRFRegistrationOutput"

class IsNSObject a => IsASAuthorizationPublicKeyCredentialPRFRegistrationOutput a where
  toASAuthorizationPublicKeyCredentialPRFRegistrationOutput :: a -> Id ASAuthorizationPublicKeyCredentialPRFRegistrationOutput

instance IsASAuthorizationPublicKeyCredentialPRFRegistrationOutput (Id ASAuthorizationPublicKeyCredentialPRFRegistrationOutput) where
  toASAuthorizationPublicKeyCredentialPRFRegistrationOutput = unsafeCastId

instance IsNSObject (Id ASAuthorizationPublicKeyCredentialPRFRegistrationOutput) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPublicKeyCredentialParameters ----------

-- | Phantom type for @ASAuthorizationPublicKeyCredentialParameters@.
data ASAuthorizationPublicKeyCredentialParameters

instance IsObjCObject (Id ASAuthorizationPublicKeyCredentialParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPublicKeyCredentialParameters"

class IsNSObject a => IsASAuthorizationPublicKeyCredentialParameters a where
  toASAuthorizationPublicKeyCredentialParameters :: a -> Id ASAuthorizationPublicKeyCredentialParameters

instance IsASAuthorizationPublicKeyCredentialParameters (Id ASAuthorizationPublicKeyCredentialParameters) where
  toASAuthorizationPublicKeyCredentialParameters = unsafeCastId

instance IsNSObject (Id ASAuthorizationPublicKeyCredentialParameters) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationRequest ----------

-- | Phantom type for @ASAuthorizationRequest@.
data ASAuthorizationRequest

instance IsObjCObject (Id ASAuthorizationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationRequest"

class IsNSObject a => IsASAuthorizationRequest a where
  toASAuthorizationRequest :: a -> Id ASAuthorizationRequest

instance IsASAuthorizationRequest (Id ASAuthorizationRequest) where
  toASAuthorizationRequest = unsafeCastId

instance IsNSObject (Id ASAuthorizationRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationSecurityKeyPublicKeyCredentialAssertion ----------

-- | Phantom type for @ASAuthorizationSecurityKeyPublicKeyCredentialAssertion@.
data ASAuthorizationSecurityKeyPublicKeyCredentialAssertion

instance IsObjCObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialAssertion"

class IsNSObject a => IsASAuthorizationSecurityKeyPublicKeyCredentialAssertion a where
  toASAuthorizationSecurityKeyPublicKeyCredentialAssertion :: a -> Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertion

instance IsASAuthorizationSecurityKeyPublicKeyCredentialAssertion (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertion) where
  toASAuthorizationSecurityKeyPublicKeyCredentialAssertion = unsafeCastId

instance IsNSObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertion) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor ----------

-- | An object to describe a credential on a security key.
-- 
-- Phantom type for @ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor@.
data ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor

instance IsObjCObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor"

class IsNSObject a => IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor a where
  toASAuthorizationSecurityKeyPublicKeyCredentialDescriptor :: a -> Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor

instance IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor) where
  toASAuthorizationSecurityKeyPublicKeyCredentialDescriptor = unsafeCastId

instance IsNSObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationSecurityKeyPublicKeyCredentialProvider ----------

-- | Phantom type for @ASAuthorizationSecurityKeyPublicKeyCredentialProvider@.
data ASAuthorizationSecurityKeyPublicKeyCredentialProvider

instance IsObjCObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialProvider"

class IsNSObject a => IsASAuthorizationSecurityKeyPublicKeyCredentialProvider a where
  toASAuthorizationSecurityKeyPublicKeyCredentialProvider :: a -> Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider

instance IsASAuthorizationSecurityKeyPublicKeyCredentialProvider (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider) where
  toASAuthorizationSecurityKeyPublicKeyCredentialProvider = unsafeCastId

instance IsNSObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationSecurityKeyPublicKeyCredentialRegistration ----------

-- | Phantom type for @ASAuthorizationSecurityKeyPublicKeyCredentialRegistration@.
data ASAuthorizationSecurityKeyPublicKeyCredentialRegistration

instance IsObjCObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialRegistration"

class IsNSObject a => IsASAuthorizationSecurityKeyPublicKeyCredentialRegistration a where
  toASAuthorizationSecurityKeyPublicKeyCredentialRegistration :: a -> Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistration

instance IsASAuthorizationSecurityKeyPublicKeyCredentialRegistration (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistration) where
  toASAuthorizationSecurityKeyPublicKeyCredentialRegistration = unsafeCastId

instance IsNSObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistration) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationSingleSignOnCredential ----------

-- | Phantom type for @ASAuthorizationSingleSignOnCredential@.
data ASAuthorizationSingleSignOnCredential

instance IsObjCObject (Id ASAuthorizationSingleSignOnCredential) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationSingleSignOnCredential"

class IsNSObject a => IsASAuthorizationSingleSignOnCredential a where
  toASAuthorizationSingleSignOnCredential :: a -> Id ASAuthorizationSingleSignOnCredential

instance IsASAuthorizationSingleSignOnCredential (Id ASAuthorizationSingleSignOnCredential) where
  toASAuthorizationSingleSignOnCredential = unsafeCastId

instance IsNSObject (Id ASAuthorizationSingleSignOnCredential) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationSingleSignOnProvider ----------

-- | Phantom type for @ASAuthorizationSingleSignOnProvider@.
data ASAuthorizationSingleSignOnProvider

instance IsObjCObject (Id ASAuthorizationSingleSignOnProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationSingleSignOnProvider"

class IsNSObject a => IsASAuthorizationSingleSignOnProvider a where
  toASAuthorizationSingleSignOnProvider :: a -> Id ASAuthorizationSingleSignOnProvider

instance IsASAuthorizationSingleSignOnProvider (Id ASAuthorizationSingleSignOnProvider) where
  toASAuthorizationSingleSignOnProvider = unsafeCastId

instance IsNSObject (Id ASAuthorizationSingleSignOnProvider) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationWebBrowserPlatformPublicKeyCredential ----------

-- | Phantom type for @ASAuthorizationWebBrowserPlatformPublicKeyCredential@.
data ASAuthorizationWebBrowserPlatformPublicKeyCredential

instance IsObjCObject (Id ASAuthorizationWebBrowserPlatformPublicKeyCredential) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationWebBrowserPlatformPublicKeyCredential"

class IsNSObject a => IsASAuthorizationWebBrowserPlatformPublicKeyCredential a where
  toASAuthorizationWebBrowserPlatformPublicKeyCredential :: a -> Id ASAuthorizationWebBrowserPlatformPublicKeyCredential

instance IsASAuthorizationWebBrowserPlatformPublicKeyCredential (Id ASAuthorizationWebBrowserPlatformPublicKeyCredential) where
  toASAuthorizationWebBrowserPlatformPublicKeyCredential = unsafeCastId

instance IsNSObject (Id ASAuthorizationWebBrowserPlatformPublicKeyCredential) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationWebBrowserPublicKeyCredentialManager ----------

-- | Phantom type for @ASAuthorizationWebBrowserPublicKeyCredentialManager@.
data ASAuthorizationWebBrowserPublicKeyCredentialManager

instance IsObjCObject (Id ASAuthorizationWebBrowserPublicKeyCredentialManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationWebBrowserPublicKeyCredentialManager"

class IsNSObject a => IsASAuthorizationWebBrowserPublicKeyCredentialManager a where
  toASAuthorizationWebBrowserPublicKeyCredentialManager :: a -> Id ASAuthorizationWebBrowserPublicKeyCredentialManager

instance IsASAuthorizationWebBrowserPublicKeyCredentialManager (Id ASAuthorizationWebBrowserPublicKeyCredentialManager) where
  toASAuthorizationWebBrowserPublicKeyCredentialManager = unsafeCastId

instance IsNSObject (Id ASAuthorizationWebBrowserPublicKeyCredentialManager) where
  toNSObject = unsafeCastId

-- ---------- ASCredentialIdentityStore ----------

-- | Phantom type for @ASCredentialIdentityStore@.
data ASCredentialIdentityStore

instance IsObjCObject (Id ASCredentialIdentityStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASCredentialIdentityStore"

class IsNSObject a => IsASCredentialIdentityStore a where
  toASCredentialIdentityStore :: a -> Id ASCredentialIdentityStore

instance IsASCredentialIdentityStore (Id ASCredentialIdentityStore) where
  toASCredentialIdentityStore = unsafeCastId

instance IsNSObject (Id ASCredentialIdentityStore) where
  toNSObject = unsafeCastId

-- ---------- ASCredentialIdentityStoreState ----------

-- | Phantom type for @ASCredentialIdentityStoreState@.
data ASCredentialIdentityStoreState

instance IsObjCObject (Id ASCredentialIdentityStoreState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASCredentialIdentityStoreState"

class IsNSObject a => IsASCredentialIdentityStoreState a where
  toASCredentialIdentityStoreState :: a -> Id ASCredentialIdentityStoreState

instance IsASCredentialIdentityStoreState (Id ASCredentialIdentityStoreState) where
  toASCredentialIdentityStoreState = unsafeCastId

instance IsNSObject (Id ASCredentialIdentityStoreState) where
  toNSObject = unsafeCastId

-- ---------- ASCredentialServiceIdentifier ----------

-- | Phantom type for @ASCredentialServiceIdentifier@.
data ASCredentialServiceIdentifier

instance IsObjCObject (Id ASCredentialServiceIdentifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASCredentialServiceIdentifier"

class IsNSObject a => IsASCredentialServiceIdentifier a where
  toASCredentialServiceIdentifier :: a -> Id ASCredentialServiceIdentifier

instance IsASCredentialServiceIdentifier (Id ASCredentialServiceIdentifier) where
  toASCredentialServiceIdentifier = unsafeCastId

instance IsNSObject (Id ASCredentialServiceIdentifier) where
  toNSObject = unsafeCastId

-- ---------- ASGeneratePasswordsRequest ----------

-- | Phantom type for @ASGeneratePasswordsRequest@.
data ASGeneratePasswordsRequest

instance IsObjCObject (Id ASGeneratePasswordsRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASGeneratePasswordsRequest"

class IsNSObject a => IsASGeneratePasswordsRequest a where
  toASGeneratePasswordsRequest :: a -> Id ASGeneratePasswordsRequest

instance IsASGeneratePasswordsRequest (Id ASGeneratePasswordsRequest) where
  toASGeneratePasswordsRequest = unsafeCastId

instance IsNSObject (Id ASGeneratePasswordsRequest) where
  toNSObject = unsafeCastId

-- ---------- ASGeneratedPassword ----------

-- | Phantom type for @ASGeneratedPassword@.
data ASGeneratedPassword

instance IsObjCObject (Id ASGeneratedPassword) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASGeneratedPassword"

class IsNSObject a => IsASGeneratedPassword a where
  toASGeneratedPassword :: a -> Id ASGeneratedPassword

instance IsASGeneratedPassword (Id ASGeneratedPassword) where
  toASGeneratedPassword = unsafeCastId

instance IsNSObject (Id ASGeneratedPassword) where
  toNSObject = unsafeCastId

-- ---------- ASOneTimeCodeCredential ----------

-- | Phantom type for @ASOneTimeCodeCredential@.
data ASOneTimeCodeCredential

instance IsObjCObject (Id ASOneTimeCodeCredential) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASOneTimeCodeCredential"

class IsNSObject a => IsASOneTimeCodeCredential a where
  toASOneTimeCodeCredential :: a -> Id ASOneTimeCodeCredential

instance IsASOneTimeCodeCredential (Id ASOneTimeCodeCredential) where
  toASOneTimeCodeCredential = unsafeCastId

instance IsNSObject (Id ASOneTimeCodeCredential) where
  toNSObject = unsafeCastId

-- ---------- ASOneTimeCodeCredentialIdentity ----------

-- | ASOneTimeCodeCredentialIdentity
--
-- An ASOneTimeCodeCredentialIdentity is used to describe an identity that can use a service upon successful one time code based authentication. Use this class to save entries into ASCredentialIdentityStore.
-- 
-- Phantom type for @ASOneTimeCodeCredentialIdentity@.
data ASOneTimeCodeCredentialIdentity

instance IsObjCObject (Id ASOneTimeCodeCredentialIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASOneTimeCodeCredentialIdentity"

class IsNSObject a => IsASOneTimeCodeCredentialIdentity a where
  toASOneTimeCodeCredentialIdentity :: a -> Id ASOneTimeCodeCredentialIdentity

instance IsASOneTimeCodeCredentialIdentity (Id ASOneTimeCodeCredentialIdentity) where
  toASOneTimeCodeCredentialIdentity = unsafeCastId

instance IsNSObject (Id ASOneTimeCodeCredentialIdentity) where
  toNSObject = unsafeCastId

-- ---------- ASOneTimeCodeCredentialRequest ----------

-- | Phantom type for @ASOneTimeCodeCredentialRequest@.
data ASOneTimeCodeCredentialRequest

instance IsObjCObject (Id ASOneTimeCodeCredentialRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASOneTimeCodeCredentialRequest"

class IsNSObject a => IsASOneTimeCodeCredentialRequest a where
  toASOneTimeCodeCredentialRequest :: a -> Id ASOneTimeCodeCredentialRequest

instance IsASOneTimeCodeCredentialRequest (Id ASOneTimeCodeCredentialRequest) where
  toASOneTimeCodeCredentialRequest = unsafeCastId

instance IsNSObject (Id ASOneTimeCodeCredentialRequest) where
  toNSObject = unsafeCastId

-- ---------- ASPasskeyAssertionCredential ----------

-- | This class encapsulates a passkey assertion response created by a credential provider extension.
-- 
-- Phantom type for @ASPasskeyAssertionCredential@.
data ASPasskeyAssertionCredential

instance IsObjCObject (Id ASPasskeyAssertionCredential) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasskeyAssertionCredential"

class IsNSObject a => IsASPasskeyAssertionCredential a where
  toASPasskeyAssertionCredential :: a -> Id ASPasskeyAssertionCredential

instance IsASPasskeyAssertionCredential (Id ASPasskeyAssertionCredential) where
  toASPasskeyAssertionCredential = unsafeCastId

instance IsNSObject (Id ASPasskeyAssertionCredential) where
  toNSObject = unsafeCastId

-- ---------- ASPasskeyAssertionCredentialExtensionInput ----------

-- | This class encapsulates input for various WebAuthn extensions during passkey assertion.
-- 
-- Phantom type for @ASPasskeyAssertionCredentialExtensionInput@.
data ASPasskeyAssertionCredentialExtensionInput

instance IsObjCObject (Id ASPasskeyAssertionCredentialExtensionInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasskeyAssertionCredentialExtensionInput"

class IsNSObject a => IsASPasskeyAssertionCredentialExtensionInput a where
  toASPasskeyAssertionCredentialExtensionInput :: a -> Id ASPasskeyAssertionCredentialExtensionInput

instance IsASPasskeyAssertionCredentialExtensionInput (Id ASPasskeyAssertionCredentialExtensionInput) where
  toASPasskeyAssertionCredentialExtensionInput = unsafeCastId

instance IsNSObject (Id ASPasskeyAssertionCredentialExtensionInput) where
  toNSObject = unsafeCastId

-- ---------- ASPasskeyAssertionCredentialExtensionOutput ----------

-- | This class encapsulates output for various WebAuthn extensions used during passkey assertion.
-- 
-- Phantom type for @ASPasskeyAssertionCredentialExtensionOutput@.
data ASPasskeyAssertionCredentialExtensionOutput

instance IsObjCObject (Id ASPasskeyAssertionCredentialExtensionOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasskeyAssertionCredentialExtensionOutput"

class IsNSObject a => IsASPasskeyAssertionCredentialExtensionOutput a where
  toASPasskeyAssertionCredentialExtensionOutput :: a -> Id ASPasskeyAssertionCredentialExtensionOutput

instance IsASPasskeyAssertionCredentialExtensionOutput (Id ASPasskeyAssertionCredentialExtensionOutput) where
  toASPasskeyAssertionCredentialExtensionOutput = unsafeCastId

instance IsNSObject (Id ASPasskeyAssertionCredentialExtensionOutput) where
  toNSObject = unsafeCastId

-- ---------- ASPasskeyCredentialIdentity ----------

-- | ASPasskeyCredentialIdentity
--
-- An ASPasswordCredentialIdentity is used to describe an identity that can use a service upon successful passkey based authentication. Use this class to save entries into ASCredentialIdentityStore.
-- 
-- Phantom type for @ASPasskeyCredentialIdentity@.
data ASPasskeyCredentialIdentity

instance IsObjCObject (Id ASPasskeyCredentialIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasskeyCredentialIdentity"

class IsNSObject a => IsASPasskeyCredentialIdentity a where
  toASPasskeyCredentialIdentity :: a -> Id ASPasskeyCredentialIdentity

instance IsASPasskeyCredentialIdentity (Id ASPasskeyCredentialIdentity) where
  toASPasskeyCredentialIdentity = unsafeCastId

instance IsNSObject (Id ASPasskeyCredentialIdentity) where
  toNSObject = unsafeCastId

-- ---------- ASPasskeyCredentialRequest ----------

-- | This class encapsulates a passkey assertion request made to a credential provider extension.
-- 
-- Phantom type for @ASPasskeyCredentialRequest@.
data ASPasskeyCredentialRequest

instance IsObjCObject (Id ASPasskeyCredentialRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasskeyCredentialRequest"

class IsNSObject a => IsASPasskeyCredentialRequest a where
  toASPasskeyCredentialRequest :: a -> Id ASPasskeyCredentialRequest

instance IsASPasskeyCredentialRequest (Id ASPasskeyCredentialRequest) where
  toASPasskeyCredentialRequest = unsafeCastId

instance IsNSObject (Id ASPasskeyCredentialRequest) where
  toNSObject = unsafeCastId

-- ---------- ASPasskeyCredentialRequestParameters ----------

-- | A class that holds various parameters related to a passkey credential request.  This class is provided by the system to the credential provider extension when there is an active passkey request as part of  -[ASCredentialProviderViewController prepareCredentialListForServiceIdentifiers:requestParameters:] and should be used  to construct a passkey credential response using the item selected by the user from the extension's UI.
-- 
-- Phantom type for @ASPasskeyCredentialRequestParameters@.
data ASPasskeyCredentialRequestParameters

instance IsObjCObject (Id ASPasskeyCredentialRequestParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasskeyCredentialRequestParameters"

class IsNSObject a => IsASPasskeyCredentialRequestParameters a where
  toASPasskeyCredentialRequestParameters :: a -> Id ASPasskeyCredentialRequestParameters

instance IsASPasskeyCredentialRequestParameters (Id ASPasskeyCredentialRequestParameters) where
  toASPasskeyCredentialRequestParameters = unsafeCastId

instance IsNSObject (Id ASPasskeyCredentialRequestParameters) where
  toNSObject = unsafeCastId

-- ---------- ASPasskeyRegistrationCredential ----------

-- | This class encapsulates a passkey registration response created by a credential provider extension.
-- 
-- Phantom type for @ASPasskeyRegistrationCredential@.
data ASPasskeyRegistrationCredential

instance IsObjCObject (Id ASPasskeyRegistrationCredential) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasskeyRegistrationCredential"

class IsNSObject a => IsASPasskeyRegistrationCredential a where
  toASPasskeyRegistrationCredential :: a -> Id ASPasskeyRegistrationCredential

instance IsASPasskeyRegistrationCredential (Id ASPasskeyRegistrationCredential) where
  toASPasskeyRegistrationCredential = unsafeCastId

instance IsNSObject (Id ASPasskeyRegistrationCredential) where
  toNSObject = unsafeCastId

-- ---------- ASPasskeyRegistrationCredentialExtensionInput ----------

-- | This class encapsulates input for various WebAuthn extensions during passkey registration.
-- 
-- Phantom type for @ASPasskeyRegistrationCredentialExtensionInput@.
data ASPasskeyRegistrationCredentialExtensionInput

instance IsObjCObject (Id ASPasskeyRegistrationCredentialExtensionInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasskeyRegistrationCredentialExtensionInput"

class IsNSObject a => IsASPasskeyRegistrationCredentialExtensionInput a where
  toASPasskeyRegistrationCredentialExtensionInput :: a -> Id ASPasskeyRegistrationCredentialExtensionInput

instance IsASPasskeyRegistrationCredentialExtensionInput (Id ASPasskeyRegistrationCredentialExtensionInput) where
  toASPasskeyRegistrationCredentialExtensionInput = unsafeCastId

instance IsNSObject (Id ASPasskeyRegistrationCredentialExtensionInput) where
  toNSObject = unsafeCastId

-- ---------- ASPasskeyRegistrationCredentialExtensionOutput ----------

-- | This class encapsulates output for various WebAuthn extensions used during passkey registration.
-- 
-- Phantom type for @ASPasskeyRegistrationCredentialExtensionOutput@.
data ASPasskeyRegistrationCredentialExtensionOutput

instance IsObjCObject (Id ASPasskeyRegistrationCredentialExtensionOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasskeyRegistrationCredentialExtensionOutput"

class IsNSObject a => IsASPasskeyRegistrationCredentialExtensionOutput a where
  toASPasskeyRegistrationCredentialExtensionOutput :: a -> Id ASPasskeyRegistrationCredentialExtensionOutput

instance IsASPasskeyRegistrationCredentialExtensionOutput (Id ASPasskeyRegistrationCredentialExtensionOutput) where
  toASPasskeyRegistrationCredentialExtensionOutput = unsafeCastId

instance IsNSObject (Id ASPasskeyRegistrationCredentialExtensionOutput) where
  toNSObject = unsafeCastId

-- ---------- ASPasswordCredential ----------

-- | Phantom type for @ASPasswordCredential@.
data ASPasswordCredential

instance IsObjCObject (Id ASPasswordCredential) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasswordCredential"

class IsNSObject a => IsASPasswordCredential a where
  toASPasswordCredential :: a -> Id ASPasswordCredential

instance IsASPasswordCredential (Id ASPasswordCredential) where
  toASPasswordCredential = unsafeCastId

instance IsNSObject (Id ASPasswordCredential) where
  toNSObject = unsafeCastId

-- ---------- ASPasswordCredentialIdentity ----------

-- | ASPasswordCredentialIdentity
--
-- An ASPasswordCredentialIdentity is used to describe an identity that can use a service upon successful password based authentication. Use this class to save entries into ASCredentialIdentityStore.
-- 
-- Phantom type for @ASPasswordCredentialIdentity@.
data ASPasswordCredentialIdentity

instance IsObjCObject (Id ASPasswordCredentialIdentity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasswordCredentialIdentity"

class IsNSObject a => IsASPasswordCredentialIdentity a where
  toASPasswordCredentialIdentity :: a -> Id ASPasswordCredentialIdentity

instance IsASPasswordCredentialIdentity (Id ASPasswordCredentialIdentity) where
  toASPasswordCredentialIdentity = unsafeCastId

instance IsNSObject (Id ASPasswordCredentialIdentity) where
  toNSObject = unsafeCastId

-- ---------- ASPasswordCredentialRequest ----------

-- | This class encapsulates a password request made to a credential provider extension.
-- 
-- Phantom type for @ASPasswordCredentialRequest@.
data ASPasswordCredentialRequest

instance IsObjCObject (Id ASPasswordCredentialRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPasswordCredentialRequest"

class IsNSObject a => IsASPasswordCredentialRequest a where
  toASPasswordCredentialRequest :: a -> Id ASPasswordCredentialRequest

instance IsASPasswordCredentialRequest (Id ASPasswordCredentialRequest) where
  toASPasswordCredentialRequest = unsafeCastId

instance IsNSObject (Id ASPasswordCredentialRequest) where
  toNSObject = unsafeCastId

-- ---------- ASPublicKeyCredentialClientData ----------

-- | This object represents the client data for a public key credential request, as defined in the WebAuthentication standard.
-- 
-- Phantom type for @ASPublicKeyCredentialClientData@.
data ASPublicKeyCredentialClientData

instance IsObjCObject (Id ASPublicKeyCredentialClientData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPublicKeyCredentialClientData"

class IsNSObject a => IsASPublicKeyCredentialClientData a where
  toASPublicKeyCredentialClientData :: a -> Id ASPublicKeyCredentialClientData

instance IsASPublicKeyCredentialClientData (Id ASPublicKeyCredentialClientData) where
  toASPublicKeyCredentialClientData = unsafeCastId

instance IsNSObject (Id ASPublicKeyCredentialClientData) where
  toNSObject = unsafeCastId

-- ---------- ASSavePasswordRequest ----------

-- | Phantom type for @ASSavePasswordRequest@.
data ASSavePasswordRequest

instance IsObjCObject (Id ASSavePasswordRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASSavePasswordRequest"

class IsNSObject a => IsASSavePasswordRequest a where
  toASSavePasswordRequest :: a -> Id ASSavePasswordRequest

instance IsASSavePasswordRequest (Id ASSavePasswordRequest) where
  toASSavePasswordRequest = unsafeCastId

instance IsNSObject (Id ASSavePasswordRequest) where
  toNSObject = unsafeCastId

-- ---------- ASSettingsHelper ----------

-- | A helper class to provide static utility methods for quick access to settings related to credential providers.
-- 
-- Phantom type for @ASSettingsHelper@.
data ASSettingsHelper

instance IsObjCObject (Id ASSettingsHelper) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASSettingsHelper"

class IsNSObject a => IsASSettingsHelper a where
  toASSettingsHelper :: a -> Id ASSettingsHelper

instance IsASSettingsHelper (Id ASSettingsHelper) where
  toASSettingsHelper = unsafeCastId

instance IsNSObject (Id ASSettingsHelper) where
  toNSObject = unsafeCastId

-- ---------- ASWebAuthenticationSession ----------

-- | ASWebAuthenticationSession
--
-- An ASWebAuthenticationSession object can be used to authenticate a user with a web service, even if the web service is run by a third party. ASWebAuthenticationSession puts the user in control of whether they want to use their existing logged-in session from Safari. The app provides a URL that points to the authentication webpage. The page will be loaded in a secure view controller. From the webpage, the user can authenticate herself and grant access to the app. On completion, the service will send a callback URL with an authentication token, and this URL will be passed to the app by ASWebAuthenticationSessionCompletionHandler.
--
-- The callback URL usually has a custom URL scheme. For the app to receive the callback URL, it needs to either register the custom URL scheme in its Info.plist, or set the scheme to callbackURLScheme argument in the initializer.
--
-- If the user has already logged into the web service in Safari or other apps via ASWebAuthenticationSession, it is possible to share the existing login information. An alert will be presented to get the user's consent for sharing their existing login information. If the user cancels the alert, the session will be canceled, and the completion handler will be called with the error code ASWebAuthenticationSessionErrorCodeCanceledLogin.
--
-- If the user taps Cancel when showing the login webpage for the web service, the session will be canceled, and the completion handler will be called with the error code ASWebAuthenticationSessionErrorCodeCanceledLogin.
--
-- The app can cancel the session by calling -[ASWebAuthenticationSession cancel]. This will also dismiss the view controller that is showing the web service's login page.
-- 
-- Phantom type for @ASWebAuthenticationSession@.
data ASWebAuthenticationSession

instance IsObjCObject (Id ASWebAuthenticationSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASWebAuthenticationSession"

class IsNSObject a => IsASWebAuthenticationSession a where
  toASWebAuthenticationSession :: a -> Id ASWebAuthenticationSession

instance IsASWebAuthenticationSession (Id ASWebAuthenticationSession) where
  toASWebAuthenticationSession = unsafeCastId

instance IsNSObject (Id ASWebAuthenticationSession) where
  toNSObject = unsafeCastId

-- ---------- ASWebAuthenticationSessionCallback ----------

-- | An object used to evaluate navigation events in an authentication session. When the session navigates to a matching URL, it will pass the URL to the session completion handler.
-- 
-- Phantom type for @ASWebAuthenticationSessionCallback@.
data ASWebAuthenticationSessionCallback

instance IsObjCObject (Id ASWebAuthenticationSessionCallback) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASWebAuthenticationSessionCallback"

class IsNSObject a => IsASWebAuthenticationSessionCallback a where
  toASWebAuthenticationSessionCallback :: a -> Id ASWebAuthenticationSessionCallback

instance IsASWebAuthenticationSessionCallback (Id ASWebAuthenticationSessionCallback) where
  toASWebAuthenticationSessionCallback = unsafeCastId

instance IsNSObject (Id ASWebAuthenticationSessionCallback) where
  toNSObject = unsafeCastId

-- ---------- ASWebAuthenticationSessionRequest ----------

-- | Phantom type for @ASWebAuthenticationSessionRequest@.
data ASWebAuthenticationSessionRequest

instance IsObjCObject (Id ASWebAuthenticationSessionRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASWebAuthenticationSessionRequest"

class IsNSObject a => IsASWebAuthenticationSessionRequest a where
  toASWebAuthenticationSessionRequest :: a -> Id ASWebAuthenticationSessionRequest

instance IsASWebAuthenticationSessionRequest (Id ASWebAuthenticationSessionRequest) where
  toASWebAuthenticationSessionRequest = unsafeCastId

instance IsNSObject (Id ASWebAuthenticationSessionRequest) where
  toNSObject = unsafeCastId

-- ---------- ASWebAuthenticationSessionWebBrowserSessionManager ----------

-- | Phantom type for @ASWebAuthenticationSessionWebBrowserSessionManager@.
data ASWebAuthenticationSessionWebBrowserSessionManager

instance IsObjCObject (Id ASWebAuthenticationSessionWebBrowserSessionManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASWebAuthenticationSessionWebBrowserSessionManager"

class IsNSObject a => IsASWebAuthenticationSessionWebBrowserSessionManager a where
  toASWebAuthenticationSessionWebBrowserSessionManager :: a -> Id ASWebAuthenticationSessionWebBrowserSessionManager

instance IsASWebAuthenticationSessionWebBrowserSessionManager (Id ASWebAuthenticationSessionWebBrowserSessionManager) where
  toASWebAuthenticationSessionWebBrowserSessionManager = unsafeCastId

instance IsNSObject (Id ASWebAuthenticationSessionWebBrowserSessionManager) where
  toNSObject = unsafeCastId

-- ---------- ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest ----------

-- | Phantom type for @ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest@.
data ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest

instance IsObjCObject (Id ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest"

class IsASAccountAuthenticationModificationRequest a => IsASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest a where
  toASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest :: a -> Id ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest

instance IsASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest (Id ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest) where
  toASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest = unsafeCastId

instance IsASAccountAuthenticationModificationRequest (Id ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest) where
  toASAccountAuthenticationModificationRequest = unsafeCastId

instance IsNSObject (Id ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest ----------

-- | Phantom type for @ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest@.
data ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest

instance IsObjCObject (Id ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest"

class IsASAccountAuthenticationModificationRequest a => IsASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest a where
  toASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest :: a -> Id ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest

instance IsASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest (Id ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest) where
  toASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest = unsafeCastId

instance IsASAccountAuthenticationModificationRequest (Id ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest) where
  toASAccountAuthenticationModificationRequest = unsafeCastId

instance IsNSObject (Id ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationOpenIDRequest ----------

-- | Phantom type for @ASAuthorizationOpenIDRequest@.
data ASAuthorizationOpenIDRequest

instance IsObjCObject (Id ASAuthorizationOpenIDRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationOpenIDRequest"

class IsASAuthorizationRequest a => IsASAuthorizationOpenIDRequest a where
  toASAuthorizationOpenIDRequest :: a -> Id ASAuthorizationOpenIDRequest

instance IsASAuthorizationOpenIDRequest (Id ASAuthorizationOpenIDRequest) where
  toASAuthorizationOpenIDRequest = unsafeCastId

instance IsASAuthorizationRequest (Id ASAuthorizationOpenIDRequest) where
  toASAuthorizationRequest = unsafeCastId

instance IsNSObject (Id ASAuthorizationOpenIDRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPasswordRequest ----------

-- | Phantom type for @ASAuthorizationPasswordRequest@.
data ASAuthorizationPasswordRequest

instance IsObjCObject (Id ASAuthorizationPasswordRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPasswordRequest"

class IsASAuthorizationRequest a => IsASAuthorizationPasswordRequest a where
  toASAuthorizationPasswordRequest :: a -> Id ASAuthorizationPasswordRequest

instance IsASAuthorizationPasswordRequest (Id ASAuthorizationPasswordRequest) where
  toASAuthorizationPasswordRequest = unsafeCastId

instance IsASAuthorizationRequest (Id ASAuthorizationPasswordRequest) where
  toASAuthorizationRequest = unsafeCastId

instance IsNSObject (Id ASAuthorizationPasswordRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPlatformPublicKeyCredentialAssertionRequest ----------

-- | Phantom type for @ASAuthorizationPlatformPublicKeyCredentialAssertionRequest@.
data ASAuthorizationPlatformPublicKeyCredentialAssertionRequest

instance IsObjCObject (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialAssertionRequest"

class IsASAuthorizationRequest a => IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest a where
  toASAuthorizationPlatformPublicKeyCredentialAssertionRequest :: a -> Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest

instance IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest) where
  toASAuthorizationPlatformPublicKeyCredentialAssertionRequest = unsafeCastId

instance IsASAuthorizationRequest (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest) where
  toASAuthorizationRequest = unsafeCastId

instance IsNSObject (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest ----------

-- | Phantom type for @ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest@.
data ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest

instance IsObjCObject (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest"

class IsASAuthorizationRequest a => IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest a where
  toASAuthorizationPlatformPublicKeyCredentialRegistrationRequest :: a -> Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest

instance IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest) where
  toASAuthorizationPlatformPublicKeyCredentialRegistrationRequest = unsafeCastId

instance IsASAuthorizationRequest (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest) where
  toASAuthorizationRequest = unsafeCastId

instance IsNSObject (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest ----------

-- | Phantom type for @ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest@.
data ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest

instance IsObjCObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest"

class IsASAuthorizationRequest a => IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest a where
  toASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest :: a -> Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest

instance IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest) where
  toASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest = unsafeCastId

instance IsASAuthorizationRequest (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest) where
  toASAuthorizationRequest = unsafeCastId

instance IsNSObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest ----------

-- | Phantom type for @ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest@.
data ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest

instance IsObjCObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest"

class IsASAuthorizationRequest a => IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest a where
  toASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest :: a -> Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest

instance IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest) where
  toASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest = unsafeCastId

instance IsASAuthorizationRequest (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest) where
  toASAuthorizationRequest = unsafeCastId

instance IsNSObject (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAccountAuthenticationModificationExtensionContext ----------

-- | Phantom type for @ASAccountAuthenticationModificationExtensionContext@.
data ASAccountAuthenticationModificationExtensionContext

instance IsObjCObject (Id ASAccountAuthenticationModificationExtensionContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAccountAuthenticationModificationExtensionContext"

class IsNSExtensionContext a => IsASAccountAuthenticationModificationExtensionContext a where
  toASAccountAuthenticationModificationExtensionContext :: a -> Id ASAccountAuthenticationModificationExtensionContext

instance IsASAccountAuthenticationModificationExtensionContext (Id ASAccountAuthenticationModificationExtensionContext) where
  toASAccountAuthenticationModificationExtensionContext = unsafeCastId

instance IsNSExtensionContext (Id ASAccountAuthenticationModificationExtensionContext) where
  toNSExtensionContext = unsafeCastId

instance IsNSObject (Id ASAccountAuthenticationModificationExtensionContext) where
  toNSObject = unsafeCastId

-- ---------- ASCredentialProviderExtensionContext ----------

-- | Phantom type for @ASCredentialProviderExtensionContext@.
data ASCredentialProviderExtensionContext

instance IsObjCObject (Id ASCredentialProviderExtensionContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASCredentialProviderExtensionContext"

class IsNSExtensionContext a => IsASCredentialProviderExtensionContext a where
  toASCredentialProviderExtensionContext :: a -> Id ASCredentialProviderExtensionContext

instance IsASCredentialProviderExtensionContext (Id ASCredentialProviderExtensionContext) where
  toASCredentialProviderExtensionContext = unsafeCastId

instance IsNSExtensionContext (Id ASCredentialProviderExtensionContext) where
  toNSExtensionContext = unsafeCastId

instance IsNSObject (Id ASCredentialProviderExtensionContext) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationAppleIDRequest ----------

-- | Phantom type for @ASAuthorizationAppleIDRequest@.
data ASAuthorizationAppleIDRequest

instance IsObjCObject (Id ASAuthorizationAppleIDRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationAppleIDRequest"

class IsASAuthorizationOpenIDRequest a => IsASAuthorizationAppleIDRequest a where
  toASAuthorizationAppleIDRequest :: a -> Id ASAuthorizationAppleIDRequest

instance IsASAuthorizationAppleIDRequest (Id ASAuthorizationAppleIDRequest) where
  toASAuthorizationAppleIDRequest = unsafeCastId

instance IsASAuthorizationOpenIDRequest (Id ASAuthorizationAppleIDRequest) where
  toASAuthorizationOpenIDRequest = unsafeCastId

instance IsASAuthorizationRequest (Id ASAuthorizationAppleIDRequest) where
  toASAuthorizationRequest = unsafeCastId

instance IsNSObject (Id ASAuthorizationAppleIDRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAuthorizationSingleSignOnRequest ----------

-- | Phantom type for @ASAuthorizationSingleSignOnRequest@.
data ASAuthorizationSingleSignOnRequest

instance IsObjCObject (Id ASAuthorizationSingleSignOnRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationSingleSignOnRequest"

class IsASAuthorizationOpenIDRequest a => IsASAuthorizationSingleSignOnRequest a where
  toASAuthorizationSingleSignOnRequest :: a -> Id ASAuthorizationSingleSignOnRequest

instance IsASAuthorizationSingleSignOnRequest (Id ASAuthorizationSingleSignOnRequest) where
  toASAuthorizationSingleSignOnRequest = unsafeCastId

instance IsASAuthorizationOpenIDRequest (Id ASAuthorizationSingleSignOnRequest) where
  toASAuthorizationOpenIDRequest = unsafeCastId

instance IsASAuthorizationRequest (Id ASAuthorizationSingleSignOnRequest) where
  toASAuthorizationRequest = unsafeCastId

instance IsNSObject (Id ASAuthorizationSingleSignOnRequest) where
  toNSObject = unsafeCastId

-- ---------- ASAccountAuthenticationModificationViewController ----------

-- | Phantom type for @ASAccountAuthenticationModificationViewController@.
data ASAccountAuthenticationModificationViewController

instance IsObjCObject (Id ASAccountAuthenticationModificationViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAccountAuthenticationModificationViewController"

class IsNSViewController a => IsASAccountAuthenticationModificationViewController a where
  toASAccountAuthenticationModificationViewController :: a -> Id ASAccountAuthenticationModificationViewController

instance IsASAccountAuthenticationModificationViewController (Id ASAccountAuthenticationModificationViewController) where
  toASAccountAuthenticationModificationViewController = unsafeCastId

instance IsNSObject (Id ASAccountAuthenticationModificationViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id ASAccountAuthenticationModificationViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id ASAccountAuthenticationModificationViewController) where
  toNSViewController = unsafeCastId

-- ---------- ASCredentialProviderViewController ----------

-- | Phantom type for @ASCredentialProviderViewController@.
data ASCredentialProviderViewController

instance IsObjCObject (Id ASCredentialProviderViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASCredentialProviderViewController"

class IsNSViewController a => IsASCredentialProviderViewController a where
  toASCredentialProviderViewController :: a -> Id ASCredentialProviderViewController

instance IsASCredentialProviderViewController (Id ASCredentialProviderViewController) where
  toASCredentialProviderViewController = unsafeCastId

instance IsNSObject (Id ASCredentialProviderViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id ASCredentialProviderViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id ASCredentialProviderViewController) where
  toNSViewController = unsafeCastId

-- ---------- ASAuthorizationAppleIDButton ----------

-- | Phantom type for @ASAuthorizationAppleIDButton@.
data ASAuthorizationAppleIDButton

instance IsObjCObject (Id ASAuthorizationAppleIDButton) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAuthorizationAppleIDButton"

class IsNSControl a => IsASAuthorizationAppleIDButton a where
  toASAuthorizationAppleIDButton :: a -> Id ASAuthorizationAppleIDButton

instance IsASAuthorizationAppleIDButton (Id ASAuthorizationAppleIDButton) where
  toASAuthorizationAppleIDButton = unsafeCastId

instance IsNSControl (Id ASAuthorizationAppleIDButton) where
  toNSControl = unsafeCastId

instance IsNSObject (Id ASAuthorizationAppleIDButton) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id ASAuthorizationAppleIDButton) where
  toNSResponder = unsafeCastId

instance IsNSView (Id ASAuthorizationAppleIDButton) where
  toNSView = unsafeCastId
