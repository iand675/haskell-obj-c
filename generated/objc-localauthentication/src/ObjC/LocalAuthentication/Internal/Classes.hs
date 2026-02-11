{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.LocalAuthentication.Internal.Classes (
    module ObjC.LocalAuthentication.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- LAAuthenticationRequirement ----------

-- | Builds requirements that can be used for protecting a @LARight@
-- 
-- Phantom type for @LAAuthenticationRequirement@.
data LAAuthenticationRequirement

instance IsObjCObject (Id LAAuthenticationRequirement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAAuthenticationRequirement"

class IsNSObject a => IsLAAuthenticationRequirement a where
  toLAAuthenticationRequirement :: a -> Id LAAuthenticationRequirement

instance IsLAAuthenticationRequirement (Id LAAuthenticationRequirement) where
  toLAAuthenticationRequirement = unsafeCastId

instance IsNSObject (Id LAAuthenticationRequirement) where
  toNSObject = unsafeCastId

-- ---------- LABiometryFallbackRequirement ----------

-- | Builds authentication requirements that can be used as fallbacks for  biometric authentication
-- 
-- Phantom type for @LABiometryFallbackRequirement@.
data LABiometryFallbackRequirement

instance IsObjCObject (Id LABiometryFallbackRequirement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LABiometryFallbackRequirement"

class IsNSObject a => IsLABiometryFallbackRequirement a where
  toLABiometryFallbackRequirement :: a -> Id LABiometryFallbackRequirement

instance IsLABiometryFallbackRequirement (Id LABiometryFallbackRequirement) where
  toLABiometryFallbackRequirement = unsafeCastId

instance IsNSObject (Id LABiometryFallbackRequirement) where
  toNSObject = unsafeCastId

-- ---------- LAContext ----------

-- | Class that represents an authentication context.
--
-- This context can be used for evaluating policies.
--
-- See: LAPolicy
-- 
-- Phantom type for @LAContext@.
data LAContext

instance IsObjCObject (Id LAContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAContext"

class IsNSObject a => IsLAContext a where
  toLAContext :: a -> Id LAContext

instance IsLAContext (Id LAContext) where
  toLAContext = unsafeCastId

instance IsNSObject (Id LAContext) where
  toNSObject = unsafeCastId

-- ---------- LADomainState ----------

-- | Phantom type for @LADomainState@.
data LADomainState

instance IsObjCObject (Id LADomainState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LADomainState"

class IsNSObject a => IsLADomainState a where
  toLADomainState :: a -> Id LADomainState

instance IsLADomainState (Id LADomainState) where
  toLADomainState = unsafeCastId

instance IsNSObject (Id LADomainState) where
  toNSObject = unsafeCastId

-- ---------- LADomainStateBiometry ----------

-- | Phantom type for @LADomainStateBiometry@.
data LADomainStateBiometry

instance IsObjCObject (Id LADomainStateBiometry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LADomainStateBiometry"

class IsNSObject a => IsLADomainStateBiometry a where
  toLADomainStateBiometry :: a -> Id LADomainStateBiometry

instance IsLADomainStateBiometry (Id LADomainStateBiometry) where
  toLADomainStateBiometry = unsafeCastId

instance IsNSObject (Id LADomainStateBiometry) where
  toNSObject = unsafeCastId

-- ---------- LADomainStateCompanion ----------

-- | Phantom type for @LADomainStateCompanion@.
data LADomainStateCompanion

instance IsObjCObject (Id LADomainStateCompanion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LADomainStateCompanion"

class IsNSObject a => IsLADomainStateCompanion a where
  toLADomainStateCompanion :: a -> Id LADomainStateCompanion

instance IsLADomainStateCompanion (Id LADomainStateCompanion) where
  toLADomainStateCompanion = unsafeCastId

instance IsNSObject (Id LADomainStateCompanion) where
  toNSObject = unsafeCastId

-- ---------- LAEnvironment ----------

-- | Phantom type for @LAEnvironment@.
data LAEnvironment

instance IsObjCObject (Id LAEnvironment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAEnvironment"

class IsNSObject a => IsLAEnvironment a where
  toLAEnvironment :: a -> Id LAEnvironment

instance IsLAEnvironment (Id LAEnvironment) where
  toLAEnvironment = unsafeCastId

instance IsNSObject (Id LAEnvironment) where
  toNSObject = unsafeCastId

-- ---------- LAEnvironmentMechanism ----------

-- | Phantom type for @LAEnvironmentMechanism@.
data LAEnvironmentMechanism

instance IsObjCObject (Id LAEnvironmentMechanism) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAEnvironmentMechanism"

class IsNSObject a => IsLAEnvironmentMechanism a where
  toLAEnvironmentMechanism :: a -> Id LAEnvironmentMechanism

instance IsLAEnvironmentMechanism (Id LAEnvironmentMechanism) where
  toLAEnvironmentMechanism = unsafeCastId

instance IsNSObject (Id LAEnvironmentMechanism) where
  toNSObject = unsafeCastId

-- ---------- LAEnvironmentState ----------

-- | Phantom type for @LAEnvironmentState@.
data LAEnvironmentState

instance IsObjCObject (Id LAEnvironmentState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAEnvironmentState"

class IsNSObject a => IsLAEnvironmentState a where
  toLAEnvironmentState :: a -> Id LAEnvironmentState

instance IsLAEnvironmentState (Id LAEnvironmentState) where
  toLAEnvironmentState = unsafeCastId

instance IsNSObject (Id LAEnvironmentState) where
  toNSObject = unsafeCastId

-- ---------- LAPrivateKey ----------

-- | Managed Private Key.
-- 
-- Phantom type for @LAPrivateKey@.
data LAPrivateKey

instance IsObjCObject (Id LAPrivateKey) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAPrivateKey"

class IsNSObject a => IsLAPrivateKey a where
  toLAPrivateKey :: a -> Id LAPrivateKey

instance IsLAPrivateKey (Id LAPrivateKey) where
  toLAPrivateKey = unsafeCastId

instance IsNSObject (Id LAPrivateKey) where
  toNSObject = unsafeCastId

-- ---------- LAPublicKey ----------

-- | The public part of an asymmetric key pair
-- 
-- Phantom type for @LAPublicKey@.
data LAPublicKey

instance IsObjCObject (Id LAPublicKey) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAPublicKey"

class IsNSObject a => IsLAPublicKey a where
  toLAPublicKey :: a -> Id LAPublicKey

instance IsLAPublicKey (Id LAPublicKey) where
  toLAPublicKey = unsafeCastId

instance IsNSObject (Id LAPublicKey) where
  toNSObject = unsafeCastId

-- ---------- LARight ----------

-- | Groups a set of requirements that need to be satisfied in order to grant access to certain resource or operation
-- 
-- Phantom type for @LARight@.
data LARight

instance IsObjCObject (Id LARight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LARight"

class IsNSObject a => IsLARight a where
  toLARight :: a -> Id LARight

instance IsLARight (Id LARight) where
  toLARight = unsafeCastId

instance IsNSObject (Id LARight) where
  toNSObject = unsafeCastId

-- ---------- LARightStore ----------

-- | Persistent storage for @LARight@ instances.
-- 
-- Phantom type for @LARightStore@.
data LARightStore

instance IsObjCObject (Id LARightStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LARightStore"

class IsNSObject a => IsLARightStore a where
  toLARightStore :: a -> Id LARightStore

instance IsLARightStore (Id LARightStore) where
  toLARightStore = unsafeCastId

instance IsNSObject (Id LARightStore) where
  toNSObject = unsafeCastId

-- ---------- LASecret ----------

-- | Generic secret
-- 
-- Phantom type for @LASecret@.
data LASecret

instance IsObjCObject (Id LASecret) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LASecret"

class IsNSObject a => IsLASecret a where
  toLASecret :: a -> Id LASecret

instance IsLASecret (Id LASecret) where
  toLASecret = unsafeCastId

instance IsNSObject (Id LASecret) where
  toNSObject = unsafeCastId

-- ---------- LAEnvironmentMechanismBiometry ----------

-- | Phantom type for @LAEnvironmentMechanismBiometry@.
data LAEnvironmentMechanismBiometry

instance IsObjCObject (Id LAEnvironmentMechanismBiometry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAEnvironmentMechanismBiometry"

class IsLAEnvironmentMechanism a => IsLAEnvironmentMechanismBiometry a where
  toLAEnvironmentMechanismBiometry :: a -> Id LAEnvironmentMechanismBiometry

instance IsLAEnvironmentMechanismBiometry (Id LAEnvironmentMechanismBiometry) where
  toLAEnvironmentMechanismBiometry = unsafeCastId

instance IsLAEnvironmentMechanism (Id LAEnvironmentMechanismBiometry) where
  toLAEnvironmentMechanism = unsafeCastId

instance IsNSObject (Id LAEnvironmentMechanismBiometry) where
  toNSObject = unsafeCastId

-- ---------- LAEnvironmentMechanismCompanion ----------

-- | Phantom type for @LAEnvironmentMechanismCompanion@.
data LAEnvironmentMechanismCompanion

instance IsObjCObject (Id LAEnvironmentMechanismCompanion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAEnvironmentMechanismCompanion"

class IsLAEnvironmentMechanism a => IsLAEnvironmentMechanismCompanion a where
  toLAEnvironmentMechanismCompanion :: a -> Id LAEnvironmentMechanismCompanion

instance IsLAEnvironmentMechanismCompanion (Id LAEnvironmentMechanismCompanion) where
  toLAEnvironmentMechanismCompanion = unsafeCastId

instance IsLAEnvironmentMechanism (Id LAEnvironmentMechanismCompanion) where
  toLAEnvironmentMechanism = unsafeCastId

instance IsNSObject (Id LAEnvironmentMechanismCompanion) where
  toNSObject = unsafeCastId

-- ---------- LAEnvironmentMechanismUserPassword ----------

-- | Phantom type for @LAEnvironmentMechanismUserPassword@.
data LAEnvironmentMechanismUserPassword

instance IsObjCObject (Id LAEnvironmentMechanismUserPassword) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAEnvironmentMechanismUserPassword"

class IsLAEnvironmentMechanism a => IsLAEnvironmentMechanismUserPassword a where
  toLAEnvironmentMechanismUserPassword :: a -> Id LAEnvironmentMechanismUserPassword

instance IsLAEnvironmentMechanismUserPassword (Id LAEnvironmentMechanismUserPassword) where
  toLAEnvironmentMechanismUserPassword = unsafeCastId

instance IsLAEnvironmentMechanism (Id LAEnvironmentMechanismUserPassword) where
  toLAEnvironmentMechanism = unsafeCastId

instance IsNSObject (Id LAEnvironmentMechanismUserPassword) where
  toNSObject = unsafeCastId

-- ---------- LAPersistedRight ----------

-- | A type of right that, when authorized, grants access to a key and secret
-- 
-- Phantom type for @LAPersistedRight@.
data LAPersistedRight

instance IsObjCObject (Id LAPersistedRight) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "LAPersistedRight"

class IsLARight a => IsLAPersistedRight a where
  toLAPersistedRight :: a -> Id LAPersistedRight

instance IsLAPersistedRight (Id LAPersistedRight) where
  toLAPersistedRight = unsafeCastId

instance IsLARight (Id LAPersistedRight) where
  toLARight = unsafeCastId

instance IsNSObject (Id LAPersistedRight) where
  toNSObject = unsafeCastId
