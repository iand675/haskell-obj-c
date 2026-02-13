{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @LAEnvironmentMechanismBiometry@.
module ObjC.LocalAuthentication.LAEnvironmentMechanismBiometry
  ( LAEnvironmentMechanismBiometry
  , IsLAEnvironmentMechanismBiometry(..)
  , biometryType
  , isEnrolled
  , isLockedOut
  , stateHash
  , builtInSensorInaccessible
  , biometryTypeSelector
  , builtInSensorInaccessibleSelector
  , isEnrolledSelector
  , isLockedOutSelector
  , stateHashSelector

  -- * Enum types
  , LABiometryType(LABiometryType)
  , pattern LABiometryTypeNone
  , pattern LABiometryNone
  , pattern LABiometryTypeTouchID
  , pattern LABiometryTypeFaceID
  , pattern LABiometryTypeOpticID

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.LocalAuthentication.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Type of biometry supported by the device.
--
-- This property does not indicate whether biometry is available or not. It always reads the type of biometry             supported by device hardware. You should check @isUsable@ property to see if it is available for use.
--
-- ObjC selector: @- biometryType@
biometryType :: IsLAEnvironmentMechanismBiometry laEnvironmentMechanismBiometry => laEnvironmentMechanismBiometry -> IO LABiometryType
biometryType laEnvironmentMechanismBiometry =
  sendMessage laEnvironmentMechanismBiometry biometryTypeSelector

-- | Whether the user has enrolled this biometry.
--
-- Even if biometry is enrolled, it does not necessarily mean that it can be used. You should check @isUsable@             property to see if it is available for use.
--
-- ObjC selector: @- isEnrolled@
isEnrolled :: IsLAEnvironmentMechanismBiometry laEnvironmentMechanismBiometry => laEnvironmentMechanismBiometry -> IO Bool
isEnrolled laEnvironmentMechanismBiometry =
  sendMessage laEnvironmentMechanismBiometry isEnrolledSelector

-- | Whether biometry is locked out.
--
-- The system might lock the user out of biometry for various reasons. For example, with Face ID, the user is             locked out after 5 failed match attempts in row. To recover from bio lockout, users need to enter their passcode             (e.g. during device ulock).
--
-- ObjC selector: @- isLockedOut@
isLockedOut :: IsLAEnvironmentMechanismBiometry laEnvironmentMechanismBiometry => laEnvironmentMechanismBiometry -> IO Bool
isLockedOut laEnvironmentMechanismBiometry =
  sendMessage laEnvironmentMechanismBiometry isLockedOutSelector

-- | The application specific state of the biometric enrollment as returned by @LAContext.domainState.biometry.stateHash@
--
-- This value represents the state of the enrollment and changes whenever the biometric enrollment is changed.             It does not directly map to the enrolled templates, e.g. if a finger is added to Touch ID enrollement and then             removed, the final state would be different.             It also returns different values to different apps to prevent tracking of user identity.
--
-- ObjC selector: @- stateHash@
stateHash :: IsLAEnvironmentMechanismBiometry laEnvironmentMechanismBiometry => laEnvironmentMechanismBiometry -> IO (Id NSData)
stateHash laEnvironmentMechanismBiometry =
  sendMessage laEnvironmentMechanismBiometry stateHashSelector

-- | Whether the built in biometric sensor is inaccessible in the current configuration, preventing the use of biometry.
--
-- Currently, the only example of this is a Clamshell Mode on macOS. The user will be not able to use Touch ID             if the MacBook lid is closed while connected to external monitor and keyboard, unless the external keyboard             has Touch ID.
--
-- ObjC selector: @- builtInSensorInaccessible@
builtInSensorInaccessible :: IsLAEnvironmentMechanismBiometry laEnvironmentMechanismBiometry => laEnvironmentMechanismBiometry -> IO Bool
builtInSensorInaccessible laEnvironmentMechanismBiometry =
  sendMessage laEnvironmentMechanismBiometry builtInSensorInaccessibleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @biometryType@
biometryTypeSelector :: Selector '[] LABiometryType
biometryTypeSelector = mkSelector "biometryType"

-- | @Selector@ for @isEnrolled@
isEnrolledSelector :: Selector '[] Bool
isEnrolledSelector = mkSelector "isEnrolled"

-- | @Selector@ for @isLockedOut@
isLockedOutSelector :: Selector '[] Bool
isLockedOutSelector = mkSelector "isLockedOut"

-- | @Selector@ for @stateHash@
stateHashSelector :: Selector '[] (Id NSData)
stateHashSelector = mkSelector "stateHash"

-- | @Selector@ for @builtInSensorInaccessible@
builtInSensorInaccessibleSelector :: Selector '[] Bool
builtInSensorInaccessibleSelector = mkSelector "builtInSensorInaccessible"

