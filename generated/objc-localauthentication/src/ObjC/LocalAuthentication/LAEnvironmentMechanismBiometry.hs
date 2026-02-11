{-# LANGUAGE PatternSynonyms #-}
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
  , isEnrolledSelector
  , isLockedOutSelector
  , stateHashSelector
  , builtInSensorInaccessibleSelector

  -- * Enum types
  , LABiometryType(LABiometryType)
  , pattern LABiometryTypeNone
  , pattern LABiometryNone
  , pattern LABiometryTypeTouchID
  , pattern LABiometryTypeFaceID
  , pattern LABiometryTypeOpticID

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

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.LocalAuthentication.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Type of biometry supported by the device.
--
-- This property does not indicate whether biometry is available or not. It always reads the type of biometry             supported by device hardware. You should check @isUsable@ property to see if it is available for use.
--
-- ObjC selector: @- biometryType@
biometryType :: IsLAEnvironmentMechanismBiometry laEnvironmentMechanismBiometry => laEnvironmentMechanismBiometry -> IO LABiometryType
biometryType laEnvironmentMechanismBiometry  =
  fmap (coerce :: CLong -> LABiometryType) $ sendMsg laEnvironmentMechanismBiometry (mkSelector "biometryType") retCLong []

-- | Whether the user has enrolled this biometry.
--
-- Even if biometry is enrolled, it does not necessarily mean that it can be used. You should check @isUsable@             property to see if it is available for use.
--
-- ObjC selector: @- isEnrolled@
isEnrolled :: IsLAEnvironmentMechanismBiometry laEnvironmentMechanismBiometry => laEnvironmentMechanismBiometry -> IO Bool
isEnrolled laEnvironmentMechanismBiometry  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg laEnvironmentMechanismBiometry (mkSelector "isEnrolled") retCULong []

-- | Whether biometry is locked out.
--
-- The system might lock the user out of biometry for various reasons. For example, with Face ID, the user is             locked out after 5 failed match attempts in row. To recover from bio lockout, users need to enter their passcode             (e.g. during device ulock).
--
-- ObjC selector: @- isLockedOut@
isLockedOut :: IsLAEnvironmentMechanismBiometry laEnvironmentMechanismBiometry => laEnvironmentMechanismBiometry -> IO Bool
isLockedOut laEnvironmentMechanismBiometry  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg laEnvironmentMechanismBiometry (mkSelector "isLockedOut") retCULong []

-- | The application specific state of the biometric enrollment as returned by @LAContext.domainState.biometry.stateHash@
--
-- This value represents the state of the enrollment and changes whenever the biometric enrollment is changed.             It does not directly map to the enrolled templates, e.g. if a finger is added to Touch ID enrollement and then             removed, the final state would be different.             It also returns different values to different apps to prevent tracking of user identity.
--
-- ObjC selector: @- stateHash@
stateHash :: IsLAEnvironmentMechanismBiometry laEnvironmentMechanismBiometry => laEnvironmentMechanismBiometry -> IO (Id NSData)
stateHash laEnvironmentMechanismBiometry  =
  sendMsg laEnvironmentMechanismBiometry (mkSelector "stateHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether the built in biometric sensor is inaccessible in the current configuration, preventing the use of biometry.
--
-- Currently, the only example of this is a Clamshell Mode on macOS. The user will be not able to use Touch ID             if the MacBook lid is closed while connected to external monitor and keyboard, unless the external keyboard             has Touch ID.
--
-- ObjC selector: @- builtInSensorInaccessible@
builtInSensorInaccessible :: IsLAEnvironmentMechanismBiometry laEnvironmentMechanismBiometry => laEnvironmentMechanismBiometry -> IO Bool
builtInSensorInaccessible laEnvironmentMechanismBiometry  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg laEnvironmentMechanismBiometry (mkSelector "builtInSensorInaccessible") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @biometryType@
biometryTypeSelector :: Selector
biometryTypeSelector = mkSelector "biometryType"

-- | @Selector@ for @isEnrolled@
isEnrolledSelector :: Selector
isEnrolledSelector = mkSelector "isEnrolled"

-- | @Selector@ for @isLockedOut@
isLockedOutSelector :: Selector
isLockedOutSelector = mkSelector "isLockedOut"

-- | @Selector@ for @stateHash@
stateHashSelector :: Selector
stateHashSelector = mkSelector "stateHash"

-- | @Selector@ for @builtInSensorInaccessible@
builtInSensorInaccessibleSelector :: Selector
builtInSensorInaccessibleSelector = mkSelector "builtInSensorInaccessible"

