{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Builds requirements that can be used for protecting a @LARight@
--
-- Generated bindings for @LAAuthenticationRequirement@.
module ObjC.LocalAuthentication.LAAuthenticationRequirement
  ( LAAuthenticationRequirement
  , IsLAAuthenticationRequirement(..)
  , biometryRequirementWithFallback
  , defaultRequirement
  , biometryRequirement
  , biometryCurrentSetRequirement
  , biometryCurrentSetRequirementSelector
  , biometryRequirementSelector
  , biometryRequirementWithFallbackSelector
  , defaultRequirementSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Requires biometric authentication or the given fallback method.
--
-- @fallback@ — Fallback used in case biometry authentication fails, is not available or not preferred by the user.
--
-- Returns: @LAAuthenticationRequirement@ instance
--
-- ObjC selector: @+ biometryRequirementWithFallback:@
biometryRequirementWithFallback :: IsLABiometryFallbackRequirement fallback => fallback -> IO (Id LAAuthenticationRequirement)
biometryRequirementWithFallback fallback =
  do
    cls' <- getRequiredClass "LAAuthenticationRequirement"
    sendClassMessage cls' biometryRequirementWithFallbackSelector (toLABiometryFallbackRequirement fallback)

-- | Requires user authentication
--
-- Returns: @LAAuthenticationRequirement@ instance
--
-- ObjC selector: @+ defaultRequirement@
defaultRequirement :: IO (Id LAAuthenticationRequirement)
defaultRequirement  =
  do
    cls' <- getRequiredClass "LAAuthenticationRequirement"
    sendClassMessage cls' defaultRequirementSelector

-- | Requires biometric authentication
--
-- The authorization will fail if:
--
-- • Biometry is not available in the current device
--
-- • There are no biometric enrollments
--
-- Returns: @LAAuthenticationRequirement@ instance
--
-- ObjC selector: @+ biometryRequirement@
biometryRequirement :: IO (Id LAAuthenticationRequirement)
biometryRequirement  =
  do
    cls' <- getRequiredClass "LAAuthenticationRequirement"
    sendClassMessage cls' biometryRequirementSelector

-- | Requires user authentication with the current biometric set
--
-- The authorization will fail if:
--
-- • Biometry is not available in the current device
--
-- • There are no biometric enrollments
--
-- • There is a change in the enrollment database -e.g a new TouchID finger is enrolled.
--
-- Returns: @LAAuthenticationRequirement@ instance
--
-- ObjC selector: @+ biometryCurrentSetRequirement@
biometryCurrentSetRequirement :: IO (Id LAAuthenticationRequirement)
biometryCurrentSetRequirement  =
  do
    cls' <- getRequiredClass "LAAuthenticationRequirement"
    sendClassMessage cls' biometryCurrentSetRequirementSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @biometryRequirementWithFallback:@
biometryRequirementWithFallbackSelector :: Selector '[Id LABiometryFallbackRequirement] (Id LAAuthenticationRequirement)
biometryRequirementWithFallbackSelector = mkSelector "biometryRequirementWithFallback:"

-- | @Selector@ for @defaultRequirement@
defaultRequirementSelector :: Selector '[] (Id LAAuthenticationRequirement)
defaultRequirementSelector = mkSelector "defaultRequirement"

-- | @Selector@ for @biometryRequirement@
biometryRequirementSelector :: Selector '[] (Id LAAuthenticationRequirement)
biometryRequirementSelector = mkSelector "biometryRequirement"

-- | @Selector@ for @biometryCurrentSetRequirement@
biometryCurrentSetRequirementSelector :: Selector '[] (Id LAAuthenticationRequirement)
biometryCurrentSetRequirementSelector = mkSelector "biometryCurrentSetRequirement"

