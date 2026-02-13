{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Builds authentication requirements that can be used as fallbacks for  biometric authentication
--
-- Generated bindings for @LABiometryFallbackRequirement@.
module ObjC.LocalAuthentication.LABiometryFallbackRequirement
  ( LABiometryFallbackRequirement
  , IsLABiometryFallbackRequirement(..)
  , defaultRequirement
  , devicePasscodeRequirement
  , defaultRequirementSelector
  , devicePasscodeRequirementSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Use default biometric fallback
--
-- Returns: @LABiometryFallbackRequirement@ instance
--
-- ObjC selector: @+ defaultRequirement@
defaultRequirement :: IO (Id LABiometryFallbackRequirement)
defaultRequirement  =
  do
    cls' <- getRequiredClass "LABiometryFallbackRequirement"
    sendClassMessage cls' defaultRequirementSelector

-- | Requires authorization using the device passcode
--
-- Returns: @LABiometryFallbackRequirement@ instance
--
-- ObjC selector: @+ devicePasscodeRequirement@
devicePasscodeRequirement :: IO (Id LABiometryFallbackRequirement)
devicePasscodeRequirement  =
  do
    cls' <- getRequiredClass "LABiometryFallbackRequirement"
    sendClassMessage cls' devicePasscodeRequirementSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultRequirement@
defaultRequirementSelector :: Selector '[] (Id LABiometryFallbackRequirement)
defaultRequirementSelector = mkSelector "defaultRequirement"

-- | @Selector@ for @devicePasscodeRequirement@
devicePasscodeRequirementSelector :: Selector '[] (Id LABiometryFallbackRequirement)
devicePasscodeRequirementSelector = mkSelector "devicePasscodeRequirement"

