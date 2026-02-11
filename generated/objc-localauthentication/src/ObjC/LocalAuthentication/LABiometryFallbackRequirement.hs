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
    sendClassMsg cls' (mkSelector "defaultRequirement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Requires authorization using the device passcode
--
-- Returns: @LABiometryFallbackRequirement@ instance
--
-- ObjC selector: @+ devicePasscodeRequirement@
devicePasscodeRequirement :: IO (Id LABiometryFallbackRequirement)
devicePasscodeRequirement  =
  do
    cls' <- getRequiredClass "LABiometryFallbackRequirement"
    sendClassMsg cls' (mkSelector "devicePasscodeRequirement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultRequirement@
defaultRequirementSelector :: Selector
defaultRequirementSelector = mkSelector "defaultRequirement"

-- | @Selector@ for @devicePasscodeRequirement@
devicePasscodeRequirementSelector :: Selector
devicePasscodeRequirementSelector = mkSelector "devicePasscodeRequirement"

