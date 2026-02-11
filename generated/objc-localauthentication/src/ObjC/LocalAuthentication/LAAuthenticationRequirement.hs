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
  , biometryRequirementWithFallbackSelector
  , defaultRequirementSelector
  , biometryRequirementSelector
  , biometryCurrentSetRequirementSelector


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
    withObjCPtr fallback $ \raw_fallback ->
      sendClassMsg cls' (mkSelector "biometryRequirementWithFallback:") (retPtr retVoid) [argPtr (castPtr raw_fallback :: Ptr ())] >>= retainedObject . castPtr

-- | Requires user authentication
--
-- Returns: @LAAuthenticationRequirement@ instance
--
-- ObjC selector: @+ defaultRequirement@
defaultRequirement :: IO (Id LAAuthenticationRequirement)
defaultRequirement  =
  do
    cls' <- getRequiredClass "LAAuthenticationRequirement"
    sendClassMsg cls' (mkSelector "defaultRequirement") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "biometryRequirement") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "biometryCurrentSetRequirement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @biometryRequirementWithFallback:@
biometryRequirementWithFallbackSelector :: Selector
biometryRequirementWithFallbackSelector = mkSelector "biometryRequirementWithFallback:"

-- | @Selector@ for @defaultRequirement@
defaultRequirementSelector :: Selector
defaultRequirementSelector = mkSelector "defaultRequirement"

-- | @Selector@ for @biometryRequirement@
biometryRequirementSelector :: Selector
biometryRequirementSelector = mkSelector "biometryRequirement"

-- | @Selector@ for @biometryCurrentSetRequirement@
biometryCurrentSetRequirementSelector :: Selector
biometryCurrentSetRequirementSelector = mkSelector "biometryCurrentSetRequirement"

