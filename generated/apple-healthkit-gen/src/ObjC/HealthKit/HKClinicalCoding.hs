{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A clinical coding that represents a medical concept using a standardized coding system.
--
-- A clinical coding pairs a ``system``, an optional ``version``, and a ``code`` which identify a medical concept.
--
-- This model is closely related to the [FHIR Coding model](https://build.fhir.org/datatypes.html#Coding).
--
-- Generated bindings for @HKClinicalCoding@.
module ObjC.HealthKit.HKClinicalCoding
  ( HKClinicalCoding
  , IsHKClinicalCoding(..)
  , init_
  , initWithSystem_version_code
  , system
  , version
  , code
  , codeSelector
  , initSelector
  , initWithSystem_version_codeSelector
  , systemSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKClinicalCoding hkClinicalCoding => hkClinicalCoding -> IO (Id HKClinicalCoding)
init_ hkClinicalCoding =
  sendOwnedMessage hkClinicalCoding initSelector

-- | Creates a clinical coding with the specified system, version, and code.
--
-- @system@ — The string that identifies the coding system, typically a HL7 URL.
--
-- @version@ — The version of the system, if applicable.
--
-- @code@ — The clinical code string that represents the medical concept.
--
-- Use when you need to explicitly construct a coding object to associate a HealthKit concept with a standardized medical code.
--
-- ObjC selector: @- initWithSystem:version:code:@
initWithSystem_version_code :: (IsHKClinicalCoding hkClinicalCoding, IsNSString system, IsNSString version, IsNSString code) => hkClinicalCoding -> system -> version -> code -> IO (Id HKClinicalCoding)
initWithSystem_version_code hkClinicalCoding system version code =
  sendOwnedMessage hkClinicalCoding initWithSystem_version_codeSelector (toNSString system) (toNSString version) (toNSString code)

-- | The string that identifies the coding system that defines this clinical code.
--
-- The system is usually expressed as a URL from the [HL7 Terminology](https://terminology.hl7.org/). For example, the RxNorm, a coding system for medications uses: @http://www.nlm.nih.gov/research/umls/rxnorm@.
--
-- ObjC selector: @- system@
system :: IsHKClinicalCoding hkClinicalCoding => hkClinicalCoding -> IO (Id NSString)
system hkClinicalCoding =
  sendMessage hkClinicalCoding systemSelector

-- | The version of the coding system.
--
-- ObjC selector: @- version@
version :: IsHKClinicalCoding hkClinicalCoding => hkClinicalCoding -> IO (Id NSString)
version hkClinicalCoding =
  sendMessage hkClinicalCoding versionSelector

-- | The clinical code that represents a medical concept inside the coding system.
--
-- The format depends on the coding system. For example, RxNorm codes are numeric.
--
-- ObjC selector: @- code@
code :: IsHKClinicalCoding hkClinicalCoding => hkClinicalCoding -> IO (Id NSString)
code hkClinicalCoding =
  sendMessage hkClinicalCoding codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKClinicalCoding)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSystem:version:code:@
initWithSystem_version_codeSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id HKClinicalCoding)
initWithSystem_version_codeSelector = mkSelector "initWithSystem:version:code:"

-- | @Selector@ for @system@
systemSelector :: Selector '[] (Id NSString)
systemSelector = mkSelector "system"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @code@
codeSelector :: Selector '[] (Id NSString)
codeSelector = mkSelector "code"

