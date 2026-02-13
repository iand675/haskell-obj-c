{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKFHIRVersion
--
-- Represents a FHIR version.
--
-- FHIR uses semantic versions ("1.0.2", "4.0.1") to communicate which FHIR version a server supports or a                given resource is represented in. A FHIR version is associated with one FHIR release.
--
-- See: http://hl7.org/fhir/versions.html#versions
--
-- Generated bindings for @HKFHIRVersion@.
module ObjC.HealthKit.HKFHIRVersion
  ( HKFHIRVersion
  , IsHKFHIRVersion(..)
  , init_
  , versionFromVersionString_error
  , primaryDSTU2Version
  , primaryR4Version
  , majorVersion
  , minorVersion
  , patchVersion
  , fhirRelease
  , stringRepresentation
  , fhirReleaseSelector
  , initSelector
  , majorVersionSelector
  , minorVersionSelector
  , patchVersionSelector
  , primaryDSTU2VersionSelector
  , primaryR4VersionSelector
  , stringRepresentationSelector
  , versionFromVersionString_errorSelector


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
init_ :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO (Id HKFHIRVersion)
init_ hkfhirVersion =
  sendOwnedMessage hkfhirVersion initSelector

-- | @+ versionFromVersionString:error:@
versionFromVersionString_error :: (IsNSString versionString, IsNSError errorOut) => versionString -> errorOut -> IO (Id HKFHIRVersion)
versionFromVersionString_error versionString errorOut =
  do
    cls' <- getRequiredClass "HKFHIRVersion"
    sendClassMessage cls' versionFromVersionString_errorSelector (toNSString versionString) (toNSError errorOut)

-- | @+ primaryDSTU2Version@
primaryDSTU2Version :: IO (Id HKFHIRVersion)
primaryDSTU2Version  =
  do
    cls' <- getRequiredClass "HKFHIRVersion"
    sendClassMessage cls' primaryDSTU2VersionSelector

-- | @+ primaryR4Version@
primaryR4Version :: IO (Id HKFHIRVersion)
primaryR4Version  =
  do
    cls' <- getRequiredClass "HKFHIRVersion"
    sendClassMessage cls' primaryR4VersionSelector

-- | @- majorVersion@
majorVersion :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO CLong
majorVersion hkfhirVersion =
  sendMessage hkfhirVersion majorVersionSelector

-- | @- minorVersion@
minorVersion :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO CLong
minorVersion hkfhirVersion =
  sendMessage hkfhirVersion minorVersionSelector

-- | @- patchVersion@
patchVersion :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO CLong
patchVersion hkfhirVersion =
  sendMessage hkfhirVersion patchVersionSelector

-- | @- FHIRRelease@
fhirRelease :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO (Id NSString)
fhirRelease hkfhirVersion =
  sendMessage hkfhirVersion fhirReleaseSelector

-- | stringRepresentation
--
-- A string representation in the format "{major}.{minor}.{patch}".
--
-- ObjC selector: @- stringRepresentation@
stringRepresentation :: IsHKFHIRVersion hkfhirVersion => hkfhirVersion -> IO (Id NSString)
stringRepresentation hkfhirVersion =
  sendMessage hkfhirVersion stringRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKFHIRVersion)
initSelector = mkSelector "init"

-- | @Selector@ for @versionFromVersionString:error:@
versionFromVersionString_errorSelector :: Selector '[Id NSString, Id NSError] (Id HKFHIRVersion)
versionFromVersionString_errorSelector = mkSelector "versionFromVersionString:error:"

-- | @Selector@ for @primaryDSTU2Version@
primaryDSTU2VersionSelector :: Selector '[] (Id HKFHIRVersion)
primaryDSTU2VersionSelector = mkSelector "primaryDSTU2Version"

-- | @Selector@ for @primaryR4Version@
primaryR4VersionSelector :: Selector '[] (Id HKFHIRVersion)
primaryR4VersionSelector = mkSelector "primaryR4Version"

-- | @Selector@ for @majorVersion@
majorVersionSelector :: Selector '[] CLong
majorVersionSelector = mkSelector "majorVersion"

-- | @Selector@ for @minorVersion@
minorVersionSelector :: Selector '[] CLong
minorVersionSelector = mkSelector "minorVersion"

-- | @Selector@ for @patchVersion@
patchVersionSelector :: Selector '[] CLong
patchVersionSelector = mkSelector "patchVersion"

-- | @Selector@ for @FHIRRelease@
fhirReleaseSelector :: Selector '[] (Id NSString)
fhirReleaseSelector = mkSelector "FHIRRelease"

-- | @Selector@ for @stringRepresentation@
stringRepresentationSelector :: Selector '[] (Id NSString)
stringRepresentationSelector = mkSelector "stringRepresentation"

