{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKFHIRResource
--
-- The HKFHIRResource class encapsulates a FHIR (Fast Healthcare Interoperability Resources) resource.
--
-- Generated bindings for @HKFHIRResource@.
module ObjC.HealthKit.HKFHIRResource
  ( HKFHIRResource
  , IsHKFHIRResource(..)
  , init_
  , fhirVersion
  , resourceType
  , identifier
  , data_
  , sourceURL
  , dataSelector
  , fhirVersionSelector
  , identifierSelector
  , initSelector
  , resourceTypeSelector
  , sourceURLSelector


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
init_ :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id HKFHIRResource)
init_ hkfhirResource =
  sendOwnedMessage hkfhirResource initSelector

-- | FHIRVersion
--
-- The FHIR version of the resource data.
--
-- ObjC selector: @- FHIRVersion@
fhirVersion :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id HKFHIRVersion)
fhirVersion hkfhirResource =
  sendMessage hkfhirResource fhirVersionSelector

-- | resourceType
--
-- The resource type, corresponding to the 'resourceType' field in the resource's JSON representation.
--
-- May be one of 8 FHIR resource types supported within HealthKit: AllergyIntolerance, Condition,                Immunization, MedicationDispense, MedicationOrder, MedicationStatement, Observation, and Procedure.
--
-- ObjC selector: @- resourceType@
resourceType :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id NSString)
resourceType hkfhirResource =
  sendMessage hkfhirResource resourceTypeSelector

-- | identifier
--
-- The identifier of the resource, corresponding to the 'id' field in the resource's JSON representation.
--
-- Unique within a given resource type and FHIR end-point, as represented by an HKSource.
--
-- ObjC selector: @- identifier@
identifier :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id NSString)
identifier hkfhirResource =
  sendMessage hkfhirResource identifierSelector

-- | data
--
-- The JSON representation of the FHIR resource.
--
-- Conforms to the HL7 Argonaut Project resource type definitions.
--
-- ObjC selector: @- data@
data_ :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id NSData)
data_ hkfhirResource =
  sendMessage hkfhirResource dataSelector

-- | sourceURL
--
-- The fully specified source URL of the FHIR resource.
--
-- This URL can be used to help determine the provenance of the resource. Direct access is protected by                OAuth: querying without suitable authorization will result in an authorization error.
--
-- ObjC selector: @- sourceURL@
sourceURL :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id NSURL)
sourceURL hkfhirResource =
  sendMessage hkfhirResource sourceURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKFHIRResource)
initSelector = mkSelector "init"

-- | @Selector@ for @FHIRVersion@
fhirVersionSelector :: Selector '[] (Id HKFHIRVersion)
fhirVersionSelector = mkSelector "FHIRVersion"

-- | @Selector@ for @resourceType@
resourceTypeSelector :: Selector '[] (Id NSString)
resourceTypeSelector = mkSelector "resourceType"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @sourceURL@
sourceURLSelector :: Selector '[] (Id NSURL)
sourceURLSelector = mkSelector "sourceURL"

