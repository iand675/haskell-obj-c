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
  , initSelector
  , fhirVersionSelector
  , resourceTypeSelector
  , identifierSelector
  , dataSelector
  , sourceURLSelector


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

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id HKFHIRResource)
init_ hkfhirResource  =
    sendMsg hkfhirResource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | FHIRVersion
--
-- The FHIR version of the resource data.
--
-- ObjC selector: @- FHIRVersion@
fhirVersion :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id HKFHIRVersion)
fhirVersion hkfhirResource  =
    sendMsg hkfhirResource (mkSelector "FHIRVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | resourceType
--
-- The resource type, corresponding to the 'resourceType' field in the resource's JSON representation.
--
-- May be one of 8 FHIR resource types supported within HealthKit: AllergyIntolerance, Condition,                Immunization, MedicationDispense, MedicationOrder, MedicationStatement, Observation, and Procedure.
--
-- ObjC selector: @- resourceType@
resourceType :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id NSString)
resourceType hkfhirResource  =
    sendMsg hkfhirResource (mkSelector "resourceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | identifier
--
-- The identifier of the resource, corresponding to the 'id' field in the resource's JSON representation.
--
-- Unique within a given resource type and FHIR end-point, as represented by an HKSource.
--
-- ObjC selector: @- identifier@
identifier :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id NSString)
identifier hkfhirResource  =
    sendMsg hkfhirResource (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | data
--
-- The JSON representation of the FHIR resource.
--
-- Conforms to the HL7 Argonaut Project resource type definitions.
--
-- ObjC selector: @- data@
data_ :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id NSData)
data_ hkfhirResource  =
    sendMsg hkfhirResource (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceURL
--
-- The fully specified source URL of the FHIR resource.
--
-- This URL can be used to help determine the provenance of the resource. Direct access is protected by                OAuth: querying without suitable authorization will result in an authorization error.
--
-- ObjC selector: @- sourceURL@
sourceURL :: IsHKFHIRResource hkfhirResource => hkfhirResource -> IO (Id NSURL)
sourceURL hkfhirResource  =
    sendMsg hkfhirResource (mkSelector "sourceURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @FHIRVersion@
fhirVersionSelector :: Selector
fhirVersionSelector = mkSelector "FHIRVersion"

-- | @Selector@ for @resourceType@
resourceTypeSelector :: Selector
resourceTypeSelector = mkSelector "resourceType"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @sourceURL@
sourceURLSelector :: Selector
sourceURLSelector = mkSelector "sourceURL"

