{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKClinicalRecord
--
-- An HKObject subclass representing a health record.
--
-- The startDate and endDate properties (inherited from HKSample) are set to the date the sample was               added to Health. Unlike other HKObject subclasses, UUID is not a stable identifier               for a given sample. Use a combination of HKSource, FHIRResource.resourceType, and               FHIRResource.identifier instead.
--
-- Generated bindings for @HKClinicalRecord@.
module ObjC.HealthKit.HKClinicalRecord
  ( HKClinicalRecord
  , IsHKClinicalRecord(..)
  , init_
  , new
  , clinicalType
  , displayName
  , fhirResource
  , clinicalTypeSelector
  , displayNameSelector
  , fhirResourceSelector
  , initSelector
  , newSelector


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
init_ :: IsHKClinicalRecord hkClinicalRecord => hkClinicalRecord -> IO (Id HKClinicalRecord)
init_ hkClinicalRecord =
  sendOwnedMessage hkClinicalRecord initSelector

-- | @+ new@
new :: IO (Id HKClinicalRecord)
new  =
  do
    cls' <- getRequiredClass "HKClinicalRecord"
    sendOwnedClassMessage cls' newSelector

-- | @- clinicalType@
clinicalType :: IsHKClinicalRecord hkClinicalRecord => hkClinicalRecord -> IO (Id HKClinicalType)
clinicalType hkClinicalRecord =
  sendMessage hkClinicalRecord clinicalTypeSelector

-- | displayName
--
-- The primary display name used in Health.
--
-- The display name is not localized, and is generally expected to be US English.
--
-- ObjC selector: @- displayName@
displayName :: IsHKClinicalRecord hkClinicalRecord => hkClinicalRecord -> IO (Id NSString)
displayName hkClinicalRecord =
  sendMessage hkClinicalRecord displayNameSelector

-- | FHIRResource
--
-- The FHIR resource (where applicable) backing this sample.
--
-- ObjC selector: @- FHIRResource@
fhirResource :: IsHKClinicalRecord hkClinicalRecord => hkClinicalRecord -> IO (Id HKFHIRResource)
fhirResource hkClinicalRecord =
  sendMessage hkClinicalRecord fhirResourceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKClinicalRecord)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKClinicalRecord)
newSelector = mkSelector "new"

-- | @Selector@ for @clinicalType@
clinicalTypeSelector :: Selector '[] (Id HKClinicalType)
clinicalTypeSelector = mkSelector "clinicalType"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @FHIRResource@
fhirResourceSelector :: Selector '[] (Id HKFHIRResource)
fhirResourceSelector = mkSelector "FHIRResource"

