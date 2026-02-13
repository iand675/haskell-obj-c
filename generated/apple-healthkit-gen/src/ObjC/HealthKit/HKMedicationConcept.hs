{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that describes a specific medication concept.
--
-- A medication concept represents the idea of a medication, like ibuprofen or insulin. It can have clinical significance, or can be created by the person using your app.
--
-- Generated bindings for @HKMedicationConcept@.
module ObjC.HealthKit.HKMedicationConcept
  ( HKMedicationConcept
  , IsHKMedicationConcept(..)
  , init_
  , identifier
  , displayText
  , generalForm
  , relatedCodings
  , displayTextSelector
  , generalFormSelector
  , identifierSelector
  , initSelector
  , relatedCodingsSelector


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
init_ :: IsHKMedicationConcept hkMedicationConcept => hkMedicationConcept -> IO (Id HKMedicationConcept)
init_ hkMedicationConcept =
  sendOwnedMessage hkMedicationConcept initSelector

-- | The unique identifier for the specific medication concept.
--
-- Each concept has one stable identifier that stays the same across devices. You can use this identifier to directly compare medications, for example, to check whether two objects represent the same medication.
--
-- ObjC selector: @- identifier@
identifier :: IsHKMedicationConcept hkMedicationConcept => hkMedicationConcept -> IO (Id HKHealthConceptIdentifier)
identifier hkMedicationConcept =
  sendMessage hkMedicationConcept identifierSelector

-- | The display name for this medication.
--
-- The name of the medication a person enters or selects during medication onboarding.
--
-- ObjC selector: @- displayText@
displayText :: IsHKMedicationConcept hkMedicationConcept => hkMedicationConcept -> IO (Id NSString)
displayText hkMedicationConcept =
  sendMessage hkMedicationConcept displayTextSelector

-- | The general form the medication is manufactured in.
--
-- A general manufactured dose form for the specific medication. This value tells you the manufactured form of the medication, such as tablet, capsule, cream, injection, or inhaler.
--
-- ObjC selector: @- generalForm@
generalForm :: IsHKMedicationConcept hkMedicationConcept => hkMedicationConcept -> IO (Id NSString)
generalForm hkMedicationConcept =
  sendMessage hkMedicationConcept generalFormSelector

-- | The set of related clinical codings for the medication.
--
-- Each coding links the medication to an external medical terminology system, such as RxNorm.
--
-- ObjC selector: @- relatedCodings@
relatedCodings :: IsHKMedicationConcept hkMedicationConcept => hkMedicationConcept -> IO (Id NSSet)
relatedCodings hkMedicationConcept =
  sendMessage hkMedicationConcept relatedCodingsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKMedicationConcept)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id HKHealthConceptIdentifier)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @displayText@
displayTextSelector :: Selector '[] (Id NSString)
displayTextSelector = mkSelector "displayText"

-- | @Selector@ for @generalForm@
generalFormSelector :: Selector '[] (Id NSString)
generalFormSelector = mkSelector "generalForm"

-- | @Selector@ for @relatedCodings@
relatedCodingsSelector :: Selector '[] (Id NSSet)
relatedCodingsSelector = mkSelector "relatedCodings"

