{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKVerifiableClinicalRecordSubject
--
-- An NSObject that represents a verifiable clinical record subject.
--
-- Generated bindings for @HKVerifiableClinicalRecordSubject@.
module ObjC.HealthKit.HKVerifiableClinicalRecordSubject
  ( HKVerifiableClinicalRecordSubject
  , IsHKVerifiableClinicalRecordSubject(..)
  , init_
  , new
  , fullName
  , dateOfBirthComponents
  , dateOfBirthComponentsSelector
  , fullNameSelector
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
init_ :: IsHKVerifiableClinicalRecordSubject hkVerifiableClinicalRecordSubject => hkVerifiableClinicalRecordSubject -> IO (Id HKVerifiableClinicalRecordSubject)
init_ hkVerifiableClinicalRecordSubject =
  sendOwnedMessage hkVerifiableClinicalRecordSubject initSelector

-- | @+ new@
new :: IO (Id HKVerifiableClinicalRecordSubject)
new  =
  do
    cls' <- getRequiredClass "HKVerifiableClinicalRecordSubject"
    sendOwnedClassMessage cls' newSelector

-- | fullName
--
-- The subject's full name.
--
-- ObjC selector: @- fullName@
fullName :: IsHKVerifiableClinicalRecordSubject hkVerifiableClinicalRecordSubject => hkVerifiableClinicalRecordSubject -> IO (Id NSString)
fullName hkVerifiableClinicalRecordSubject =
  sendMessage hkVerifiableClinicalRecordSubject fullNameSelector

-- | dateOfBirthComponents
--
-- The subject's date of birth components.
--
-- ObjC selector: @- dateOfBirthComponents@
dateOfBirthComponents :: IsHKVerifiableClinicalRecordSubject hkVerifiableClinicalRecordSubject => hkVerifiableClinicalRecordSubject -> IO (Id NSDateComponents)
dateOfBirthComponents hkVerifiableClinicalRecordSubject =
  sendMessage hkVerifiableClinicalRecordSubject dateOfBirthComponentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKVerifiableClinicalRecordSubject)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKVerifiableClinicalRecordSubject)
newSelector = mkSelector "new"

-- | @Selector@ for @fullName@
fullNameSelector :: Selector '[] (Id NSString)
fullNameSelector = mkSelector "fullName"

-- | @Selector@ for @dateOfBirthComponents@
dateOfBirthComponentsSelector :: Selector '[] (Id NSDateComponents)
dateOfBirthComponentsSelector = mkSelector "dateOfBirthComponents"

