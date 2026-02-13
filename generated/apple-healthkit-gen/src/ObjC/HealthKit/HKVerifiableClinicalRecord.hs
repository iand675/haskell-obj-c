{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKVerifiableClinicalRecord
--
-- An NSObject that represents a verifiable clinical record.
--
-- Generated bindings for @HKVerifiableClinicalRecord@.
module ObjC.HealthKit.HKVerifiableClinicalRecord
  ( HKVerifiableClinicalRecord
  , IsHKVerifiableClinicalRecord(..)
  , init_
  , new
  , recordTypes
  , issuerIdentifier
  , subject
  , issuedDate
  , relevantDate
  , expirationDate
  , itemNames
  , sourceType
  , dataRepresentation
  , jwsRepresentation
  , dataRepresentationSelector
  , expirationDateSelector
  , initSelector
  , issuedDateSelector
  , issuerIdentifierSelector
  , itemNamesSelector
  , jwsRepresentationSelector
  , newSelector
  , recordTypesSelector
  , relevantDateSelector
  , sourceTypeSelector
  , subjectSelector


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
init_ :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id HKVerifiableClinicalRecord)
init_ hkVerifiableClinicalRecord =
  sendOwnedMessage hkVerifiableClinicalRecord initSelector

-- | @+ new@
new :: IO (Id HKVerifiableClinicalRecord)
new  =
  do
    cls' <- getRequiredClass "HKVerifiableClinicalRecord"
    sendOwnedClassMessage cls' newSelector

-- | recordTypes
--
-- The types present in this record.
--
-- ObjC selector: @- recordTypes@
recordTypes :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSArray)
recordTypes hkVerifiableClinicalRecord =
  sendMessage hkVerifiableClinicalRecord recordTypesSelector

-- | issuerIdentifier
--
-- The identifier for the issuer of this record.
--
-- ObjC selector: @- issuerIdentifier@
issuerIdentifier :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSString)
issuerIdentifier hkVerifiableClinicalRecord =
  sendMessage hkVerifiableClinicalRecord issuerIdentifierSelector

-- | subject
--
-- The subject of this record.
--
-- ObjC selector: @- subject@
subject :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id HKVerifiableClinicalRecordSubject)
subject hkVerifiableClinicalRecord =
  sendMessage hkVerifiableClinicalRecord subjectSelector

-- | issuedDate
--
-- The date this record was issued.
--
-- ObjC selector: @- issuedDate@
issuedDate :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSDate)
issuedDate hkVerifiableClinicalRecord =
  sendMessage hkVerifiableClinicalRecord issuedDateSelector

-- | relevantDate
--
-- A date most relevant to this record, like when a vaccine was administered or a test was performed.
--
-- ObjC selector: @- relevantDate@
relevantDate :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSDate)
relevantDate hkVerifiableClinicalRecord =
  sendMessage hkVerifiableClinicalRecord relevantDateSelector

-- | expirationDate
--
-- The date this record expires.
--
-- ObjC selector: @- expirationDate@
expirationDate :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSDate)
expirationDate hkVerifiableClinicalRecord =
  sendMessage hkVerifiableClinicalRecord expirationDateSelector

-- | itemNames
--
-- A list of display names for each item contained in this record.
--
-- ObjC selector: @- itemNames@
itemNames :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSArray)
itemNames hkVerifiableClinicalRecord =
  sendMessage hkVerifiableClinicalRecord itemNamesSelector

-- | sourceType
--
-- The type of the source leading to this verifiable record.
--
-- ObjC selector: @- sourceType@
sourceType :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSString)
sourceType hkVerifiableClinicalRecord =
  sendMessage hkVerifiableClinicalRecord sourceTypeSelector

-- | dataRepresentation
--
-- The record's data representation, determined by source type.
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSData)
dataRepresentation hkVerifiableClinicalRecord =
  sendMessage hkVerifiableClinicalRecord dataRepresentationSelector

-- | JWSRepresentation
--
-- The record's entirety as JSON Web Signature (JWS) data.
--
-- ObjC selector: @- JWSRepresentation@
jwsRepresentation :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSData)
jwsRepresentation hkVerifiableClinicalRecord =
  sendMessage hkVerifiableClinicalRecord jwsRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKVerifiableClinicalRecord)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKVerifiableClinicalRecord)
newSelector = mkSelector "new"

-- | @Selector@ for @recordTypes@
recordTypesSelector :: Selector '[] (Id NSArray)
recordTypesSelector = mkSelector "recordTypes"

-- | @Selector@ for @issuerIdentifier@
issuerIdentifierSelector :: Selector '[] (Id NSString)
issuerIdentifierSelector = mkSelector "issuerIdentifier"

-- | @Selector@ for @subject@
subjectSelector :: Selector '[] (Id HKVerifiableClinicalRecordSubject)
subjectSelector = mkSelector "subject"

-- | @Selector@ for @issuedDate@
issuedDateSelector :: Selector '[] (Id NSDate)
issuedDateSelector = mkSelector "issuedDate"

-- | @Selector@ for @relevantDate@
relevantDateSelector :: Selector '[] (Id NSDate)
relevantDateSelector = mkSelector "relevantDate"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector '[] (Id NSDate)
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @itemNames@
itemNamesSelector :: Selector '[] (Id NSArray)
itemNamesSelector = mkSelector "itemNames"

-- | @Selector@ for @sourceType@
sourceTypeSelector :: Selector '[] (Id NSString)
sourceTypeSelector = mkSelector "sourceType"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector '[] (Id NSData)
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @JWSRepresentation@
jwsRepresentationSelector :: Selector '[] (Id NSData)
jwsRepresentationSelector = mkSelector "JWSRepresentation"

