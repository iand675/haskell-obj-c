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
  , initSelector
  , newSelector
  , recordTypesSelector
  , issuerIdentifierSelector
  , subjectSelector
  , issuedDateSelector
  , relevantDateSelector
  , expirationDateSelector
  , itemNamesSelector
  , sourceTypeSelector
  , dataRepresentationSelector
  , jwsRepresentationSelector


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
init_ :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id HKVerifiableClinicalRecord)
init_ hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKVerifiableClinicalRecord)
new  =
  do
    cls' <- getRequiredClass "HKVerifiableClinicalRecord"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | recordTypes
--
-- The types present in this record.
--
-- ObjC selector: @- recordTypes@
recordTypes :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSArray)
recordTypes hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "recordTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | issuerIdentifier
--
-- The identifier for the issuer of this record.
--
-- ObjC selector: @- issuerIdentifier@
issuerIdentifier :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSString)
issuerIdentifier hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "issuerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | subject
--
-- The subject of this record.
--
-- ObjC selector: @- subject@
subject :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id HKVerifiableClinicalRecordSubject)
subject hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "subject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | issuedDate
--
-- The date this record was issued.
--
-- ObjC selector: @- issuedDate@
issuedDate :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSDate)
issuedDate hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "issuedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | relevantDate
--
-- A date most relevant to this record, like when a vaccine was administered or a test was performed.
--
-- ObjC selector: @- relevantDate@
relevantDate :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSDate)
relevantDate hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "relevantDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | expirationDate
--
-- The date this record expires.
--
-- ObjC selector: @- expirationDate@
expirationDate :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSDate)
expirationDate hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "expirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | itemNames
--
-- A list of display names for each item contained in this record.
--
-- ObjC selector: @- itemNames@
itemNames :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSArray)
itemNames hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "itemNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceType
--
-- The type of the source leading to this verifiable record.
--
-- ObjC selector: @- sourceType@
sourceType :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSString)
sourceType hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "sourceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dataRepresentation
--
-- The record's data representation, determined by source type.
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSData)
dataRepresentation hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | JWSRepresentation
--
-- The record's entirety as JSON Web Signature (JWS) data.
--
-- ObjC selector: @- JWSRepresentation@
jwsRepresentation :: IsHKVerifiableClinicalRecord hkVerifiableClinicalRecord => hkVerifiableClinicalRecord -> IO (Id NSData)
jwsRepresentation hkVerifiableClinicalRecord  =
    sendMsg hkVerifiableClinicalRecord (mkSelector "JWSRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @recordTypes@
recordTypesSelector :: Selector
recordTypesSelector = mkSelector "recordTypes"

-- | @Selector@ for @issuerIdentifier@
issuerIdentifierSelector :: Selector
issuerIdentifierSelector = mkSelector "issuerIdentifier"

-- | @Selector@ for @subject@
subjectSelector :: Selector
subjectSelector = mkSelector "subject"

-- | @Selector@ for @issuedDate@
issuedDateSelector :: Selector
issuedDateSelector = mkSelector "issuedDate"

-- | @Selector@ for @relevantDate@
relevantDateSelector :: Selector
relevantDateSelector = mkSelector "relevantDate"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @itemNames@
itemNamesSelector :: Selector
itemNamesSelector = mkSelector "itemNames"

-- | @Selector@ for @sourceType@
sourceTypeSelector :: Selector
sourceTypeSelector = mkSelector "sourceType"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @JWSRepresentation@
jwsRepresentationSelector :: Selector
jwsRepresentationSelector = mkSelector "JWSRepresentation"

