{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKVisionPrescription
--
-- HKSample subclass representing a vision prescription
--
-- Generated bindings for @HKVisionPrescription@.
module ObjC.HealthKit.HKVisionPrescription
  ( HKVisionPrescription
  , IsHKVisionPrescription(..)
  , prescriptionWithType_dateIssued_expirationDate_device_metadata
  , init_
  , new
  , prescriptionType
  , dateIssued
  , expirationDate
  , dateIssuedSelector
  , expirationDateSelector
  , initSelector
  , newSelector
  , prescriptionTypeSelector
  , prescriptionWithType_dateIssued_expirationDate_device_metadataSelector

  -- * Enum types
  , HKVisionPrescriptionType(HKVisionPrescriptionType)
  , pattern HKVisionPrescriptionTypeGlasses
  , pattern HKVisionPrescriptionTypeContacts

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | prescriptionWithType:dateIssued:expirationDate:device:metadata
--
-- @type@ — The prescription type
--
-- @dateIssued@ — The date the prescription was issued
--
-- @expirationDate@ — The date the prescription expires
--
-- @device@ — The device that generated the sample
--
-- @metadata@ — The metadata for the sample
--
-- ObjC selector: @+ prescriptionWithType:dateIssued:expirationDate:device:metadata:@
prescriptionWithType_dateIssued_expirationDate_device_metadata :: (IsNSDate dateIssued, IsNSDate expirationDate, IsHKDevice device, IsNSDictionary metadata) => HKVisionPrescriptionType -> dateIssued -> expirationDate -> device -> metadata -> IO (Id HKVisionPrescription)
prescriptionWithType_dateIssued_expirationDate_device_metadata type_ dateIssued expirationDate device metadata =
  do
    cls' <- getRequiredClass "HKVisionPrescription"
    sendClassMessage cls' prescriptionWithType_dateIssued_expirationDate_device_metadataSelector type_ (toNSDate dateIssued) (toNSDate expirationDate) (toHKDevice device) (toNSDictionary metadata)

-- | @- init@
init_ :: IsHKVisionPrescription hkVisionPrescription => hkVisionPrescription -> IO (Id HKVisionPrescription)
init_ hkVisionPrescription =
  sendOwnedMessage hkVisionPrescription initSelector

-- | @+ new@
new :: IO (Id HKVisionPrescription)
new  =
  do
    cls' <- getRequiredClass "HKVisionPrescription"
    sendOwnedClassMessage cls' newSelector

-- | prescriptionType
--
-- A vision prescription type (glasses or contacts)
--
-- ObjC selector: @- prescriptionType@
prescriptionType :: IsHKVisionPrescription hkVisionPrescription => hkVisionPrescription -> IO HKVisionPrescriptionType
prescriptionType hkVisionPrescription =
  sendMessage hkVisionPrescription prescriptionTypeSelector

-- | dateIssued
--
-- The date the prescription was issued
--
-- ObjC selector: @- dateIssued@
dateIssued :: IsHKVisionPrescription hkVisionPrescription => hkVisionPrescription -> IO (Id NSDate)
dateIssued hkVisionPrescription =
  sendMessage hkVisionPrescription dateIssuedSelector

-- | expirationDate
--
-- The date the prescription will expire
--
-- ObjC selector: @- expirationDate@
expirationDate :: IsHKVisionPrescription hkVisionPrescription => hkVisionPrescription -> IO (Id NSDate)
expirationDate hkVisionPrescription =
  sendMessage hkVisionPrescription expirationDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prescriptionWithType:dateIssued:expirationDate:device:metadata:@
prescriptionWithType_dateIssued_expirationDate_device_metadataSelector :: Selector '[HKVisionPrescriptionType, Id NSDate, Id NSDate, Id HKDevice, Id NSDictionary] (Id HKVisionPrescription)
prescriptionWithType_dateIssued_expirationDate_device_metadataSelector = mkSelector "prescriptionWithType:dateIssued:expirationDate:device:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKVisionPrescription)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKVisionPrescription)
newSelector = mkSelector "new"

-- | @Selector@ for @prescriptionType@
prescriptionTypeSelector :: Selector '[] HKVisionPrescriptionType
prescriptionTypeSelector = mkSelector "prescriptionType"

-- | @Selector@ for @dateIssued@
dateIssuedSelector :: Selector '[] (Id NSDate)
dateIssuedSelector = mkSelector "dateIssued"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector '[] (Id NSDate)
expirationDateSelector = mkSelector "expirationDate"

