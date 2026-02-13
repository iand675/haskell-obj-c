{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKGlassesPrescription
--
-- An object subclass representing a glasses prescription
--
-- Generated bindings for @HKGlassesPrescription@.
module ObjC.HealthKit.HKGlassesPrescription
  ( HKGlassesPrescription
  , IsHKGlassesPrescription(..)
  , prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadata
  , init_
  , new
  , prescriptionWithType_dateIssued_expirationDate_device_metadata
  , rightEye
  , leftEye
  , initSelector
  , leftEyeSelector
  , newSelector
  , prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadataSelector
  , prescriptionWithType_dateIssued_expirationDate_device_metadataSelector
  , rightEyeSelector

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

-- | prescriptionWithRightEyeSpecification:leftEyeSpecification:type:dateIssued:expirationDate:device:metadata
--
-- @rightEyeSpecification@ — The right eye specification
--
-- @leftEyeSpecification@ — The left eye specification
--
-- @dateIssued@ — The date the prescription was issued
--
-- @expirationDate@ — The date the prescription expires
--
-- @device@ — The device that generated the sample
--
-- @metadata@ — The metadata for the sample
--
-- ObjC selector: @+ prescriptionWithRightEyeSpecification:leftEyeSpecification:dateIssued:expirationDate:device:metadata:@
prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadata :: (IsHKGlassesLensSpecification rightEyeSpecification, IsHKGlassesLensSpecification leftEyeSpecification, IsNSDate dateIssued, IsNSDate expirationDate, IsHKDevice device, IsNSDictionary metadata) => rightEyeSpecification -> leftEyeSpecification -> dateIssued -> expirationDate -> device -> metadata -> IO (Id HKGlassesPrescription)
prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadata rightEyeSpecification leftEyeSpecification dateIssued expirationDate device metadata =
  do
    cls' <- getRequiredClass "HKGlassesPrescription"
    sendClassMessage cls' prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadataSelector (toHKGlassesLensSpecification rightEyeSpecification) (toHKGlassesLensSpecification leftEyeSpecification) (toNSDate dateIssued) (toNSDate expirationDate) (toHKDevice device) (toNSDictionary metadata)

-- | @- init@
init_ :: IsHKGlassesPrescription hkGlassesPrescription => hkGlassesPrescription -> IO (Id HKGlassesPrescription)
init_ hkGlassesPrescription =
  sendOwnedMessage hkGlassesPrescription initSelector

-- | @+ new@
new :: IO (Id HKGlassesPrescription)
new  =
  do
    cls' <- getRequiredClass "HKGlassesPrescription"
    sendOwnedClassMessage cls' newSelector

-- | @+ prescriptionWithType:dateIssued:expirationDate:device:metadata:@
prescriptionWithType_dateIssued_expirationDate_device_metadata :: (IsNSDate dateIssued, IsNSDate expirationDate, IsHKDevice device, IsNSDictionary metadata) => HKVisionPrescriptionType -> dateIssued -> expirationDate -> device -> metadata -> IO (Id HKGlassesPrescription)
prescriptionWithType_dateIssued_expirationDate_device_metadata type_ dateIssued expirationDate device metadata =
  do
    cls' <- getRequiredClass "HKGlassesPrescription"
    sendClassMessage cls' prescriptionWithType_dateIssued_expirationDate_device_metadataSelector type_ (toNSDate dateIssued) (toNSDate expirationDate) (toHKDevice device) (toNSDictionary metadata)

-- | rightEye
--
-- The right eye lens specification
--
-- ObjC selector: @- rightEye@
rightEye :: IsHKGlassesPrescription hkGlassesPrescription => hkGlassesPrescription -> IO (Id HKGlassesLensSpecification)
rightEye hkGlassesPrescription =
  sendMessage hkGlassesPrescription rightEyeSelector

-- | leftEye
--
-- The left eye lens specification
--
-- ObjC selector: @- leftEye@
leftEye :: IsHKGlassesPrescription hkGlassesPrescription => hkGlassesPrescription -> IO (Id HKGlassesLensSpecification)
leftEye hkGlassesPrescription =
  sendMessage hkGlassesPrescription leftEyeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prescriptionWithRightEyeSpecification:leftEyeSpecification:dateIssued:expirationDate:device:metadata:@
prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadataSelector :: Selector '[Id HKGlassesLensSpecification, Id HKGlassesLensSpecification, Id NSDate, Id NSDate, Id HKDevice, Id NSDictionary] (Id HKGlassesPrescription)
prescriptionWithRightEyeSpecification_leftEyeSpecification_dateIssued_expirationDate_device_metadataSelector = mkSelector "prescriptionWithRightEyeSpecification:leftEyeSpecification:dateIssued:expirationDate:device:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKGlassesPrescription)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKGlassesPrescription)
newSelector = mkSelector "new"

-- | @Selector@ for @prescriptionWithType:dateIssued:expirationDate:device:metadata:@
prescriptionWithType_dateIssued_expirationDate_device_metadataSelector :: Selector '[HKVisionPrescriptionType, Id NSDate, Id NSDate, Id HKDevice, Id NSDictionary] (Id HKGlassesPrescription)
prescriptionWithType_dateIssued_expirationDate_device_metadataSelector = mkSelector "prescriptionWithType:dateIssued:expirationDate:device:metadata:"

-- | @Selector@ for @rightEye@
rightEyeSelector :: Selector '[] (Id HKGlassesLensSpecification)
rightEyeSelector = mkSelector "rightEye"

-- | @Selector@ for @leftEye@
leftEyeSelector :: Selector '[] (Id HKGlassesLensSpecification)
leftEyeSelector = mkSelector "leftEye"

