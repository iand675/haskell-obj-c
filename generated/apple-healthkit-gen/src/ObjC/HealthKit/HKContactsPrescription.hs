{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKContactsPrescription
--
-- An object representing a contacts prescription
--
-- Generated bindings for @HKContactsPrescription@.
module ObjC.HealthKit.HKContactsPrescription
  ( HKContactsPrescription
  , IsHKContactsPrescription(..)
  , prescriptionWithRightEyeSpecification_leftEyeSpecification_brand_dateIssued_expirationDate_device_metadata
  , init_
  , new
  , prescriptionWithType_dateIssued_expirationDate_device_metadata
  , rightEye
  , leftEye
  , brand
  , brandSelector
  , initSelector
  , leftEyeSelector
  , newSelector
  , prescriptionWithRightEyeSpecification_leftEyeSpecification_brand_dateIssued_expirationDate_device_metadataSelector
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

-- | prescriptionWithRightEyeSpecification:leftEyeSpecification:brand:dateIssued:expirationDate:device:metadata
--
-- @rightEyeSpecification@ — The right eye specification
--
-- @leftEyeSpecification@ — The left eye specification
--
-- @brand@ — The prescribed brand after contact lens fitting
--
-- @dateIssued@ — The date the prescription was issued
--
-- @expirationDate@ — The date the prescription expires
--
-- @device@ — The device that generated the sample
--
-- @metadata@ — The metadata for the sample
--
-- ObjC selector: @+ prescriptionWithRightEyeSpecification:leftEyeSpecification:brand:dateIssued:expirationDate:device:metadata:@
prescriptionWithRightEyeSpecification_leftEyeSpecification_brand_dateIssued_expirationDate_device_metadata :: (IsHKContactsLensSpecification rightEyeSpecification, IsHKContactsLensSpecification leftEyeSpecification, IsNSString brand, IsNSDate dateIssued, IsNSDate expirationDate, IsHKDevice device, IsNSDictionary metadata) => rightEyeSpecification -> leftEyeSpecification -> brand -> dateIssued -> expirationDate -> device -> metadata -> IO (Id HKContactsPrescription)
prescriptionWithRightEyeSpecification_leftEyeSpecification_brand_dateIssued_expirationDate_device_metadata rightEyeSpecification leftEyeSpecification brand dateIssued expirationDate device metadata =
  do
    cls' <- getRequiredClass "HKContactsPrescription"
    sendClassMessage cls' prescriptionWithRightEyeSpecification_leftEyeSpecification_brand_dateIssued_expirationDate_device_metadataSelector (toHKContactsLensSpecification rightEyeSpecification) (toHKContactsLensSpecification leftEyeSpecification) (toNSString brand) (toNSDate dateIssued) (toNSDate expirationDate) (toHKDevice device) (toNSDictionary metadata)

-- | @- init@
init_ :: IsHKContactsPrescription hkContactsPrescription => hkContactsPrescription -> IO (Id HKContactsPrescription)
init_ hkContactsPrescription =
  sendOwnedMessage hkContactsPrescription initSelector

-- | @+ new@
new :: IO (Id HKContactsPrescription)
new  =
  do
    cls' <- getRequiredClass "HKContactsPrescription"
    sendOwnedClassMessage cls' newSelector

-- | @+ prescriptionWithType:dateIssued:expirationDate:device:metadata:@
prescriptionWithType_dateIssued_expirationDate_device_metadata :: (IsNSDate dateIssued, IsNSDate expirationDate, IsHKDevice device, IsNSDictionary metadata) => HKVisionPrescriptionType -> dateIssued -> expirationDate -> device -> metadata -> IO (Id HKContactsPrescription)
prescriptionWithType_dateIssued_expirationDate_device_metadata type_ dateIssued expirationDate device metadata =
  do
    cls' <- getRequiredClass "HKContactsPrescription"
    sendClassMessage cls' prescriptionWithType_dateIssued_expirationDate_device_metadataSelector type_ (toNSDate dateIssued) (toNSDate expirationDate) (toHKDevice device) (toNSDictionary metadata)

-- | rightEye
--
-- The right eye lens specification
--
-- ObjC selector: @- rightEye@
rightEye :: IsHKContactsPrescription hkContactsPrescription => hkContactsPrescription -> IO (Id HKContactsLensSpecification)
rightEye hkContactsPrescription =
  sendMessage hkContactsPrescription rightEyeSelector

-- | leftEye
--
-- The left eye lens specification
--
-- ObjC selector: @- leftEye@
leftEye :: IsHKContactsPrescription hkContactsPrescription => hkContactsPrescription -> IO (Id HKContactsLensSpecification)
leftEye hkContactsPrescription =
  sendMessage hkContactsPrescription leftEyeSelector

-- | brand
--
-- The prescribed brand after contact lens fitting
--
-- ObjC selector: @- brand@
brand :: IsHKContactsPrescription hkContactsPrescription => hkContactsPrescription -> IO (Id NSString)
brand hkContactsPrescription =
  sendMessage hkContactsPrescription brandSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prescriptionWithRightEyeSpecification:leftEyeSpecification:brand:dateIssued:expirationDate:device:metadata:@
prescriptionWithRightEyeSpecification_leftEyeSpecification_brand_dateIssued_expirationDate_device_metadataSelector :: Selector '[Id HKContactsLensSpecification, Id HKContactsLensSpecification, Id NSString, Id NSDate, Id NSDate, Id HKDevice, Id NSDictionary] (Id HKContactsPrescription)
prescriptionWithRightEyeSpecification_leftEyeSpecification_brand_dateIssued_expirationDate_device_metadataSelector = mkSelector "prescriptionWithRightEyeSpecification:leftEyeSpecification:brand:dateIssued:expirationDate:device:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKContactsPrescription)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKContactsPrescription)
newSelector = mkSelector "new"

-- | @Selector@ for @prescriptionWithType:dateIssued:expirationDate:device:metadata:@
prescriptionWithType_dateIssued_expirationDate_device_metadataSelector :: Selector '[HKVisionPrescriptionType, Id NSDate, Id NSDate, Id HKDevice, Id NSDictionary] (Id HKContactsPrescription)
prescriptionWithType_dateIssued_expirationDate_device_metadataSelector = mkSelector "prescriptionWithType:dateIssued:expirationDate:device:metadata:"

-- | @Selector@ for @rightEye@
rightEyeSelector :: Selector '[] (Id HKContactsLensSpecification)
rightEyeSelector = mkSelector "rightEye"

-- | @Selector@ for @leftEye@
leftEyeSelector :: Selector '[] (Id HKContactsLensSpecification)
leftEyeSelector = mkSelector "leftEye"

-- | @Selector@ for @brand@
brandSelector :: Selector '[] (Id NSString)
brandSelector = mkSelector "brand"

