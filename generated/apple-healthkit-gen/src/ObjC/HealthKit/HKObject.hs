{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKObject@.
module ObjC.HealthKit.HKObject
  ( HKObject
  , IsHKObject(..)
  , init_
  , uuid
  , source
  , sourceRevision
  , device
  , metadata
  , deviceSelector
  , initSelector
  , metadataSelector
  , sourceRevisionSelector
  , sourceSelector
  , uuidSelector


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
init_ :: IsHKObject hkObject => hkObject -> IO (Id HKObject)
init_ hkObject =
  sendOwnedMessage hkObject initSelector

-- | UUID
--
-- A unique identifier of the receiver in the HealthKit database.
--
-- ObjC selector: @- UUID@
uuid :: IsHKObject hkObject => hkObject -> IO (Id NSUUID)
uuid hkObject =
  sendMessage hkObject uuidSelector

-- | @- source@
source :: IsHKObject hkObject => hkObject -> IO (Id HKSource)
source hkObject =
  sendMessage hkObject sourceSelector

-- | sourceRevision
--
-- Represents the revision of the source responsible for saving the receiver.
--
-- ObjC selector: @- sourceRevision@
sourceRevision :: IsHKObject hkObject => hkObject -> IO (Id HKSourceRevision)
sourceRevision hkObject =
  sendMessage hkObject sourceRevisionSelector

-- | device
--
-- Represents the device that generated the data of the receiver.
--
-- ObjC selector: @- device@
device :: IsHKObject hkObject => hkObject -> IO (Id HKDevice)
device hkObject =
  sendMessage hkObject deviceSelector

-- | metadata
--
-- Extra information describing properties of the receiver.
--
-- Keys must be NSString and values must be either NSString, NSNumber, NSDate, or                HKQuantity. See HKMetadata.h for potential metadata keys and values.
--
-- ObjC selector: @- metadata@
metadata :: IsHKObject hkObject => hkObject -> IO (Id NSDictionary)
metadata hkObject =
  sendMessage hkObject metadataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKObject)
initSelector = mkSelector "init"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id HKSource)
sourceSelector = mkSelector "source"

-- | @Selector@ for @sourceRevision@
sourceRevisionSelector :: Selector '[] (Id HKSourceRevision)
sourceRevisionSelector = mkSelector "sourceRevision"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] (Id HKDevice)
deviceSelector = mkSelector "device"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSDictionary)
metadataSelector = mkSelector "metadata"

