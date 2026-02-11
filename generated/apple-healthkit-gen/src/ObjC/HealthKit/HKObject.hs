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
  , initSelector
  , uuidSelector
  , sourceSelector
  , sourceRevisionSelector
  , deviceSelector
  , metadataSelector


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
init_ :: IsHKObject hkObject => hkObject -> IO (Id HKObject)
init_ hkObject  =
    sendMsg hkObject (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | UUID
--
-- A unique identifier of the receiver in the HealthKit database.
--
-- ObjC selector: @- UUID@
uuid :: IsHKObject hkObject => hkObject -> IO (Id NSUUID)
uuid hkObject  =
    sendMsg hkObject (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- source@
source :: IsHKObject hkObject => hkObject -> IO (Id HKSource)
source hkObject  =
    sendMsg hkObject (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceRevision
--
-- Represents the revision of the source responsible for saving the receiver.
--
-- ObjC selector: @- sourceRevision@
sourceRevision :: IsHKObject hkObject => hkObject -> IO (Id HKSourceRevision)
sourceRevision hkObject  =
    sendMsg hkObject (mkSelector "sourceRevision") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | device
--
-- Represents the device that generated the data of the receiver.
--
-- ObjC selector: @- device@
device :: IsHKObject hkObject => hkObject -> IO (Id HKDevice)
device hkObject  =
    sendMsg hkObject (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- Extra information describing properties of the receiver.
--
-- Keys must be NSString and values must be either NSString, NSNumber, NSDate, or                HKQuantity. See HKMetadata.h for potential metadata keys and values.
--
-- ObjC selector: @- metadata@
metadata :: IsHKObject hkObject => hkObject -> IO (Id NSDictionary)
metadata hkObject  =
    sendMsg hkObject (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @sourceRevision@
sourceRevisionSelector :: Selector
sourceRevisionSelector = mkSelector "sourceRevision"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

