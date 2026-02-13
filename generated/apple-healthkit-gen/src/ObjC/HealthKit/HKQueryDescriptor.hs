{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKQueryDescriptor@.
module ObjC.HealthKit.HKQueryDescriptor
  ( HKQueryDescriptor
  , IsHKQueryDescriptor(..)
  , init_
  , new
  , initWithSampleType_predicate
  , sampleType
  , predicate
  , initSelector
  , initWithSampleType_predicateSelector
  , newSelector
  , predicateSelector
  , sampleTypeSelector


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
init_ :: IsHKQueryDescriptor hkQueryDescriptor => hkQueryDescriptor -> IO (Id HKQueryDescriptor)
init_ hkQueryDescriptor =
  sendOwnedMessage hkQueryDescriptor initSelector

-- | @+ new@
new :: IO (Id HKQueryDescriptor)
new  =
  do
    cls' <- getRequiredClass "HKQueryDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | initWithSampleType:predicate:
--
-- Returns a query descriptor that describes a data type and predicate to be used in an HKQuery.
--
-- @sampleType@ — The type of sample to retrieve.
--
-- @predicate@ — The predicate which samples should match.
--
-- ObjC selector: @- initWithSampleType:predicate:@
initWithSampleType_predicate :: (IsHKQueryDescriptor hkQueryDescriptor, IsHKSampleType sampleType, IsNSPredicate predicate) => hkQueryDescriptor -> sampleType -> predicate -> IO (Id HKQueryDescriptor)
initWithSampleType_predicate hkQueryDescriptor sampleType predicate =
  sendOwnedMessage hkQueryDescriptor initWithSampleType_predicateSelector (toHKSampleType sampleType) (toNSPredicate predicate)

-- | sampleType
--
-- The type of sample to retrieve in an HKQuery.
--
-- ObjC selector: @- sampleType@
sampleType :: IsHKQueryDescriptor hkQueryDescriptor => hkQueryDescriptor -> IO (Id HKSampleType)
sampleType hkQueryDescriptor =
  sendMessage hkQueryDescriptor sampleTypeSelector

-- | predicate
--
-- The predicate which samples should match.
--
-- ObjC selector: @- predicate@
predicate :: IsHKQueryDescriptor hkQueryDescriptor => hkQueryDescriptor -> IO (Id NSPredicate)
predicate hkQueryDescriptor =
  sendMessage hkQueryDescriptor predicateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKQueryDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKQueryDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSampleType:predicate:@
initWithSampleType_predicateSelector :: Selector '[Id HKSampleType, Id NSPredicate] (Id HKQueryDescriptor)
initWithSampleType_predicateSelector = mkSelector "initWithSampleType:predicate:"

-- | @Selector@ for @sampleType@
sampleTypeSelector :: Selector '[] (Id HKSampleType)
sampleTypeSelector = mkSelector "sampleType"

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

