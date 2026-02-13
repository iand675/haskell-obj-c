{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKLiveWorkoutDataSource
--
-- An HKLiveWorkoutDataSource is to be used with an HKWorkoutBuilder to automatically collect samples
--
-- Generated bindings for @HKLiveWorkoutDataSource@.
module ObjC.HealthKit.HKLiveWorkoutDataSource
  ( HKLiveWorkoutDataSource
  , IsHKLiveWorkoutDataSource(..)
  , init_
  , initWithHealthStore_workoutConfiguration
  , enableCollectionForType_predicate
  , disableCollectionForType
  , typesToCollect
  , disableCollectionForTypeSelector
  , enableCollectionForType_predicateSelector
  , initSelector
  , initWithHealthStore_workoutConfigurationSelector
  , typesToCollectSelector


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
init_ :: IsHKLiveWorkoutDataSource hkLiveWorkoutDataSource => hkLiveWorkoutDataSource -> IO (Id HKLiveWorkoutDataSource)
init_ hkLiveWorkoutDataSource =
  sendOwnedMessage hkLiveWorkoutDataSource initSelector

-- | initWithHealthStore:workoutConfiguration:
--
-- The designated initializer of HKLiveWorkoutDataSource.
--
-- @healthStore@ — The HKHealthStore. This should match the one used to create the corresponding                                HKWorkoutBuilder.
--
-- @configuration@ — An optional workout configuration. typesToCollect will be populated with default                                types for the workout configuration
--
-- ObjC selector: @- initWithHealthStore:workoutConfiguration:@
initWithHealthStore_workoutConfiguration :: (IsHKLiveWorkoutDataSource hkLiveWorkoutDataSource, IsHKHealthStore healthStore, IsHKWorkoutConfiguration configuration) => hkLiveWorkoutDataSource -> healthStore -> configuration -> IO (Id HKLiveWorkoutDataSource)
initWithHealthStore_workoutConfiguration hkLiveWorkoutDataSource healthStore configuration =
  sendOwnedMessage hkLiveWorkoutDataSource initWithHealthStore_workoutConfigurationSelector (toHKHealthStore healthStore) (toHKWorkoutConfiguration configuration)

-- | enableCollectionForType:predicate
--
-- Adds a new type of quantity sample to collect.
--
-- Calling this method for a type that is already being collected will override the predicate for that type.
--
-- @quantityType@ — The type of sample to collect.
--
-- @predicate@ — If non-nil, collected samples must match this predicate.
--
-- ObjC selector: @- enableCollectionForType:predicate:@
enableCollectionForType_predicate :: (IsHKLiveWorkoutDataSource hkLiveWorkoutDataSource, IsHKQuantityType quantityType, IsNSPredicate predicate) => hkLiveWorkoutDataSource -> quantityType -> predicate -> IO ()
enableCollectionForType_predicate hkLiveWorkoutDataSource quantityType predicate =
  sendMessage hkLiveWorkoutDataSource enableCollectionForType_predicateSelector (toHKQuantityType quantityType) (toNSPredicate predicate)

-- | disableCollectionForType:
--
-- Removes the specified quantity type from the types to collect.
--
-- @quantityType@ — The type of sample to no longer collect.
--
-- ObjC selector: @- disableCollectionForType:@
disableCollectionForType :: (IsHKLiveWorkoutDataSource hkLiveWorkoutDataSource, IsHKQuantityType quantityType) => hkLiveWorkoutDataSource -> quantityType -> IO ()
disableCollectionForType hkLiveWorkoutDataSource quantityType =
  sendMessage hkLiveWorkoutDataSource disableCollectionForTypeSelector (toHKQuantityType quantityType)

-- | typesToCollect
--
-- The quantity types the receiver is collecting.
--
-- ObjC selector: @- typesToCollect@
typesToCollect :: IsHKLiveWorkoutDataSource hkLiveWorkoutDataSource => hkLiveWorkoutDataSource -> IO (Id NSSet)
typesToCollect hkLiveWorkoutDataSource =
  sendMessage hkLiveWorkoutDataSource typesToCollectSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKLiveWorkoutDataSource)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithHealthStore:workoutConfiguration:@
initWithHealthStore_workoutConfigurationSelector :: Selector '[Id HKHealthStore, Id HKWorkoutConfiguration] (Id HKLiveWorkoutDataSource)
initWithHealthStore_workoutConfigurationSelector = mkSelector "initWithHealthStore:workoutConfiguration:"

-- | @Selector@ for @enableCollectionForType:predicate:@
enableCollectionForType_predicateSelector :: Selector '[Id HKQuantityType, Id NSPredicate] ()
enableCollectionForType_predicateSelector = mkSelector "enableCollectionForType:predicate:"

-- | @Selector@ for @disableCollectionForType:@
disableCollectionForTypeSelector :: Selector '[Id HKQuantityType] ()
disableCollectionForTypeSelector = mkSelector "disableCollectionForType:"

-- | @Selector@ for @typesToCollect@
typesToCollectSelector :: Selector '[] (Id NSSet)
typesToCollectSelector = mkSelector "typesToCollect"

