{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUAudioUnitBusArray
--
-- Container for an audio unit's input or output busses.
--
-- Hosts can observe a bus property across all busses by using KVO on this object, without		having to observe it on each individual bus. (One could add listeners to individual busses,		but that means one has to observe bus count changes and add/remove listeners in response.		Also, NSArray's addObserver:toObjectsAtIndexes:forKeyPath:options:context: is problematic;		it does not let the individual objects override the observation request, and so a bus which		is proxying a bus in an extension process does not get the message.)
--
-- Some audio units (e.g. mixers) support variable numbers of busses, via subclassing. When the		bus count changes, a KVO notification is sent on "inputBusses" or "outputBusses," as		appropriate.
--
-- Subclassers should see also the AUAudioUnitBusImplementation category.
--
-- The bus array is bridged to the v2 property kAudioUnitProperty_ElementCount.
--
-- Generated bindings for @AUAudioUnitBusArray@.
module ObjC.AudioToolbox.AUAudioUnitBusArray
  ( AUAudioUnitBusArray
  , IsAUAudioUnitBusArray(..)
  , init_
  , initWithAudioUnit_busType_busses
  , initWithAudioUnit_busType
  , objectAtIndexedSubscript
  , setBusCount_error
  , addObserverToAllBusses_forKeyPath_options_context
  , removeObserverFromAllBusses_forKeyPath_context
  , replaceBusses
  , count
  , countChangeable
  , ownerAudioUnit
  , busType
  , addObserverToAllBusses_forKeyPath_options_contextSelector
  , busTypeSelector
  , countChangeableSelector
  , countSelector
  , initSelector
  , initWithAudioUnit_busTypeSelector
  , initWithAudioUnit_busType_bussesSelector
  , objectAtIndexedSubscriptSelector
  , ownerAudioUnitSelector
  , removeObserverFromAllBusses_forKeyPath_contextSelector
  , replaceBussesSelector
  , setBusCount_errorSelector

  -- * Enum types
  , AUAudioUnitBusType(AUAudioUnitBusType)
  , pattern AUAudioUnitBusTypeInput
  , pattern AUAudioUnitBusTypeOutput
  , NSKeyValueObservingOptions(NSKeyValueObservingOptions)
  , pattern NSKeyValueObservingOptionNew
  , pattern NSKeyValueObservingOptionOld
  , pattern NSKeyValueObservingOptionInitial
  , pattern NSKeyValueObservingOptionPrior

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioToolbox.Internal.Classes
import ObjC.AudioToolbox.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> IO (Id AUAudioUnitBusArray)
init_ auAudioUnitBusArray =
  sendOwnedMessage auAudioUnitBusArray initSelector

-- | initWithAudioUnit:busType:busses:
--
-- Initializes by making a copy of the supplied bus array.
--
-- ObjC selector: @- initWithAudioUnit:busType:busses:@
initWithAudioUnit_busType_busses :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsAUAudioUnit owner, IsNSArray busArray) => auAudioUnitBusArray -> owner -> AUAudioUnitBusType -> busArray -> IO (Id AUAudioUnitBusArray)
initWithAudioUnit_busType_busses auAudioUnitBusArray owner busType busArray =
  sendOwnedMessage auAudioUnitBusArray initWithAudioUnit_busType_bussesSelector (toAUAudioUnit owner) busType (toNSArray busArray)

-- | initWithAudioUnit:busType:
--
-- Initializes an empty bus array.
--
-- ObjC selector: @- initWithAudioUnit:busType:@
initWithAudioUnit_busType :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsAUAudioUnit owner) => auAudioUnitBusArray -> owner -> AUAudioUnitBusType -> IO (Id AUAudioUnitBusArray)
initWithAudioUnit_busType auAudioUnitBusArray owner busType =
  sendOwnedMessage auAudioUnitBusArray initWithAudioUnit_busTypeSelector (toAUAudioUnit owner) busType

-- | objectAtIndexedSubscript:
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> CULong -> IO (Id AUAudioUnitBus)
objectAtIndexedSubscript auAudioUnitBusArray index =
  sendMessage auAudioUnitBusArray objectAtIndexedSubscriptSelector index

-- | setBusCount:error:
--
-- Change the number of busses in the array.
--
-- ObjC selector: @- setBusCount:error:@
setBusCount_error :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsNSError outError) => auAudioUnitBusArray -> CULong -> outError -> IO Bool
setBusCount_error auAudioUnitBusArray count outError =
  sendMessage auAudioUnitBusArray setBusCount_errorSelector count (toNSError outError)

-- | addObserverToAllBusses:forKeyPath:options:context:
--
-- Add a KVO observer for a property on all busses in the array.
--
-- ObjC selector: @- addObserverToAllBusses:forKeyPath:options:context:@
addObserverToAllBusses_forKeyPath_options_context :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsNSObject observer, IsNSString keyPath) => auAudioUnitBusArray -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserverToAllBusses_forKeyPath_options_context auAudioUnitBusArray observer keyPath options context =
  sendMessage auAudioUnitBusArray addObserverToAllBusses_forKeyPath_options_contextSelector (toNSObject observer) (toNSString keyPath) options context

-- | removeObserverFromAllBusses:forKeyPath:context:
--
-- Remove a KVO observer for a property on all busses in the array.
--
-- ObjC selector: @- removeObserverFromAllBusses:forKeyPath:context:@
removeObserverFromAllBusses_forKeyPath_context :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsNSObject observer, IsNSString keyPath) => auAudioUnitBusArray -> observer -> keyPath -> Ptr () -> IO ()
removeObserverFromAllBusses_forKeyPath_context auAudioUnitBusArray observer keyPath context =
  sendMessage auAudioUnitBusArray removeObserverFromAllBusses_forKeyPath_contextSelector (toNSObject observer) (toNSString keyPath) context

-- | Sets the bus array to be a copy of the supplied array. The base class issues KVO notifications.
--
-- ObjC selector: @- replaceBusses:@
replaceBusses :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsNSArray busArray) => auAudioUnitBusArray -> busArray -> IO ()
replaceBusses auAudioUnitBusArray busArray =
  sendMessage auAudioUnitBusArray replaceBussesSelector (toNSArray busArray)

-- | count
--
-- ObjC selector: @- count@
count :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> IO CULong
count auAudioUnitBusArray =
  sendMessage auAudioUnitBusArray countSelector

-- | countChangeable
--
-- Whether the array can have a variable number of busses.
--
-- The base implementation returns false.
--
-- ObjC selector: @- countChangeable@
countChangeable :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> IO Bool
countChangeable auAudioUnitBusArray =
  sendMessage auAudioUnitBusArray countChangeableSelector

-- | The audio unit that owns the bus.
--
-- ObjC selector: @- ownerAudioUnit@
ownerAudioUnit :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> IO (Id AUAudioUnit)
ownerAudioUnit auAudioUnitBusArray =
  sendMessage auAudioUnitBusArray ownerAudioUnitSelector

-- | Which bus array this is (input or output).
--
-- ObjC selector: @- busType@
busType :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> IO AUAudioUnitBusType
busType auAudioUnitBusArray =
  sendMessage auAudioUnitBusArray busTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AUAudioUnitBusArray)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAudioUnit:busType:busses:@
initWithAudioUnit_busType_bussesSelector :: Selector '[Id AUAudioUnit, AUAudioUnitBusType, Id NSArray] (Id AUAudioUnitBusArray)
initWithAudioUnit_busType_bussesSelector = mkSelector "initWithAudioUnit:busType:busses:"

-- | @Selector@ for @initWithAudioUnit:busType:@
initWithAudioUnit_busTypeSelector :: Selector '[Id AUAudioUnit, AUAudioUnitBusType] (Id AUAudioUnitBusArray)
initWithAudioUnit_busTypeSelector = mkSelector "initWithAudioUnit:busType:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id AUAudioUnitBus)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setBusCount:error:@
setBusCount_errorSelector :: Selector '[CULong, Id NSError] Bool
setBusCount_errorSelector = mkSelector "setBusCount:error:"

-- | @Selector@ for @addObserverToAllBusses:forKeyPath:options:context:@
addObserverToAllBusses_forKeyPath_options_contextSelector :: Selector '[Id NSObject, Id NSString, NSKeyValueObservingOptions, Ptr ()] ()
addObserverToAllBusses_forKeyPath_options_contextSelector = mkSelector "addObserverToAllBusses:forKeyPath:options:context:"

-- | @Selector@ for @removeObserverFromAllBusses:forKeyPath:context:@
removeObserverFromAllBusses_forKeyPath_contextSelector :: Selector '[Id NSObject, Id NSString, Ptr ()] ()
removeObserverFromAllBusses_forKeyPath_contextSelector = mkSelector "removeObserverFromAllBusses:forKeyPath:context:"

-- | @Selector@ for @replaceBusses:@
replaceBussesSelector :: Selector '[Id NSArray] ()
replaceBussesSelector = mkSelector "replaceBusses:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @countChangeable@
countChangeableSelector :: Selector '[] Bool
countChangeableSelector = mkSelector "countChangeable"

-- | @Selector@ for @ownerAudioUnit@
ownerAudioUnitSelector :: Selector '[] (Id AUAudioUnit)
ownerAudioUnitSelector = mkSelector "ownerAudioUnit"

-- | @Selector@ for @busType@
busTypeSelector :: Selector '[] AUAudioUnitBusType
busTypeSelector = mkSelector "busType"

