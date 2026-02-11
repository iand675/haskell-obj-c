{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithAudioUnit_busType_bussesSelector
  , initWithAudioUnit_busTypeSelector
  , objectAtIndexedSubscriptSelector
  , setBusCount_errorSelector
  , addObserverToAllBusses_forKeyPath_options_contextSelector
  , removeObserverFromAllBusses_forKeyPath_contextSelector
  , replaceBussesSelector
  , countSelector
  , countChangeableSelector
  , ownerAudioUnitSelector
  , busTypeSelector

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

import ObjC.AudioToolbox.Internal.Classes
import ObjC.AudioToolbox.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> IO (Id AUAudioUnitBusArray)
init_ auAudioUnitBusArray  =
  sendMsg auAudioUnitBusArray (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithAudioUnit:busType:busses:
--
-- Initializes by making a copy of the supplied bus array.
--
-- ObjC selector: @- initWithAudioUnit:busType:busses:@
initWithAudioUnit_busType_busses :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsAUAudioUnit owner, IsNSArray busArray) => auAudioUnitBusArray -> owner -> AUAudioUnitBusType -> busArray -> IO (Id AUAudioUnitBusArray)
initWithAudioUnit_busType_busses auAudioUnitBusArray  owner busType busArray =
withObjCPtr owner $ \raw_owner ->
  withObjCPtr busArray $ \raw_busArray ->
      sendMsg auAudioUnitBusArray (mkSelector "initWithAudioUnit:busType:busses:") (retPtr retVoid) [argPtr (castPtr raw_owner :: Ptr ()), argCLong (coerce busType), argPtr (castPtr raw_busArray :: Ptr ())] >>= ownedObject . castPtr

-- | initWithAudioUnit:busType:
--
-- Initializes an empty bus array.
--
-- ObjC selector: @- initWithAudioUnit:busType:@
initWithAudioUnit_busType :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsAUAudioUnit owner) => auAudioUnitBusArray -> owner -> AUAudioUnitBusType -> IO (Id AUAudioUnitBusArray)
initWithAudioUnit_busType auAudioUnitBusArray  owner busType =
withObjCPtr owner $ \raw_owner ->
    sendMsg auAudioUnitBusArray (mkSelector "initWithAudioUnit:busType:") (retPtr retVoid) [argPtr (castPtr raw_owner :: Ptr ()), argCLong (coerce busType)] >>= ownedObject . castPtr

-- | objectAtIndexedSubscript:
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> CULong -> IO (Id AUAudioUnitBus)
objectAtIndexedSubscript auAudioUnitBusArray  index =
  sendMsg auAudioUnitBusArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | setBusCount:error:
--
-- Change the number of busses in the array.
--
-- ObjC selector: @- setBusCount:error:@
setBusCount_error :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsNSError outError) => auAudioUnitBusArray -> CULong -> outError -> IO Bool
setBusCount_error auAudioUnitBusArray  count outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg auAudioUnitBusArray (mkSelector "setBusCount:error:") retCULong [argCULong (fromIntegral count), argPtr (castPtr raw_outError :: Ptr ())]

-- | addObserverToAllBusses:forKeyPath:options:context:
--
-- Add a KVO observer for a property on all busses in the array.
--
-- ObjC selector: @- addObserverToAllBusses:forKeyPath:options:context:@
addObserverToAllBusses_forKeyPath_options_context :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsNSObject observer, IsNSString keyPath) => auAudioUnitBusArray -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserverToAllBusses_forKeyPath_options_context auAudioUnitBusArray  observer keyPath options context =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg auAudioUnitBusArray (mkSelector "addObserverToAllBusses:forKeyPath:options:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argCULong (coerce options), argPtr context]

-- | removeObserverFromAllBusses:forKeyPath:context:
--
-- Remove a KVO observer for a property on all busses in the array.
--
-- ObjC selector: @- removeObserverFromAllBusses:forKeyPath:context:@
removeObserverFromAllBusses_forKeyPath_context :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsNSObject observer, IsNSString keyPath) => auAudioUnitBusArray -> observer -> keyPath -> Ptr () -> IO ()
removeObserverFromAllBusses_forKeyPath_context auAudioUnitBusArray  observer keyPath context =
withObjCPtr observer $ \raw_observer ->
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg auAudioUnitBusArray (mkSelector "removeObserverFromAllBusses:forKeyPath:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argPtr context]

-- | Sets the bus array to be a copy of the supplied array. The base class issues KVO notifications.
--
-- ObjC selector: @- replaceBusses:@
replaceBusses :: (IsAUAudioUnitBusArray auAudioUnitBusArray, IsNSArray busArray) => auAudioUnitBusArray -> busArray -> IO ()
replaceBusses auAudioUnitBusArray  busArray =
withObjCPtr busArray $ \raw_busArray ->
    sendMsg auAudioUnitBusArray (mkSelector "replaceBusses:") retVoid [argPtr (castPtr raw_busArray :: Ptr ())]

-- | count
--
-- ObjC selector: @- count@
count :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> IO CULong
count auAudioUnitBusArray  =
  sendMsg auAudioUnitBusArray (mkSelector "count") retCULong []

-- | countChangeable
--
-- Whether the array can have a variable number of busses.
--
-- The base implementation returns false.
--
-- ObjC selector: @- countChangeable@
countChangeable :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> IO Bool
countChangeable auAudioUnitBusArray  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg auAudioUnitBusArray (mkSelector "countChangeable") retCULong []

-- | The audio unit that owns the bus.
--
-- ObjC selector: @- ownerAudioUnit@
ownerAudioUnit :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> IO (Id AUAudioUnit)
ownerAudioUnit auAudioUnitBusArray  =
  sendMsg auAudioUnitBusArray (mkSelector "ownerAudioUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Which bus array this is (input or output).
--
-- ObjC selector: @- busType@
busType :: IsAUAudioUnitBusArray auAudioUnitBusArray => auAudioUnitBusArray -> IO AUAudioUnitBusType
busType auAudioUnitBusArray  =
  fmap (coerce :: CLong -> AUAudioUnitBusType) $ sendMsg auAudioUnitBusArray (mkSelector "busType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAudioUnit:busType:busses:@
initWithAudioUnit_busType_bussesSelector :: Selector
initWithAudioUnit_busType_bussesSelector = mkSelector "initWithAudioUnit:busType:busses:"

-- | @Selector@ for @initWithAudioUnit:busType:@
initWithAudioUnit_busTypeSelector :: Selector
initWithAudioUnit_busTypeSelector = mkSelector "initWithAudioUnit:busType:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setBusCount:error:@
setBusCount_errorSelector :: Selector
setBusCount_errorSelector = mkSelector "setBusCount:error:"

-- | @Selector@ for @addObserverToAllBusses:forKeyPath:options:context:@
addObserverToAllBusses_forKeyPath_options_contextSelector :: Selector
addObserverToAllBusses_forKeyPath_options_contextSelector = mkSelector "addObserverToAllBusses:forKeyPath:options:context:"

-- | @Selector@ for @removeObserverFromAllBusses:forKeyPath:context:@
removeObserverFromAllBusses_forKeyPath_contextSelector :: Selector
removeObserverFromAllBusses_forKeyPath_contextSelector = mkSelector "removeObserverFromAllBusses:forKeyPath:context:"

-- | @Selector@ for @replaceBusses:@
replaceBussesSelector :: Selector
replaceBussesSelector = mkSelector "replaceBusses:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @countChangeable@
countChangeableSelector :: Selector
countChangeableSelector = mkSelector "countChangeable"

-- | @Selector@ for @ownerAudioUnit@
ownerAudioUnitSelector :: Selector
ownerAudioUnitSelector = mkSelector "ownerAudioUnit"

-- | @Selector@ for @busType@
busTypeSelector :: Selector
busTypeSelector = mkSelector "busType"

