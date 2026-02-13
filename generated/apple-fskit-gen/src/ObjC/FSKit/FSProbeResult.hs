{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents the results of a specific probe.
--
-- For any ``result`` value other than ``FSMatchResult/notRecognized``, ensure the ``name`` and ``containerID`` values are non-@nil@. When a container or volume format doesn't use a name, return an empty string. Also use an empty string in the case in which the format supports a name, but the value isn't set yet.
--
-- Some container or volume formats may lack a durable UUID on which to base a container identifier. This situation is only valid for unary file systems. In such a case, return a random UUID.
--
-- With a block device resource, a probe operation may successfully get a result but encounter an error reading the name or UUID. If this happens, use whatever information is available, and provide an empty string or random UUID for the name or container ID, respectively.
--
-- Generated bindings for @FSProbeResult@.
module ObjC.FSKit.FSProbeResult
  ( FSProbeResult
  , IsFSProbeResult(..)
  , init_
  , recognizedProbeResultWithName_containerID
  , usableButLimitedProbeResultWithName_containerID
  , usableProbeResultWithName_containerID
  , result
  , name
  , containerID
  , notRecognizedProbeResult
  , usableButLimitedProbeResult
  , containerIDSelector
  , initSelector
  , nameSelector
  , notRecognizedProbeResultSelector
  , recognizedProbeResultWithName_containerIDSelector
  , resultSelector
  , usableButLimitedProbeResultSelector
  , usableButLimitedProbeResultWithName_containerIDSelector
  , usableProbeResultWithName_containerIDSelector

  -- * Enum types
  , FSMatchResult(FSMatchResult)
  , pattern FSMatchResultNotRecognized
  , pattern FSMatchResultRecognized
  , pattern FSMatchResultUsableButLimited
  , pattern FSMatchResultUsable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.FSKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSProbeResult fsProbeResult => fsProbeResult -> IO (Id FSProbeResult)
init_ fsProbeResult =
  sendOwnedMessage fsProbeResult initSelector

-- | Creates a probe result for a recognized file system.
--
-- - Parameters:   - name: The resource name, as found during the probe operation. If the file system doesn't support names, or is awaiting naming, use an empty string.   - containerID: The container identifier, as found during the probe operation. If the file system doesn't support durable identifiers, use a random UUID.
--
-- ObjC selector: @+ recognizedProbeResultWithName:containerID:@
recognizedProbeResultWithName_containerID :: (IsNSString name, IsFSContainerIdentifier containerID) => name -> containerID -> IO (Id FSProbeResult)
recognizedProbeResultWithName_containerID name containerID =
  do
    cls' <- getRequiredClass "FSProbeResult"
    sendClassMessage cls' recognizedProbeResultWithName_containerIDSelector (toNSString name) (toFSContainerIdentifier containerID)

-- | Creates a probe result for a recognized file system that is usable, but with limited capabilities.
--
-- - Parameters:   - name: The resource name, as found during the probe operation. If the file system doesn't support names, or is awaiting naming, use an empty string.   - containerID: The container identifier, as found during the probe operation. If the file system doesn't support durable identifiers, use a random UUID.
--
-- ObjC selector: @+ usableButLimitedProbeResultWithName:containerID:@
usableButLimitedProbeResultWithName_containerID :: (IsNSString name, IsFSContainerIdentifier containerID) => name -> containerID -> IO (Id FSProbeResult)
usableButLimitedProbeResultWithName_containerID name containerID =
  do
    cls' <- getRequiredClass "FSProbeResult"
    sendClassMessage cls' usableButLimitedProbeResultWithName_containerIDSelector (toNSString name) (toFSContainerIdentifier containerID)

-- | Creates a probe result for a recognized and usable file system.
--
-- - Parameters:   - name: The resource name, as found during the probe operation. If the file system doesn't support names, or is awaiting naming, use an empty string.   - containerID: The container identifier, as found during the probe operation. If the file system doesn't support durable identifiers, use a random UUID.
--
-- ObjC selector: @+ usableProbeResultWithName:containerID:@
usableProbeResultWithName_containerID :: (IsNSString name, IsFSContainerIdentifier containerID) => name -> containerID -> IO (Id FSProbeResult)
usableProbeResultWithName_containerID name containerID =
  do
    cls' <- getRequiredClass "FSProbeResult"
    sendClassMessage cls' usableProbeResultWithName_containerIDSelector (toNSString name) (toFSContainerIdentifier containerID)

-- | The match result, representing the recognition and usability of a probed resource.
--
-- ObjC selector: @- result@
result :: IsFSProbeResult fsProbeResult => fsProbeResult -> IO FSMatchResult
result fsProbeResult =
  sendMessage fsProbeResult resultSelector

-- | The resource name, as found during the probe operation.
--
-- This value is non-@nil@ unless the ``FSProbeResult/result`` is ``FSMatchResult/notRecognized`. For formats that lack a name, this value may be an empty string. This value can also be an empty string if the format supports a name, but the value isn't set yet.
--
-- ObjC selector: @- name@
name :: IsFSProbeResult fsProbeResult => fsProbeResult -> IO (Id NSString)
name fsProbeResult =
  sendMessage fsProbeResult nameSelector

-- | The container identifier, as found during the probe operation.
--
-- This value is non-@nil@ unless the ``FSProbeResult/result`` is ``FSMatchResult/notRecognized``. For formats that lack a durable UUID on which to base a container identifier --- which is only legal for a ``FSUnaryFileSystem`` --- this value may be a random UUID.
--
-- ObjC selector: @- containerID@
containerID :: IsFSProbeResult fsProbeResult => fsProbeResult -> IO (Id FSContainerIdentifier)
containerID fsProbeResult =
  sendMessage fsProbeResult containerIDSelector

-- | A probe result for an unrecognized file system.
--
-- An unrecognized probe result contains @nil@ for its ``FSProbeResult/name`` and ``FSProbeResult/containerID`` properties.
--
-- ObjC selector: @+ notRecognizedProbeResult@
notRecognizedProbeResult :: IO (Id FSProbeResult)
notRecognizedProbeResult  =
  do
    cls' <- getRequiredClass "FSProbeResult"
    sendClassMessage cls' notRecognizedProbeResultSelector

-- | A probe result for a recognized file system that is usable, but with limited capabilities.
--
-- This kind of probe result lacks the ``FSProbeResult/name``, ``FSProbeResult/containerID``, or both. Don't return this result from probing a resource that isn't limited.
--
-- ObjC selector: @+ usableButLimitedProbeResult@
usableButLimitedProbeResult :: IO (Id FSProbeResult)
usableButLimitedProbeResult  =
  do
    cls' <- getRequiredClass "FSProbeResult"
    sendClassMessage cls' usableButLimitedProbeResultSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSProbeResult)
initSelector = mkSelector "init"

-- | @Selector@ for @recognizedProbeResultWithName:containerID:@
recognizedProbeResultWithName_containerIDSelector :: Selector '[Id NSString, Id FSContainerIdentifier] (Id FSProbeResult)
recognizedProbeResultWithName_containerIDSelector = mkSelector "recognizedProbeResultWithName:containerID:"

-- | @Selector@ for @usableButLimitedProbeResultWithName:containerID:@
usableButLimitedProbeResultWithName_containerIDSelector :: Selector '[Id NSString, Id FSContainerIdentifier] (Id FSProbeResult)
usableButLimitedProbeResultWithName_containerIDSelector = mkSelector "usableButLimitedProbeResultWithName:containerID:"

-- | @Selector@ for @usableProbeResultWithName:containerID:@
usableProbeResultWithName_containerIDSelector :: Selector '[Id NSString, Id FSContainerIdentifier] (Id FSProbeResult)
usableProbeResultWithName_containerIDSelector = mkSelector "usableProbeResultWithName:containerID:"

-- | @Selector@ for @result@
resultSelector :: Selector '[] FSMatchResult
resultSelector = mkSelector "result"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @containerID@
containerIDSelector :: Selector '[] (Id FSContainerIdentifier)
containerIDSelector = mkSelector "containerID"

-- | @Selector@ for @notRecognizedProbeResult@
notRecognizedProbeResultSelector :: Selector '[] (Id FSProbeResult)
notRecognizedProbeResultSelector = mkSelector "notRecognizedProbeResult"

-- | @Selector@ for @usableButLimitedProbeResult@
usableButLimitedProbeResultSelector :: Selector '[] (Id FSProbeResult)
usableButLimitedProbeResultSelector = mkSelector "usableButLimitedProbeResult"

