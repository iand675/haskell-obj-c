{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLCounterSampleBufferDescriptor
--
-- Object to represent the counter state.
--
-- Generated bindings for @MTLCounterSampleBufferDescriptor@.
module ObjC.Metal.MTLCounterSampleBufferDescriptor
  ( MTLCounterSampleBufferDescriptor
  , IsMTLCounterSampleBufferDescriptor(..)
  , counterSet
  , setCounterSet
  , label
  , setLabel
  , storageMode
  , setStorageMode
  , sampleCount
  , setSampleCount
  , counterSetSelector
  , labelSelector
  , sampleCountSelector
  , setCounterSetSelector
  , setLabelSelector
  , setSampleCountSelector
  , setStorageModeSelector
  , storageModeSelector

  -- * Enum types
  , MTLStorageMode(MTLStorageMode)
  , pattern MTLStorageModeShared
  , pattern MTLStorageModeManaged
  , pattern MTLStorageModePrivate
  , pattern MTLStorageModeMemoryless

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | counterSet The counterset to be sampled for this counter sample buffer
--
-- ObjC selector: @- counterSet@
counterSet :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> IO RawId
counterSet mtlCounterSampleBufferDescriptor =
  sendMessage mtlCounterSampleBufferDescriptor counterSetSelector

-- | counterSet The counterset to be sampled for this counter sample buffer
--
-- ObjC selector: @- setCounterSet:@
setCounterSet :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> RawId -> IO ()
setCounterSet mtlCounterSampleBufferDescriptor value =
  sendMessage mtlCounterSampleBufferDescriptor setCounterSetSelector value

-- | label A label to identify the sample buffer in debugging tools.
--
-- ObjC selector: @- label@
label :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> IO (Id NSString)
label mtlCounterSampleBufferDescriptor =
  sendMessage mtlCounterSampleBufferDescriptor labelSelector

-- | label A label to identify the sample buffer in debugging tools.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor, IsNSString value) => mtlCounterSampleBufferDescriptor -> value -> IO ()
setLabel mtlCounterSampleBufferDescriptor value =
  sendMessage mtlCounterSampleBufferDescriptor setLabelSelector (toNSString value)

-- | storageMode The storage mode for the sample buffer.  Only
--
-- MTLStorageModeShared and MTLStorageModePrivate may be used.
--
-- ObjC selector: @- storageMode@
storageMode :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> IO MTLStorageMode
storageMode mtlCounterSampleBufferDescriptor =
  sendMessage mtlCounterSampleBufferDescriptor storageModeSelector

-- | storageMode The storage mode for the sample buffer.  Only
--
-- MTLStorageModeShared and MTLStorageModePrivate may be used.
--
-- ObjC selector: @- setStorageMode:@
setStorageMode :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> MTLStorageMode -> IO ()
setStorageMode mtlCounterSampleBufferDescriptor value =
  sendMessage mtlCounterSampleBufferDescriptor setStorageModeSelector value

-- | sampleCount The number of samples that may be stored in the
--
-- counter sample buffer.
--
-- ObjC selector: @- sampleCount@
sampleCount :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> IO CULong
sampleCount mtlCounterSampleBufferDescriptor =
  sendMessage mtlCounterSampleBufferDescriptor sampleCountSelector

-- | sampleCount The number of samples that may be stored in the
--
-- counter sample buffer.
--
-- ObjC selector: @- setSampleCount:@
setSampleCount :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> CULong -> IO ()
setSampleCount mtlCounterSampleBufferDescriptor value =
  sendMessage mtlCounterSampleBufferDescriptor setSampleCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @counterSet@
counterSetSelector :: Selector '[] RawId
counterSetSelector = mkSelector "counterSet"

-- | @Selector@ for @setCounterSet:@
setCounterSetSelector :: Selector '[RawId] ()
setCounterSetSelector = mkSelector "setCounterSet:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @storageMode@
storageModeSelector :: Selector '[] MTLStorageMode
storageModeSelector = mkSelector "storageMode"

-- | @Selector@ for @setStorageMode:@
setStorageModeSelector :: Selector '[MTLStorageMode] ()
setStorageModeSelector = mkSelector "setStorageMode:"

-- | @Selector@ for @sampleCount@
sampleCountSelector :: Selector '[] CULong
sampleCountSelector = mkSelector "sampleCount"

-- | @Selector@ for @setSampleCount:@
setSampleCountSelector :: Selector '[CULong] ()
setSampleCountSelector = mkSelector "setSampleCount:"

