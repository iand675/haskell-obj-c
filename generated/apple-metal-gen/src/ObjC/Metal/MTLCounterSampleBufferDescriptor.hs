{-# LANGUAGE PatternSynonyms #-}
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
  , setCounterSetSelector
  , labelSelector
  , setLabelSelector
  , storageModeSelector
  , setStorageModeSelector
  , sampleCountSelector
  , setSampleCountSelector

  -- * Enum types
  , MTLStorageMode(MTLStorageMode)
  , pattern MTLStorageModeShared
  , pattern MTLStorageModeManaged
  , pattern MTLStorageModePrivate
  , pattern MTLStorageModeMemoryless

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | counterSet The counterset to be sampled for this counter sample buffer
--
-- ObjC selector: @- counterSet@
counterSet :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> IO RawId
counterSet mtlCounterSampleBufferDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlCounterSampleBufferDescriptor (mkSelector "counterSet") (retPtr retVoid) []

-- | counterSet The counterset to be sampled for this counter sample buffer
--
-- ObjC selector: @- setCounterSet:@
setCounterSet :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> RawId -> IO ()
setCounterSet mtlCounterSampleBufferDescriptor  value =
    sendMsg mtlCounterSampleBufferDescriptor (mkSelector "setCounterSet:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | label A label to identify the sample buffer in debugging tools.
--
-- ObjC selector: @- label@
label :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> IO (Id NSString)
label mtlCounterSampleBufferDescriptor  =
    sendMsg mtlCounterSampleBufferDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label A label to identify the sample buffer in debugging tools.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor, IsNSString value) => mtlCounterSampleBufferDescriptor -> value -> IO ()
setLabel mtlCounterSampleBufferDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlCounterSampleBufferDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | storageMode The storage mode for the sample buffer.  Only
--
-- MTLStorageModeShared and MTLStorageModePrivate may be used.
--
-- ObjC selector: @- storageMode@
storageMode :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> IO MTLStorageMode
storageMode mtlCounterSampleBufferDescriptor  =
    fmap (coerce :: CULong -> MTLStorageMode) $ sendMsg mtlCounterSampleBufferDescriptor (mkSelector "storageMode") retCULong []

-- | storageMode The storage mode for the sample buffer.  Only
--
-- MTLStorageModeShared and MTLStorageModePrivate may be used.
--
-- ObjC selector: @- setStorageMode:@
setStorageMode :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> MTLStorageMode -> IO ()
setStorageMode mtlCounterSampleBufferDescriptor  value =
    sendMsg mtlCounterSampleBufferDescriptor (mkSelector "setStorageMode:") retVoid [argCULong (coerce value)]

-- | sampleCount The number of samples that may be stored in the
--
-- counter sample buffer.
--
-- ObjC selector: @- sampleCount@
sampleCount :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> IO CULong
sampleCount mtlCounterSampleBufferDescriptor  =
    sendMsg mtlCounterSampleBufferDescriptor (mkSelector "sampleCount") retCULong []

-- | sampleCount The number of samples that may be stored in the
--
-- counter sample buffer.
--
-- ObjC selector: @- setSampleCount:@
setSampleCount :: IsMTLCounterSampleBufferDescriptor mtlCounterSampleBufferDescriptor => mtlCounterSampleBufferDescriptor -> CULong -> IO ()
setSampleCount mtlCounterSampleBufferDescriptor  value =
    sendMsg mtlCounterSampleBufferDescriptor (mkSelector "setSampleCount:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @counterSet@
counterSetSelector :: Selector
counterSetSelector = mkSelector "counterSet"

-- | @Selector@ for @setCounterSet:@
setCounterSetSelector :: Selector
setCounterSetSelector = mkSelector "setCounterSet:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @storageMode@
storageModeSelector :: Selector
storageModeSelector = mkSelector "storageMode"

-- | @Selector@ for @setStorageMode:@
setStorageModeSelector :: Selector
setStorageModeSelector = mkSelector "setStorageMode:"

-- | @Selector@ for @sampleCount@
sampleCountSelector :: Selector
sampleCountSelector = mkSelector "sampleCount"

-- | @Selector@ for @setSampleCount:@
setSampleCountSelector :: Selector
setSampleCountSelector = mkSelector "setSampleCount:"

