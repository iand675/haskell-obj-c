{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A wrapper object for a data buffer.
--
-- This object provides a "zero-copy" buffer, for use when reading data from files. By not requiring additional buffer copying, this object reduces the extension's memory footprint and improves performance. The @FSMutableFileDataBuffer@ behaves similarly to a @uio@ in the kernel.
--
-- Generated bindings for @FSMutableFileDataBuffer@.
module ObjC.FSKit.FSMutableFileDataBuffer
  ( FSMutableFileDataBuffer
  , IsFSMutableFileDataBuffer(..)
  , init_
  , mutableBytes
  , length_
  , initSelector
  , lengthSelector
  , mutableBytesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSMutableFileDataBuffer fsMutableFileDataBuffer => fsMutableFileDataBuffer -> IO (Id FSMutableFileDataBuffer)
init_ fsMutableFileDataBuffer =
  sendOwnedMessage fsMutableFileDataBuffer initSelector

-- | The byte data.
--
-- ObjC selector: @- mutableBytes@
mutableBytes :: IsFSMutableFileDataBuffer fsMutableFileDataBuffer => fsMutableFileDataBuffer -> IO (Ptr ())
mutableBytes fsMutableFileDataBuffer =
  sendMessage fsMutableFileDataBuffer mutableBytesSelector

-- | The data length of the buffer.
--
-- ObjC selector: @- length@
length_ :: IsFSMutableFileDataBuffer fsMutableFileDataBuffer => fsMutableFileDataBuffer -> IO CULong
length_ fsMutableFileDataBuffer =
  sendMessage fsMutableFileDataBuffer lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSMutableFileDataBuffer)
initSelector = mkSelector "init"

-- | @Selector@ for @mutableBytes@
mutableBytesSelector :: Selector '[] (Ptr ())
mutableBytesSelector = mkSelector "mutableBytes"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

