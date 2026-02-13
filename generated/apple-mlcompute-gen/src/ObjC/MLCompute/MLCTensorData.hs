{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCTensorData
--
-- An object to encapsulate memory to be used as tensor data
--
-- Generated bindings for @MLCTensorData@.
module ObjC.MLCompute.MLCTensorData
  ( MLCTensorData
  , IsMLCTensorData(..)
  , new
  , init_
  , dataWithBytesNoCopy_length
  , dataWithImmutableBytesNoCopy_length
  , dataWithBytesNoCopy_length_deallocator
  , bytes
  , length_
  , bytesSelector
  , dataWithBytesNoCopy_lengthSelector
  , dataWithBytesNoCopy_length_deallocatorSelector
  , dataWithImmutableBytesNoCopy_lengthSelector
  , initSelector
  , lengthSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCTensorData)
new  =
  do
    cls' <- getRequiredClass "MLCTensorData"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCTensorData mlcTensorData => mlcTensorData -> IO (Id MLCTensorData)
init_ mlcTensorData =
  sendOwnedMessage mlcTensorData initSelector

-- | Creates a data object that holds a given number of bytes from a given buffer.
--
-- Note: The returned object will not take ownership of the @bytes@ pointer and thus will not free it on deallocation.
--
-- @bytes@ — A buffer containing data for the new object.
--
-- @length@ — The number of bytes to hold from @bytes.@ This value must not exceed the length of @bytes.@
--
-- Returns: A new @MLCTensorData@ object.
--
-- ObjC selector: @+ dataWithBytesNoCopy:length:@
dataWithBytesNoCopy_length :: Ptr () -> CULong -> IO (Id MLCTensorData)
dataWithBytesNoCopy_length bytes length_ =
  do
    cls' <- getRequiredClass "MLCTensorData"
    sendClassMessage cls' dataWithBytesNoCopy_lengthSelector bytes length_

-- | Creates a data object that holds a given number of bytes from a given buffer.
--
-- Note: The returned object will not take ownership of the @bytes@ pointer and thus will not free it on deallocation. The underlying bytes in the return object should not be mutated.
--
-- @bytes@ — A buffer containing data for the new object.
--
-- @length@ — The number of bytes to hold from @bytes.@ This value must not exceed the length of @bytes.@
--
-- Returns: A new @MLCTensorData@ object.
--
-- ObjC selector: @+ dataWithImmutableBytesNoCopy:length:@
dataWithImmutableBytesNoCopy_length :: Const (Ptr ()) -> CULong -> IO (Id MLCTensorData)
dataWithImmutableBytesNoCopy_length bytes length_ =
  do
    cls' <- getRequiredClass "MLCTensorData"
    sendClassMessage cls' dataWithImmutableBytesNoCopy_lengthSelector bytes length_

-- | Creates a data object that holds a given number of bytes from a given buffer. with a custom deallocator block.
--
-- @bytes@ — A buffer containing data for the new object.
--
-- @length@ — The number of bytes to hold from @bytes.@ This value must not exceed the length of @bytes.@
--
-- @deallocator@ — A block to invoke when the resulting object is deallocated.
--
-- Returns: A new  @MLCTensorData@ object.
--
-- ObjC selector: @+ dataWithBytesNoCopy:length:deallocator:@
dataWithBytesNoCopy_length_deallocator :: Ptr () -> CULong -> Ptr () -> IO (Id MLCTensorData)
dataWithBytesNoCopy_length_deallocator bytes length_ deallocator =
  do
    cls' <- getRequiredClass "MLCTensorData"
    sendClassMessage cls' dataWithBytesNoCopy_length_deallocatorSelector bytes length_ deallocator

-- | bytes
--
-- Pointer to memory that contains or will be used for tensor data
--
-- ObjC selector: @- bytes@
bytes :: IsMLCTensorData mlcTensorData => mlcTensorData -> IO (Ptr ())
bytes mlcTensorData =
  sendMessage mlcTensorData bytesSelector

-- | length
--
-- The size in bytes of the tensor data
--
-- ObjC selector: @- length@
length_ :: IsMLCTensorData mlcTensorData => mlcTensorData -> IO CULong
length_ mlcTensorData =
  sendMessage mlcTensorData lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCTensorData)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCTensorData)
initSelector = mkSelector "init"

-- | @Selector@ for @dataWithBytesNoCopy:length:@
dataWithBytesNoCopy_lengthSelector :: Selector '[Ptr (), CULong] (Id MLCTensorData)
dataWithBytesNoCopy_lengthSelector = mkSelector "dataWithBytesNoCopy:length:"

-- | @Selector@ for @dataWithImmutableBytesNoCopy:length:@
dataWithImmutableBytesNoCopy_lengthSelector :: Selector '[Const (Ptr ()), CULong] (Id MLCTensorData)
dataWithImmutableBytesNoCopy_lengthSelector = mkSelector "dataWithImmutableBytesNoCopy:length:"

-- | @Selector@ for @dataWithBytesNoCopy:length:deallocator:@
dataWithBytesNoCopy_length_deallocatorSelector :: Selector '[Ptr (), CULong, Ptr ()] (Id MLCTensorData)
dataWithBytesNoCopy_length_deallocatorSelector = mkSelector "dataWithBytesNoCopy:length:deallocator:"

-- | @Selector@ for @bytes@
bytesSelector :: Selector '[] (Ptr ())
bytesSelector = mkSelector "bytes"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

