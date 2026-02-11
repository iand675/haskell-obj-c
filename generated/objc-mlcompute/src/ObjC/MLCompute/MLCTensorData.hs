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
  , newSelector
  , initSelector
  , dataWithBytesNoCopy_lengthSelector
  , dataWithImmutableBytesNoCopy_lengthSelector
  , dataWithBytesNoCopy_length_deallocatorSelector
  , bytesSelector
  , lengthSelector


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

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCTensorData)
new  =
  do
    cls' <- getRequiredClass "MLCTensorData"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCTensorData mlcTensorData => mlcTensorData -> IO (Id MLCTensorData)
init_ mlcTensorData  =
  sendMsg mlcTensorData (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "dataWithBytesNoCopy:length:") (retPtr retVoid) [argPtr bytes, argCULong (fromIntegral length_)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "dataWithImmutableBytesNoCopy:length:") (retPtr retVoid) [argPtr (unConst bytes), argCULong (fromIntegral length_)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "dataWithBytesNoCopy:length:deallocator:") (retPtr retVoid) [argPtr bytes, argCULong (fromIntegral length_), argPtr (castPtr deallocator :: Ptr ())] >>= retainedObject . castPtr

-- | bytes
--
-- Pointer to memory that contains or will be used for tensor data
--
-- ObjC selector: @- bytes@
bytes :: IsMLCTensorData mlcTensorData => mlcTensorData -> IO (Ptr ())
bytes mlcTensorData  =
  fmap castPtr $ sendMsg mlcTensorData (mkSelector "bytes") (retPtr retVoid) []

-- | length
--
-- The size in bytes of the tensor data
--
-- ObjC selector: @- length@
length_ :: IsMLCTensorData mlcTensorData => mlcTensorData -> IO CULong
length_ mlcTensorData  =
  sendMsg mlcTensorData (mkSelector "length") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @dataWithBytesNoCopy:length:@
dataWithBytesNoCopy_lengthSelector :: Selector
dataWithBytesNoCopy_lengthSelector = mkSelector "dataWithBytesNoCopy:length:"

-- | @Selector@ for @dataWithImmutableBytesNoCopy:length:@
dataWithImmutableBytesNoCopy_lengthSelector :: Selector
dataWithImmutableBytesNoCopy_lengthSelector = mkSelector "dataWithImmutableBytesNoCopy:length:"

-- | @Selector@ for @dataWithBytesNoCopy:length:deallocator:@
dataWithBytesNoCopy_length_deallocatorSelector :: Selector
dataWithBytesNoCopy_length_deallocatorSelector = mkSelector "dataWithBytesNoCopy:length:deallocator:"

-- | @Selector@ for @bytes@
bytesSelector :: Selector
bytesSelector = mkSelector "bytes"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

