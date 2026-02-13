{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Mutable Data		***************
--
-- Generated bindings for @NSMutableData@.
module ObjC.Foundation.NSMutableData
  ( NSMutableData
  , IsNSMutableData(..)
  , decompressUsingAlgorithm_error
  , compressUsingAlgorithm_error
  , dataWithCapacity
  , dataWithLength
  , initWithCapacity
  , initWithLength
  , appendBytes_length
  , appendData
  , increaseLengthBy
  , replaceBytesInRange_withBytes
  , resetBytesInRange
  , setData
  , replaceBytesInRange_withBytes_length
  , mutableBytes
  , length_
  , setLength
  , appendBytes_lengthSelector
  , appendDataSelector
  , compressUsingAlgorithm_errorSelector
  , dataWithCapacitySelector
  , dataWithLengthSelector
  , decompressUsingAlgorithm_errorSelector
  , increaseLengthBySelector
  , initWithCapacitySelector
  , initWithLengthSelector
  , lengthSelector
  , mutableBytesSelector
  , replaceBytesInRange_withBytesSelector
  , replaceBytesInRange_withBytes_lengthSelector
  , resetBytesInRangeSelector
  , setDataSelector
  , setLengthSelector

  -- * Enum types
  , NSDataCompressionAlgorithm(NSDataCompressionAlgorithm)
  , pattern NSDataCompressionAlgorithmLZFSE
  , pattern NSDataCompressionAlgorithmLZ4
  , pattern NSDataCompressionAlgorithmLZMA
  , pattern NSDataCompressionAlgorithmZlib

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- decompressUsingAlgorithm:error:@
decompressUsingAlgorithm_error :: (IsNSMutableData nsMutableData, IsNSError error_) => nsMutableData -> NSDataCompressionAlgorithm -> error_ -> IO Bool
decompressUsingAlgorithm_error nsMutableData algorithm error_ =
  sendMessage nsMutableData decompressUsingAlgorithm_errorSelector algorithm (toNSError error_)

-- | @- compressUsingAlgorithm:error:@
compressUsingAlgorithm_error :: (IsNSMutableData nsMutableData, IsNSError error_) => nsMutableData -> NSDataCompressionAlgorithm -> error_ -> IO Bool
compressUsingAlgorithm_error nsMutableData algorithm error_ =
  sendMessage nsMutableData compressUsingAlgorithm_errorSelector algorithm (toNSError error_)

-- | @+ dataWithCapacity:@
dataWithCapacity :: CULong -> IO (Id NSMutableData)
dataWithCapacity aNumItems =
  do
    cls' <- getRequiredClass "NSMutableData"
    sendClassMessage cls' dataWithCapacitySelector aNumItems

-- | @+ dataWithLength:@
dataWithLength :: CULong -> IO (Id NSMutableData)
dataWithLength length_ =
  do
    cls' <- getRequiredClass "NSMutableData"
    sendClassMessage cls' dataWithLengthSelector length_

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableData nsMutableData => nsMutableData -> CULong -> IO (Id NSMutableData)
initWithCapacity nsMutableData capacity =
  sendOwnedMessage nsMutableData initWithCapacitySelector capacity

-- | @- initWithLength:@
initWithLength :: IsNSMutableData nsMutableData => nsMutableData -> CULong -> IO (Id NSMutableData)
initWithLength nsMutableData length_ =
  sendOwnedMessage nsMutableData initWithLengthSelector length_

-- | @- appendBytes:length:@
appendBytes_length :: IsNSMutableData nsMutableData => nsMutableData -> Const (Ptr ()) -> CULong -> IO ()
appendBytes_length nsMutableData bytes length_ =
  sendMessage nsMutableData appendBytes_lengthSelector bytes length_

-- | @- appendData:@
appendData :: (IsNSMutableData nsMutableData, IsNSData other) => nsMutableData -> other -> IO ()
appendData nsMutableData other =
  sendMessage nsMutableData appendDataSelector (toNSData other)

-- | @- increaseLengthBy:@
increaseLengthBy :: IsNSMutableData nsMutableData => nsMutableData -> CULong -> IO ()
increaseLengthBy nsMutableData extraLength =
  sendMessage nsMutableData increaseLengthBySelector extraLength

-- | @- replaceBytesInRange:withBytes:@
replaceBytesInRange_withBytes :: IsNSMutableData nsMutableData => nsMutableData -> NSRange -> Const (Ptr ()) -> IO ()
replaceBytesInRange_withBytes nsMutableData range bytes =
  sendMessage nsMutableData replaceBytesInRange_withBytesSelector range bytes

-- | @- resetBytesInRange:@
resetBytesInRange :: IsNSMutableData nsMutableData => nsMutableData -> NSRange -> IO ()
resetBytesInRange nsMutableData range =
  sendMessage nsMutableData resetBytesInRangeSelector range

-- | @- setData:@
setData :: (IsNSMutableData nsMutableData, IsNSData data_) => nsMutableData -> data_ -> IO ()
setData nsMutableData data_ =
  sendMessage nsMutableData setDataSelector (toNSData data_)

-- | @- replaceBytesInRange:withBytes:length:@
replaceBytesInRange_withBytes_length :: IsNSMutableData nsMutableData => nsMutableData -> NSRange -> Const (Ptr ()) -> CULong -> IO ()
replaceBytesInRange_withBytes_length nsMutableData range replacementBytes replacementLength =
  sendMessage nsMutableData replaceBytesInRange_withBytes_lengthSelector range replacementBytes replacementLength

-- | @- mutableBytes@
mutableBytes :: IsNSMutableData nsMutableData => nsMutableData -> IO (Ptr ())
mutableBytes nsMutableData =
  sendMessage nsMutableData mutableBytesSelector

-- | @- length@
length_ :: IsNSMutableData nsMutableData => nsMutableData -> IO CULong
length_ nsMutableData =
  sendMessage nsMutableData lengthSelector

-- | @- setLength:@
setLength :: IsNSMutableData nsMutableData => nsMutableData -> CULong -> IO ()
setLength nsMutableData value =
  sendMessage nsMutableData setLengthSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @decompressUsingAlgorithm:error:@
decompressUsingAlgorithm_errorSelector :: Selector '[NSDataCompressionAlgorithm, Id NSError] Bool
decompressUsingAlgorithm_errorSelector = mkSelector "decompressUsingAlgorithm:error:"

-- | @Selector@ for @compressUsingAlgorithm:error:@
compressUsingAlgorithm_errorSelector :: Selector '[NSDataCompressionAlgorithm, Id NSError] Bool
compressUsingAlgorithm_errorSelector = mkSelector "compressUsingAlgorithm:error:"

-- | @Selector@ for @dataWithCapacity:@
dataWithCapacitySelector :: Selector '[CULong] (Id NSMutableData)
dataWithCapacitySelector = mkSelector "dataWithCapacity:"

-- | @Selector@ for @dataWithLength:@
dataWithLengthSelector :: Selector '[CULong] (Id NSMutableData)
dataWithLengthSelector = mkSelector "dataWithLength:"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector '[CULong] (Id NSMutableData)
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @initWithLength:@
initWithLengthSelector :: Selector '[CULong] (Id NSMutableData)
initWithLengthSelector = mkSelector "initWithLength:"

-- | @Selector@ for @appendBytes:length:@
appendBytes_lengthSelector :: Selector '[Const (Ptr ()), CULong] ()
appendBytes_lengthSelector = mkSelector "appendBytes:length:"

-- | @Selector@ for @appendData:@
appendDataSelector :: Selector '[Id NSData] ()
appendDataSelector = mkSelector "appendData:"

-- | @Selector@ for @increaseLengthBy:@
increaseLengthBySelector :: Selector '[CULong] ()
increaseLengthBySelector = mkSelector "increaseLengthBy:"

-- | @Selector@ for @replaceBytesInRange:withBytes:@
replaceBytesInRange_withBytesSelector :: Selector '[NSRange, Const (Ptr ())] ()
replaceBytesInRange_withBytesSelector = mkSelector "replaceBytesInRange:withBytes:"

-- | @Selector@ for @resetBytesInRange:@
resetBytesInRangeSelector :: Selector '[NSRange] ()
resetBytesInRangeSelector = mkSelector "resetBytesInRange:"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSData] ()
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @replaceBytesInRange:withBytes:length:@
replaceBytesInRange_withBytes_lengthSelector :: Selector '[NSRange, Const (Ptr ()), CULong] ()
replaceBytesInRange_withBytes_lengthSelector = mkSelector "replaceBytesInRange:withBytes:length:"

-- | @Selector@ for @mutableBytes@
mutableBytesSelector :: Selector '[] (Ptr ())
mutableBytesSelector = mkSelector "mutableBytes"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector '[CULong] ()
setLengthSelector = mkSelector "setLength:"

