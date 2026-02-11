{-# LANGUAGE PatternSynonyms #-}
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
  , decompressUsingAlgorithm_errorSelector
  , compressUsingAlgorithm_errorSelector
  , dataWithCapacitySelector
  , dataWithLengthSelector
  , initWithCapacitySelector
  , initWithLengthSelector
  , appendBytes_lengthSelector
  , appendDataSelector
  , increaseLengthBySelector
  , replaceBytesInRange_withBytesSelector
  , resetBytesInRangeSelector
  , setDataSelector
  , replaceBytesInRange_withBytes_lengthSelector
  , mutableBytesSelector
  , lengthSelector
  , setLengthSelector

  -- * Enum types
  , NSDataCompressionAlgorithm(NSDataCompressionAlgorithm)
  , pattern NSDataCompressionAlgorithmLZFSE
  , pattern NSDataCompressionAlgorithmLZ4
  , pattern NSDataCompressionAlgorithmLZMA
  , pattern NSDataCompressionAlgorithmZlib

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- decompressUsingAlgorithm:error:@
decompressUsingAlgorithm_error :: (IsNSMutableData nsMutableData, IsNSError error_) => nsMutableData -> NSDataCompressionAlgorithm -> error_ -> IO Bool
decompressUsingAlgorithm_error nsMutableData  algorithm error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableData (mkSelector "decompressUsingAlgorithm:error:") retCULong [argCLong (coerce algorithm), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- compressUsingAlgorithm:error:@
compressUsingAlgorithm_error :: (IsNSMutableData nsMutableData, IsNSError error_) => nsMutableData -> NSDataCompressionAlgorithm -> error_ -> IO Bool
compressUsingAlgorithm_error nsMutableData  algorithm error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMutableData (mkSelector "compressUsingAlgorithm:error:") retCULong [argCLong (coerce algorithm), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ dataWithCapacity:@
dataWithCapacity :: CULong -> IO (Id NSMutableData)
dataWithCapacity aNumItems =
  do
    cls' <- getRequiredClass "NSMutableData"
    sendClassMsg cls' (mkSelector "dataWithCapacity:") (retPtr retVoid) [argCULong aNumItems] >>= retainedObject . castPtr

-- | @+ dataWithLength:@
dataWithLength :: CULong -> IO (Id NSMutableData)
dataWithLength length_ =
  do
    cls' <- getRequiredClass "NSMutableData"
    sendClassMsg cls' (mkSelector "dataWithLength:") (retPtr retVoid) [argCULong length_] >>= retainedObject . castPtr

-- | @- initWithCapacity:@
initWithCapacity :: IsNSMutableData nsMutableData => nsMutableData -> CULong -> IO (Id NSMutableData)
initWithCapacity nsMutableData  capacity =
    sendMsg nsMutableData (mkSelector "initWithCapacity:") (retPtr retVoid) [argCULong capacity] >>= ownedObject . castPtr

-- | @- initWithLength:@
initWithLength :: IsNSMutableData nsMutableData => nsMutableData -> CULong -> IO (Id NSMutableData)
initWithLength nsMutableData  length_ =
    sendMsg nsMutableData (mkSelector "initWithLength:") (retPtr retVoid) [argCULong length_] >>= ownedObject . castPtr

-- | @- appendBytes:length:@
appendBytes_length :: IsNSMutableData nsMutableData => nsMutableData -> Const (Ptr ()) -> CULong -> IO ()
appendBytes_length nsMutableData  bytes length_ =
    sendMsg nsMutableData (mkSelector "appendBytes:length:") retVoid [argPtr (unConst bytes), argCULong length_]

-- | @- appendData:@
appendData :: (IsNSMutableData nsMutableData, IsNSData other) => nsMutableData -> other -> IO ()
appendData nsMutableData  other =
  withObjCPtr other $ \raw_other ->
      sendMsg nsMutableData (mkSelector "appendData:") retVoid [argPtr (castPtr raw_other :: Ptr ())]

-- | @- increaseLengthBy:@
increaseLengthBy :: IsNSMutableData nsMutableData => nsMutableData -> CULong -> IO ()
increaseLengthBy nsMutableData  extraLength =
    sendMsg nsMutableData (mkSelector "increaseLengthBy:") retVoid [argCULong extraLength]

-- | @- replaceBytesInRange:withBytes:@
replaceBytesInRange_withBytes :: IsNSMutableData nsMutableData => nsMutableData -> NSRange -> Const (Ptr ()) -> IO ()
replaceBytesInRange_withBytes nsMutableData  range bytes =
    sendMsg nsMutableData (mkSelector "replaceBytesInRange:withBytes:") retVoid [argNSRange range, argPtr (unConst bytes)]

-- | @- resetBytesInRange:@
resetBytesInRange :: IsNSMutableData nsMutableData => nsMutableData -> NSRange -> IO ()
resetBytesInRange nsMutableData  range =
    sendMsg nsMutableData (mkSelector "resetBytesInRange:") retVoid [argNSRange range]

-- | @- setData:@
setData :: (IsNSMutableData nsMutableData, IsNSData data_) => nsMutableData -> data_ -> IO ()
setData nsMutableData  data_ =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg nsMutableData (mkSelector "setData:") retVoid [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- replaceBytesInRange:withBytes:length:@
replaceBytesInRange_withBytes_length :: IsNSMutableData nsMutableData => nsMutableData -> NSRange -> Const (Ptr ()) -> CULong -> IO ()
replaceBytesInRange_withBytes_length nsMutableData  range replacementBytes replacementLength =
    sendMsg nsMutableData (mkSelector "replaceBytesInRange:withBytes:length:") retVoid [argNSRange range, argPtr (unConst replacementBytes), argCULong replacementLength]

-- | @- mutableBytes@
mutableBytes :: IsNSMutableData nsMutableData => nsMutableData -> IO (Ptr ())
mutableBytes nsMutableData  =
    fmap castPtr $ sendMsg nsMutableData (mkSelector "mutableBytes") (retPtr retVoid) []

-- | @- length@
length_ :: IsNSMutableData nsMutableData => nsMutableData -> IO CULong
length_ nsMutableData  =
    sendMsg nsMutableData (mkSelector "length") retCULong []

-- | @- setLength:@
setLength :: IsNSMutableData nsMutableData => nsMutableData -> CULong -> IO ()
setLength nsMutableData  value =
    sendMsg nsMutableData (mkSelector "setLength:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @decompressUsingAlgorithm:error:@
decompressUsingAlgorithm_errorSelector :: Selector
decompressUsingAlgorithm_errorSelector = mkSelector "decompressUsingAlgorithm:error:"

-- | @Selector@ for @compressUsingAlgorithm:error:@
compressUsingAlgorithm_errorSelector :: Selector
compressUsingAlgorithm_errorSelector = mkSelector "compressUsingAlgorithm:error:"

-- | @Selector@ for @dataWithCapacity:@
dataWithCapacitySelector :: Selector
dataWithCapacitySelector = mkSelector "dataWithCapacity:"

-- | @Selector@ for @dataWithLength:@
dataWithLengthSelector :: Selector
dataWithLengthSelector = mkSelector "dataWithLength:"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @initWithLength:@
initWithLengthSelector :: Selector
initWithLengthSelector = mkSelector "initWithLength:"

-- | @Selector@ for @appendBytes:length:@
appendBytes_lengthSelector :: Selector
appendBytes_lengthSelector = mkSelector "appendBytes:length:"

-- | @Selector@ for @appendData:@
appendDataSelector :: Selector
appendDataSelector = mkSelector "appendData:"

-- | @Selector@ for @increaseLengthBy:@
increaseLengthBySelector :: Selector
increaseLengthBySelector = mkSelector "increaseLengthBy:"

-- | @Selector@ for @replaceBytesInRange:withBytes:@
replaceBytesInRange_withBytesSelector :: Selector
replaceBytesInRange_withBytesSelector = mkSelector "replaceBytesInRange:withBytes:"

-- | @Selector@ for @resetBytesInRange:@
resetBytesInRangeSelector :: Selector
resetBytesInRangeSelector = mkSelector "resetBytesInRange:"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @replaceBytesInRange:withBytes:length:@
replaceBytesInRange_withBytes_lengthSelector :: Selector
replaceBytesInRange_withBytes_lengthSelector = mkSelector "replaceBytesInRange:withBytes:length:"

-- | @Selector@ for @mutableBytes@
mutableBytesSelector :: Selector
mutableBytesSelector = mkSelector "mutableBytes"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector
setLengthSelector = mkSelector "setLength:"

