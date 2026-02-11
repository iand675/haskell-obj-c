{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Immutable Data		***************
--
-- Generated bindings for @NSData@.
module ObjC.Foundation.NSData
  ( NSData
  , IsNSData(..)
  , getBytes
  , dataWithContentsOfMappedFile
  , initWithContentsOfMappedFile
  , initWithBase64Encoding
  , base64Encoding
  , decompressedDataUsingAlgorithm_error
  , compressedDataUsingAlgorithm_error
  , initWithBase64EncodedString_options
  , base64EncodedStringWithOptions
  , initWithBase64EncodedData_options
  , base64EncodedDataWithOptions
  , data_
  , dataWithBytes_length
  , dataWithBytesNoCopy_length
  , dataWithBytesNoCopy_length_freeWhenDone
  , dataWithContentsOfFile_options_error
  , dataWithContentsOfURL_options_error
  , dataWithContentsOfFile
  , dataWithContentsOfURL
  , initWithBytes_length
  , initWithBytesNoCopy_length
  , initWithBytesNoCopy_length_freeWhenDone
  , initWithBytesNoCopy_length_deallocator
  , initWithContentsOfFile_options_error
  , initWithContentsOfURL_options_error
  , initWithContentsOfFile
  , initWithContentsOfURL
  , initWithData
  , dataWithData
  , getBytes_length
  , getBytes_range
  , isEqualToData
  , subdataWithRange
  , writeToFile_atomically
  , writeToURL_atomically
  , writeToFile_options_error
  , writeToURL_options_error
  , rangeOfData_options_range
  , enumerateByteRangesUsingBlock
  , length_
  , description
  , getBytesSelector
  , dataWithContentsOfMappedFileSelector
  , initWithContentsOfMappedFileSelector
  , initWithBase64EncodingSelector
  , base64EncodingSelector
  , decompressedDataUsingAlgorithm_errorSelector
  , compressedDataUsingAlgorithm_errorSelector
  , initWithBase64EncodedString_optionsSelector
  , base64EncodedStringWithOptionsSelector
  , initWithBase64EncodedData_optionsSelector
  , base64EncodedDataWithOptionsSelector
  , dataSelector
  , dataWithBytes_lengthSelector
  , dataWithBytesNoCopy_lengthSelector
  , dataWithBytesNoCopy_length_freeWhenDoneSelector
  , dataWithContentsOfFile_options_errorSelector
  , dataWithContentsOfURL_options_errorSelector
  , dataWithContentsOfFileSelector
  , dataWithContentsOfURLSelector
  , initWithBytes_lengthSelector
  , initWithBytesNoCopy_lengthSelector
  , initWithBytesNoCopy_length_freeWhenDoneSelector
  , initWithBytesNoCopy_length_deallocatorSelector
  , initWithContentsOfFile_options_errorSelector
  , initWithContentsOfURL_options_errorSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , initWithDataSelector
  , dataWithDataSelector
  , getBytes_lengthSelector
  , getBytes_rangeSelector
  , isEqualToDataSelector
  , subdataWithRangeSelector
  , writeToFile_atomicallySelector
  , writeToURL_atomicallySelector
  , writeToFile_options_errorSelector
  , writeToURL_options_errorSelector
  , rangeOfData_options_rangeSelector
  , enumerateByteRangesUsingBlockSelector
  , lengthSelector
  , descriptionSelector

  -- * Enum types
  , NSDataBase64DecodingOptions(NSDataBase64DecodingOptions)
  , pattern NSDataBase64DecodingIgnoreUnknownCharacters
  , NSDataBase64EncodingOptions(NSDataBase64EncodingOptions)
  , pattern NSDataBase64Encoding64CharacterLineLength
  , pattern NSDataBase64Encoding76CharacterLineLength
  , pattern NSDataBase64EncodingEndLineWithCarriageReturn
  , pattern NSDataBase64EncodingEndLineWithLineFeed
  , NSDataCompressionAlgorithm(NSDataCompressionAlgorithm)
  , pattern NSDataCompressionAlgorithmLZFSE
  , pattern NSDataCompressionAlgorithmLZ4
  , pattern NSDataCompressionAlgorithmLZMA
  , pattern NSDataCompressionAlgorithmZlib
  , NSDataReadingOptions(NSDataReadingOptions)
  , pattern NSDataReadingMappedIfSafe
  , pattern NSDataReadingUncached
  , pattern NSDataReadingMappedAlways
  , pattern NSDataReadingMapped
  , pattern NSMappedRead
  , pattern NSUncachedRead
  , NSDataSearchOptions(NSDataSearchOptions)
  , pattern NSDataSearchBackwards
  , pattern NSDataSearchAnchored
  , NSDataWritingOptions(NSDataWritingOptions)
  , pattern NSDataWritingAtomic
  , pattern NSDataWritingWithoutOverwriting
  , pattern NSDataWritingFileProtectionNone
  , pattern NSDataWritingFileProtectionComplete
  , pattern NSDataWritingFileProtectionCompleteUnlessOpen
  , pattern NSDataWritingFileProtectionCompleteUntilFirstUserAuthentication
  , pattern NSDataWritingFileProtectionCompleteWhenUserInactive
  , pattern NSDataWritingFileProtectionMask
  , pattern NSAtomicWrite

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- getBytes:@
getBytes :: IsNSData nsData => nsData -> Ptr () -> IO ()
getBytes nsData  buffer =
  sendMsg nsData (mkSelector "getBytes:") retVoid [argPtr buffer]

-- | @+ dataWithContentsOfMappedFile:@
dataWithContentsOfMappedFile :: IsNSString path => path -> IO RawId
dataWithContentsOfMappedFile path =
  do
    cls' <- getRequiredClass "NSData"
    withObjCPtr path $ \raw_path ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "dataWithContentsOfMappedFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @- initWithContentsOfMappedFile:@
initWithContentsOfMappedFile :: (IsNSData nsData, IsNSString path) => nsData -> path -> IO RawId
initWithContentsOfMappedFile nsData  path =
withObjCPtr path $ \raw_path ->
    fmap (RawId . castPtr) $ sendMsg nsData (mkSelector "initWithContentsOfMappedFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @- initWithBase64Encoding:@
initWithBase64Encoding :: (IsNSData nsData, IsNSString base64String) => nsData -> base64String -> IO RawId
initWithBase64Encoding nsData  base64String =
withObjCPtr base64String $ \raw_base64String ->
    fmap (RawId . castPtr) $ sendMsg nsData (mkSelector "initWithBase64Encoding:") (retPtr retVoid) [argPtr (castPtr raw_base64String :: Ptr ())]

-- | @- base64Encoding@
base64Encoding :: IsNSData nsData => nsData -> IO (Id NSString)
base64Encoding nsData  =
  sendMsg nsData (mkSelector "base64Encoding") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- decompressedDataUsingAlgorithm:error:@
decompressedDataUsingAlgorithm_error :: (IsNSData nsData, IsNSError error_) => nsData -> NSDataCompressionAlgorithm -> error_ -> IO (Id NSData)
decompressedDataUsingAlgorithm_error nsData  algorithm error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsData (mkSelector "decompressedDataUsingAlgorithm:error:") (retPtr retVoid) [argCLong (coerce algorithm), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- compressedDataUsingAlgorithm:error:@
compressedDataUsingAlgorithm_error :: (IsNSData nsData, IsNSError error_) => nsData -> NSDataCompressionAlgorithm -> error_ -> IO (Id NSData)
compressedDataUsingAlgorithm_error nsData  algorithm error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsData (mkSelector "compressedDataUsingAlgorithm:error:") (retPtr retVoid) [argCLong (coerce algorithm), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithBase64EncodedString:options:@
initWithBase64EncodedString_options :: (IsNSData nsData, IsNSString base64String) => nsData -> base64String -> NSDataBase64DecodingOptions -> IO (Id NSData)
initWithBase64EncodedString_options nsData  base64String options =
withObjCPtr base64String $ \raw_base64String ->
    sendMsg nsData (mkSelector "initWithBase64EncodedString:options:") (retPtr retVoid) [argPtr (castPtr raw_base64String :: Ptr ()), argCULong (coerce options)] >>= ownedObject . castPtr

-- | @- base64EncodedStringWithOptions:@
base64EncodedStringWithOptions :: IsNSData nsData => nsData -> NSDataBase64EncodingOptions -> IO (Id NSString)
base64EncodedStringWithOptions nsData  options =
  sendMsg nsData (mkSelector "base64EncodedStringWithOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- initWithBase64EncodedData:options:@
initWithBase64EncodedData_options :: (IsNSData nsData, IsNSData base64Data) => nsData -> base64Data -> NSDataBase64DecodingOptions -> IO (Id NSData)
initWithBase64EncodedData_options nsData  base64Data options =
withObjCPtr base64Data $ \raw_base64Data ->
    sendMsg nsData (mkSelector "initWithBase64EncodedData:options:") (retPtr retVoid) [argPtr (castPtr raw_base64Data :: Ptr ()), argCULong (coerce options)] >>= ownedObject . castPtr

-- | @- base64EncodedDataWithOptions:@
base64EncodedDataWithOptions :: IsNSData nsData => nsData -> NSDataBase64EncodingOptions -> IO (Id NSData)
base64EncodedDataWithOptions nsData  options =
  sendMsg nsData (mkSelector "base64EncodedDataWithOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= retainedObject . castPtr

-- | @+ data@
data_ :: IO (Id NSData)
data_  =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMsg cls' (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ dataWithBytes:length:@
dataWithBytes_length :: Const (Ptr ()) -> CULong -> IO (Id NSData)
dataWithBytes_length bytes length_ =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMsg cls' (mkSelector "dataWithBytes:length:") (retPtr retVoid) [argPtr (unConst bytes), argCULong (fromIntegral length_)] >>= retainedObject . castPtr

-- | @+ dataWithBytesNoCopy:length:@
dataWithBytesNoCopy_length :: Ptr () -> CULong -> IO (Id NSData)
dataWithBytesNoCopy_length bytes length_ =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMsg cls' (mkSelector "dataWithBytesNoCopy:length:") (retPtr retVoid) [argPtr bytes, argCULong (fromIntegral length_)] >>= retainedObject . castPtr

-- | @+ dataWithBytesNoCopy:length:freeWhenDone:@
dataWithBytesNoCopy_length_freeWhenDone :: Ptr () -> CULong -> Bool -> IO (Id NSData)
dataWithBytesNoCopy_length_freeWhenDone bytes length_ b =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMsg cls' (mkSelector "dataWithBytesNoCopy:length:freeWhenDone:") (retPtr retVoid) [argPtr bytes, argCULong (fromIntegral length_), argCULong (if b then 1 else 0)] >>= retainedObject . castPtr

-- | @+ dataWithContentsOfFile:options:error:@
dataWithContentsOfFile_options_error :: (IsNSString path, IsNSError errorPtr) => path -> NSDataReadingOptions -> errorPtr -> IO (Id NSData)
dataWithContentsOfFile_options_error path readOptionsMask errorPtr =
  do
    cls' <- getRequiredClass "NSData"
    withObjCPtr path $ \raw_path ->
      withObjCPtr errorPtr $ \raw_errorPtr ->
        sendClassMsg cls' (mkSelector "dataWithContentsOfFile:options:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (coerce readOptionsMask), argPtr (castPtr raw_errorPtr :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dataWithContentsOfURL:options:error:@
dataWithContentsOfURL_options_error :: (IsNSURL url, IsNSError errorPtr) => url -> NSDataReadingOptions -> errorPtr -> IO (Id NSData)
dataWithContentsOfURL_options_error url readOptionsMask errorPtr =
  do
    cls' <- getRequiredClass "NSData"
    withObjCPtr url $ \raw_url ->
      withObjCPtr errorPtr $ \raw_errorPtr ->
        sendClassMsg cls' (mkSelector "dataWithContentsOfURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce readOptionsMask), argPtr (castPtr raw_errorPtr :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dataWithContentsOfFile:@
dataWithContentsOfFile :: IsNSString path => path -> IO (Id NSData)
dataWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSData"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "dataWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dataWithContentsOfURL:@
dataWithContentsOfURL :: IsNSURL url => url -> IO (Id NSData)
dataWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSData"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "dataWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithBytes:length:@
initWithBytes_length :: IsNSData nsData => nsData -> Const (Ptr ()) -> CULong -> IO (Id NSData)
initWithBytes_length nsData  bytes length_ =
  sendMsg nsData (mkSelector "initWithBytes:length:") (retPtr retVoid) [argPtr (unConst bytes), argCULong (fromIntegral length_)] >>= ownedObject . castPtr

-- | @- initWithBytesNoCopy:length:@
initWithBytesNoCopy_length :: IsNSData nsData => nsData -> Ptr () -> CULong -> IO (Id NSData)
initWithBytesNoCopy_length nsData  bytes length_ =
  sendMsg nsData (mkSelector "initWithBytesNoCopy:length:") (retPtr retVoid) [argPtr bytes, argCULong (fromIntegral length_)] >>= ownedObject . castPtr

-- | @- initWithBytesNoCopy:length:freeWhenDone:@
initWithBytesNoCopy_length_freeWhenDone :: IsNSData nsData => nsData -> Ptr () -> CULong -> Bool -> IO (Id NSData)
initWithBytesNoCopy_length_freeWhenDone nsData  bytes length_ b =
  sendMsg nsData (mkSelector "initWithBytesNoCopy:length:freeWhenDone:") (retPtr retVoid) [argPtr bytes, argCULong (fromIntegral length_), argCULong (if b then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithBytesNoCopy:length:deallocator:@
initWithBytesNoCopy_length_deallocator :: IsNSData nsData => nsData -> Ptr () -> CULong -> Ptr () -> IO (Id NSData)
initWithBytesNoCopy_length_deallocator nsData  bytes length_ deallocator =
  sendMsg nsData (mkSelector "initWithBytesNoCopy:length:deallocator:") (retPtr retVoid) [argPtr bytes, argCULong (fromIntegral length_), argPtr (castPtr deallocator :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfFile:options:error:@
initWithContentsOfFile_options_error :: (IsNSData nsData, IsNSString path, IsNSError errorPtr) => nsData -> path -> NSDataReadingOptions -> errorPtr -> IO (Id NSData)
initWithContentsOfFile_options_error nsData  path readOptionsMask errorPtr =
withObjCPtr path $ \raw_path ->
  withObjCPtr errorPtr $ \raw_errorPtr ->
      sendMsg nsData (mkSelector "initWithContentsOfFile:options:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (coerce readOptionsMask), argPtr (castPtr raw_errorPtr :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_error :: (IsNSData nsData, IsNSURL url, IsNSError errorPtr) => nsData -> url -> NSDataReadingOptions -> errorPtr -> IO (Id NSData)
initWithContentsOfURL_options_error nsData  url readOptionsMask errorPtr =
withObjCPtr url $ \raw_url ->
  withObjCPtr errorPtr $ \raw_errorPtr ->
      sendMsg nsData (mkSelector "initWithContentsOfURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce readOptionsMask), argPtr (castPtr raw_errorPtr :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSData nsData, IsNSString path) => nsData -> path -> IO (Id NSData)
initWithContentsOfFile nsData  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsData (mkSelector "initWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSData nsData, IsNSURL url) => nsData -> url -> IO (Id NSData)
initWithContentsOfURL nsData  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsData (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsNSData nsData, IsNSData data_) => nsData -> data_ -> IO (Id NSData)
initWithData nsData  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsData (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ dataWithData:@
dataWithData :: IsNSData data_ => data_ -> IO (Id NSData)
dataWithData data_ =
  do
    cls' <- getRequiredClass "NSData"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "dataWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- getBytes:length:@
getBytes_length :: IsNSData nsData => nsData -> Ptr () -> CULong -> IO ()
getBytes_length nsData  buffer length_ =
  sendMsg nsData (mkSelector "getBytes:length:") retVoid [argPtr buffer, argCULong (fromIntegral length_)]

-- | @- getBytes:range:@
getBytes_range :: IsNSData nsData => nsData -> Ptr () -> NSRange -> IO ()
getBytes_range nsData  buffer range =
  sendMsg nsData (mkSelector "getBytes:range:") retVoid [argPtr buffer, argNSRange range]

-- | @- isEqualToData:@
isEqualToData :: (IsNSData nsData, IsNSData other) => nsData -> other -> IO Bool
isEqualToData nsData  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsData (mkSelector "isEqualToData:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- subdataWithRange:@
subdataWithRange :: IsNSData nsData => nsData -> NSRange -> IO (Id NSData)
subdataWithRange nsData  range =
  sendMsg nsData (mkSelector "subdataWithRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- writeToFile:atomically:@
writeToFile_atomically :: (IsNSData nsData, IsNSString path) => nsData -> path -> Bool -> IO Bool
writeToFile_atomically nsData  path useAuxiliaryFile =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsData (mkSelector "writeToFile:atomically:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argCULong (if useAuxiliaryFile then 1 else 0)]

-- | @- writeToURL:atomically:@
writeToURL_atomically :: (IsNSData nsData, IsNSURL url) => nsData -> url -> Bool -> IO Bool
writeToURL_atomically nsData  url atomically =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsData (mkSelector "writeToURL:atomically:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (if atomically then 1 else 0)]

-- | @- writeToFile:options:error:@
writeToFile_options_error :: (IsNSData nsData, IsNSString path, IsNSError errorPtr) => nsData -> path -> NSDataWritingOptions -> errorPtr -> IO Bool
writeToFile_options_error nsData  path writeOptionsMask errorPtr =
withObjCPtr path $ \raw_path ->
  withObjCPtr errorPtr $ \raw_errorPtr ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsData (mkSelector "writeToFile:options:error:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argCULong (coerce writeOptionsMask), argPtr (castPtr raw_errorPtr :: Ptr ())]

-- | @- writeToURL:options:error:@
writeToURL_options_error :: (IsNSData nsData, IsNSURL url, IsNSError errorPtr) => nsData -> url -> NSDataWritingOptions -> errorPtr -> IO Bool
writeToURL_options_error nsData  url writeOptionsMask errorPtr =
withObjCPtr url $ \raw_url ->
  withObjCPtr errorPtr $ \raw_errorPtr ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsData (mkSelector "writeToURL:options:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce writeOptionsMask), argPtr (castPtr raw_errorPtr :: Ptr ())]

-- | @- rangeOfData:options:range:@
rangeOfData_options_range :: (IsNSData nsData, IsNSData dataToFind) => nsData -> dataToFind -> NSDataSearchOptions -> NSRange -> IO NSRange
rangeOfData_options_range nsData  dataToFind mask searchRange =
withObjCPtr dataToFind $ \raw_dataToFind ->
    sendMsgStret nsData (mkSelector "rangeOfData:options:range:") retNSRange [argPtr (castPtr raw_dataToFind :: Ptr ()), argCULong (coerce mask), argNSRange searchRange]

-- | @- enumerateByteRangesUsingBlock:@
enumerateByteRangesUsingBlock :: IsNSData nsData => nsData -> Ptr () -> IO ()
enumerateByteRangesUsingBlock nsData  block =
  sendMsg nsData (mkSelector "enumerateByteRangesUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- length@
length_ :: IsNSData nsData => nsData -> IO CULong
length_ nsData  =
  sendMsg nsData (mkSelector "length") retCULong []

-- | @- description@
description :: IsNSData nsData => nsData -> IO (Id NSString)
description nsData  =
  sendMsg nsData (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getBytes:@
getBytesSelector :: Selector
getBytesSelector = mkSelector "getBytes:"

-- | @Selector@ for @dataWithContentsOfMappedFile:@
dataWithContentsOfMappedFileSelector :: Selector
dataWithContentsOfMappedFileSelector = mkSelector "dataWithContentsOfMappedFile:"

-- | @Selector@ for @initWithContentsOfMappedFile:@
initWithContentsOfMappedFileSelector :: Selector
initWithContentsOfMappedFileSelector = mkSelector "initWithContentsOfMappedFile:"

-- | @Selector@ for @initWithBase64Encoding:@
initWithBase64EncodingSelector :: Selector
initWithBase64EncodingSelector = mkSelector "initWithBase64Encoding:"

-- | @Selector@ for @base64Encoding@
base64EncodingSelector :: Selector
base64EncodingSelector = mkSelector "base64Encoding"

-- | @Selector@ for @decompressedDataUsingAlgorithm:error:@
decompressedDataUsingAlgorithm_errorSelector :: Selector
decompressedDataUsingAlgorithm_errorSelector = mkSelector "decompressedDataUsingAlgorithm:error:"

-- | @Selector@ for @compressedDataUsingAlgorithm:error:@
compressedDataUsingAlgorithm_errorSelector :: Selector
compressedDataUsingAlgorithm_errorSelector = mkSelector "compressedDataUsingAlgorithm:error:"

-- | @Selector@ for @initWithBase64EncodedString:options:@
initWithBase64EncodedString_optionsSelector :: Selector
initWithBase64EncodedString_optionsSelector = mkSelector "initWithBase64EncodedString:options:"

-- | @Selector@ for @base64EncodedStringWithOptions:@
base64EncodedStringWithOptionsSelector :: Selector
base64EncodedStringWithOptionsSelector = mkSelector "base64EncodedStringWithOptions:"

-- | @Selector@ for @initWithBase64EncodedData:options:@
initWithBase64EncodedData_optionsSelector :: Selector
initWithBase64EncodedData_optionsSelector = mkSelector "initWithBase64EncodedData:options:"

-- | @Selector@ for @base64EncodedDataWithOptions:@
base64EncodedDataWithOptionsSelector :: Selector
base64EncodedDataWithOptionsSelector = mkSelector "base64EncodedDataWithOptions:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @dataWithBytes:length:@
dataWithBytes_lengthSelector :: Selector
dataWithBytes_lengthSelector = mkSelector "dataWithBytes:length:"

-- | @Selector@ for @dataWithBytesNoCopy:length:@
dataWithBytesNoCopy_lengthSelector :: Selector
dataWithBytesNoCopy_lengthSelector = mkSelector "dataWithBytesNoCopy:length:"

-- | @Selector@ for @dataWithBytesNoCopy:length:freeWhenDone:@
dataWithBytesNoCopy_length_freeWhenDoneSelector :: Selector
dataWithBytesNoCopy_length_freeWhenDoneSelector = mkSelector "dataWithBytesNoCopy:length:freeWhenDone:"

-- | @Selector@ for @dataWithContentsOfFile:options:error:@
dataWithContentsOfFile_options_errorSelector :: Selector
dataWithContentsOfFile_options_errorSelector = mkSelector "dataWithContentsOfFile:options:error:"

-- | @Selector@ for @dataWithContentsOfURL:options:error:@
dataWithContentsOfURL_options_errorSelector :: Selector
dataWithContentsOfURL_options_errorSelector = mkSelector "dataWithContentsOfURL:options:error:"

-- | @Selector@ for @dataWithContentsOfFile:@
dataWithContentsOfFileSelector :: Selector
dataWithContentsOfFileSelector = mkSelector "dataWithContentsOfFile:"

-- | @Selector@ for @dataWithContentsOfURL:@
dataWithContentsOfURLSelector :: Selector
dataWithContentsOfURLSelector = mkSelector "dataWithContentsOfURL:"

-- | @Selector@ for @initWithBytes:length:@
initWithBytes_lengthSelector :: Selector
initWithBytes_lengthSelector = mkSelector "initWithBytes:length:"

-- | @Selector@ for @initWithBytesNoCopy:length:@
initWithBytesNoCopy_lengthSelector :: Selector
initWithBytesNoCopy_lengthSelector = mkSelector "initWithBytesNoCopy:length:"

-- | @Selector@ for @initWithBytesNoCopy:length:freeWhenDone:@
initWithBytesNoCopy_length_freeWhenDoneSelector :: Selector
initWithBytesNoCopy_length_freeWhenDoneSelector = mkSelector "initWithBytesNoCopy:length:freeWhenDone:"

-- | @Selector@ for @initWithBytesNoCopy:length:deallocator:@
initWithBytesNoCopy_length_deallocatorSelector :: Selector
initWithBytesNoCopy_length_deallocatorSelector = mkSelector "initWithBytesNoCopy:length:deallocator:"

-- | @Selector@ for @initWithContentsOfFile:options:error:@
initWithContentsOfFile_options_errorSelector :: Selector
initWithContentsOfFile_options_errorSelector = mkSelector "initWithContentsOfFile:options:error:"

-- | @Selector@ for @initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_errorSelector :: Selector
initWithContentsOfURL_options_errorSelector = mkSelector "initWithContentsOfURL:options:error:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @dataWithData:@
dataWithDataSelector :: Selector
dataWithDataSelector = mkSelector "dataWithData:"

-- | @Selector@ for @getBytes:length:@
getBytes_lengthSelector :: Selector
getBytes_lengthSelector = mkSelector "getBytes:length:"

-- | @Selector@ for @getBytes:range:@
getBytes_rangeSelector :: Selector
getBytes_rangeSelector = mkSelector "getBytes:range:"

-- | @Selector@ for @isEqualToData:@
isEqualToDataSelector :: Selector
isEqualToDataSelector = mkSelector "isEqualToData:"

-- | @Selector@ for @subdataWithRange:@
subdataWithRangeSelector :: Selector
subdataWithRangeSelector = mkSelector "subdataWithRange:"

-- | @Selector@ for @writeToFile:atomically:@
writeToFile_atomicallySelector :: Selector
writeToFile_atomicallySelector = mkSelector "writeToFile:atomically:"

-- | @Selector@ for @writeToURL:atomically:@
writeToURL_atomicallySelector :: Selector
writeToURL_atomicallySelector = mkSelector "writeToURL:atomically:"

-- | @Selector@ for @writeToFile:options:error:@
writeToFile_options_errorSelector :: Selector
writeToFile_options_errorSelector = mkSelector "writeToFile:options:error:"

-- | @Selector@ for @writeToURL:options:error:@
writeToURL_options_errorSelector :: Selector
writeToURL_options_errorSelector = mkSelector "writeToURL:options:error:"

-- | @Selector@ for @rangeOfData:options:range:@
rangeOfData_options_rangeSelector :: Selector
rangeOfData_options_rangeSelector = mkSelector "rangeOfData:options:range:"

-- | @Selector@ for @enumerateByteRangesUsingBlock:@
enumerateByteRangesUsingBlockSelector :: Selector
enumerateByteRangesUsingBlockSelector = mkSelector "enumerateByteRangesUsingBlock:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

