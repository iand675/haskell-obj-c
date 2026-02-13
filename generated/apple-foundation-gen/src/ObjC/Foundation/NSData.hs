{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , bytes
  , description
  , base64EncodedDataWithOptionsSelector
  , base64EncodedStringWithOptionsSelector
  , base64EncodingSelector
  , bytesSelector
  , compressedDataUsingAlgorithm_errorSelector
  , dataSelector
  , dataWithBytesNoCopy_lengthSelector
  , dataWithBytesNoCopy_length_freeWhenDoneSelector
  , dataWithBytes_lengthSelector
  , dataWithContentsOfFileSelector
  , dataWithContentsOfFile_options_errorSelector
  , dataWithContentsOfMappedFileSelector
  , dataWithContentsOfURLSelector
  , dataWithContentsOfURL_options_errorSelector
  , dataWithDataSelector
  , decompressedDataUsingAlgorithm_errorSelector
  , descriptionSelector
  , enumerateByteRangesUsingBlockSelector
  , getBytesSelector
  , getBytes_lengthSelector
  , getBytes_rangeSelector
  , initWithBase64EncodedData_optionsSelector
  , initWithBase64EncodedString_optionsSelector
  , initWithBase64EncodingSelector
  , initWithBytesNoCopy_lengthSelector
  , initWithBytesNoCopy_length_deallocatorSelector
  , initWithBytesNoCopy_length_freeWhenDoneSelector
  , initWithBytes_lengthSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfFile_options_errorSelector
  , initWithContentsOfMappedFileSelector
  , initWithContentsOfURLSelector
  , initWithContentsOfURL_options_errorSelector
  , initWithDataSelector
  , isEqualToDataSelector
  , lengthSelector
  , rangeOfData_options_rangeSelector
  , subdataWithRangeSelector
  , writeToFile_atomicallySelector
  , writeToFile_options_errorSelector
  , writeToURL_atomicallySelector
  , writeToURL_options_errorSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- getBytes:@
getBytes :: IsNSData nsData => nsData -> Ptr () -> IO ()
getBytes nsData buffer =
  sendMessage nsData getBytesSelector buffer

-- | @+ dataWithContentsOfMappedFile:@
dataWithContentsOfMappedFile :: IsNSString path => path -> IO RawId
dataWithContentsOfMappedFile path =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMessage cls' dataWithContentsOfMappedFileSelector (toNSString path)

-- | @- initWithContentsOfMappedFile:@
initWithContentsOfMappedFile :: (IsNSData nsData, IsNSString path) => nsData -> path -> IO RawId
initWithContentsOfMappedFile nsData path =
  sendOwnedMessage nsData initWithContentsOfMappedFileSelector (toNSString path)

-- | @- initWithBase64Encoding:@
initWithBase64Encoding :: (IsNSData nsData, IsNSString base64String) => nsData -> base64String -> IO RawId
initWithBase64Encoding nsData base64String =
  sendOwnedMessage nsData initWithBase64EncodingSelector (toNSString base64String)

-- | @- base64Encoding@
base64Encoding :: IsNSData nsData => nsData -> IO (Id NSString)
base64Encoding nsData =
  sendMessage nsData base64EncodingSelector

-- | @- decompressedDataUsingAlgorithm:error:@
decompressedDataUsingAlgorithm_error :: (IsNSData nsData, IsNSError error_) => nsData -> NSDataCompressionAlgorithm -> error_ -> IO (Id NSData)
decompressedDataUsingAlgorithm_error nsData algorithm error_ =
  sendMessage nsData decompressedDataUsingAlgorithm_errorSelector algorithm (toNSError error_)

-- | @- compressedDataUsingAlgorithm:error:@
compressedDataUsingAlgorithm_error :: (IsNSData nsData, IsNSError error_) => nsData -> NSDataCompressionAlgorithm -> error_ -> IO (Id NSData)
compressedDataUsingAlgorithm_error nsData algorithm error_ =
  sendMessage nsData compressedDataUsingAlgorithm_errorSelector algorithm (toNSError error_)

-- | @- initWithBase64EncodedString:options:@
initWithBase64EncodedString_options :: (IsNSData nsData, IsNSString base64String) => nsData -> base64String -> NSDataBase64DecodingOptions -> IO (Id NSData)
initWithBase64EncodedString_options nsData base64String options =
  sendOwnedMessage nsData initWithBase64EncodedString_optionsSelector (toNSString base64String) options

-- | @- base64EncodedStringWithOptions:@
base64EncodedStringWithOptions :: IsNSData nsData => nsData -> NSDataBase64EncodingOptions -> IO (Id NSString)
base64EncodedStringWithOptions nsData options =
  sendMessage nsData base64EncodedStringWithOptionsSelector options

-- | @- initWithBase64EncodedData:options:@
initWithBase64EncodedData_options :: (IsNSData nsData, IsNSData base64Data) => nsData -> base64Data -> NSDataBase64DecodingOptions -> IO (Id NSData)
initWithBase64EncodedData_options nsData base64Data options =
  sendOwnedMessage nsData initWithBase64EncodedData_optionsSelector (toNSData base64Data) options

-- | @- base64EncodedDataWithOptions:@
base64EncodedDataWithOptions :: IsNSData nsData => nsData -> NSDataBase64EncodingOptions -> IO (Id NSData)
base64EncodedDataWithOptions nsData options =
  sendMessage nsData base64EncodedDataWithOptionsSelector options

-- | @+ data@
data_ :: IO (Id NSData)
data_  =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMessage cls' dataSelector

-- | @+ dataWithBytes:length:@
dataWithBytes_length :: Const (Ptr ()) -> CULong -> IO (Id NSData)
dataWithBytes_length bytes length_ =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMessage cls' dataWithBytes_lengthSelector bytes length_

-- | @+ dataWithBytesNoCopy:length:@
dataWithBytesNoCopy_length :: Ptr () -> CULong -> IO (Id NSData)
dataWithBytesNoCopy_length bytes length_ =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMessage cls' dataWithBytesNoCopy_lengthSelector bytes length_

-- | @+ dataWithBytesNoCopy:length:freeWhenDone:@
dataWithBytesNoCopy_length_freeWhenDone :: Ptr () -> CULong -> Bool -> IO (Id NSData)
dataWithBytesNoCopy_length_freeWhenDone bytes length_ b =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMessage cls' dataWithBytesNoCopy_length_freeWhenDoneSelector bytes length_ b

-- | @+ dataWithContentsOfFile:options:error:@
dataWithContentsOfFile_options_error :: (IsNSString path, IsNSError errorPtr) => path -> NSDataReadingOptions -> errorPtr -> IO (Id NSData)
dataWithContentsOfFile_options_error path readOptionsMask errorPtr =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMessage cls' dataWithContentsOfFile_options_errorSelector (toNSString path) readOptionsMask (toNSError errorPtr)

-- | @+ dataWithContentsOfURL:options:error:@
dataWithContentsOfURL_options_error :: (IsNSURL url, IsNSError errorPtr) => url -> NSDataReadingOptions -> errorPtr -> IO (Id NSData)
dataWithContentsOfURL_options_error url readOptionsMask errorPtr =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMessage cls' dataWithContentsOfURL_options_errorSelector (toNSURL url) readOptionsMask (toNSError errorPtr)

-- | @+ dataWithContentsOfFile:@
dataWithContentsOfFile :: IsNSString path => path -> IO (Id NSData)
dataWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMessage cls' dataWithContentsOfFileSelector (toNSString path)

-- | @+ dataWithContentsOfURL:@
dataWithContentsOfURL :: IsNSURL url => url -> IO (Id NSData)
dataWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMessage cls' dataWithContentsOfURLSelector (toNSURL url)

-- | @- initWithBytes:length:@
initWithBytes_length :: IsNSData nsData => nsData -> Const (Ptr ()) -> CULong -> IO (Id NSData)
initWithBytes_length nsData bytes length_ =
  sendOwnedMessage nsData initWithBytes_lengthSelector bytes length_

-- | @- initWithBytesNoCopy:length:@
initWithBytesNoCopy_length :: IsNSData nsData => nsData -> Ptr () -> CULong -> IO (Id NSData)
initWithBytesNoCopy_length nsData bytes length_ =
  sendOwnedMessage nsData initWithBytesNoCopy_lengthSelector bytes length_

-- | @- initWithBytesNoCopy:length:freeWhenDone:@
initWithBytesNoCopy_length_freeWhenDone :: IsNSData nsData => nsData -> Ptr () -> CULong -> Bool -> IO (Id NSData)
initWithBytesNoCopy_length_freeWhenDone nsData bytes length_ b =
  sendOwnedMessage nsData initWithBytesNoCopy_length_freeWhenDoneSelector bytes length_ b

-- | @- initWithBytesNoCopy:length:deallocator:@
initWithBytesNoCopy_length_deallocator :: IsNSData nsData => nsData -> Ptr () -> CULong -> Ptr () -> IO (Id NSData)
initWithBytesNoCopy_length_deallocator nsData bytes length_ deallocator =
  sendOwnedMessage nsData initWithBytesNoCopy_length_deallocatorSelector bytes length_ deallocator

-- | @- initWithContentsOfFile:options:error:@
initWithContentsOfFile_options_error :: (IsNSData nsData, IsNSString path, IsNSError errorPtr) => nsData -> path -> NSDataReadingOptions -> errorPtr -> IO (Id NSData)
initWithContentsOfFile_options_error nsData path readOptionsMask errorPtr =
  sendOwnedMessage nsData initWithContentsOfFile_options_errorSelector (toNSString path) readOptionsMask (toNSError errorPtr)

-- | @- initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_error :: (IsNSData nsData, IsNSURL url, IsNSError errorPtr) => nsData -> url -> NSDataReadingOptions -> errorPtr -> IO (Id NSData)
initWithContentsOfURL_options_error nsData url readOptionsMask errorPtr =
  sendOwnedMessage nsData initWithContentsOfURL_options_errorSelector (toNSURL url) readOptionsMask (toNSError errorPtr)

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSData nsData, IsNSString path) => nsData -> path -> IO (Id NSData)
initWithContentsOfFile nsData path =
  sendOwnedMessage nsData initWithContentsOfFileSelector (toNSString path)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSData nsData, IsNSURL url) => nsData -> url -> IO (Id NSData)
initWithContentsOfURL nsData url =
  sendOwnedMessage nsData initWithContentsOfURLSelector (toNSURL url)

-- | @- initWithData:@
initWithData :: (IsNSData nsData, IsNSData data_) => nsData -> data_ -> IO (Id NSData)
initWithData nsData data_ =
  sendOwnedMessage nsData initWithDataSelector (toNSData data_)

-- | @+ dataWithData:@
dataWithData :: IsNSData data_ => data_ -> IO (Id NSData)
dataWithData data_ =
  do
    cls' <- getRequiredClass "NSData"
    sendClassMessage cls' dataWithDataSelector (toNSData data_)

-- | @- getBytes:length:@
getBytes_length :: IsNSData nsData => nsData -> Ptr () -> CULong -> IO ()
getBytes_length nsData buffer length_ =
  sendMessage nsData getBytes_lengthSelector buffer length_

-- | @- getBytes:range:@
getBytes_range :: IsNSData nsData => nsData -> Ptr () -> NSRange -> IO ()
getBytes_range nsData buffer range =
  sendMessage nsData getBytes_rangeSelector buffer range

-- | @- isEqualToData:@
isEqualToData :: (IsNSData nsData, IsNSData other) => nsData -> other -> IO Bool
isEqualToData nsData other =
  sendMessage nsData isEqualToDataSelector (toNSData other)

-- | @- subdataWithRange:@
subdataWithRange :: IsNSData nsData => nsData -> NSRange -> IO (Id NSData)
subdataWithRange nsData range =
  sendMessage nsData subdataWithRangeSelector range

-- | @- writeToFile:atomically:@
writeToFile_atomically :: (IsNSData nsData, IsNSString path) => nsData -> path -> Bool -> IO Bool
writeToFile_atomically nsData path useAuxiliaryFile =
  sendMessage nsData writeToFile_atomicallySelector (toNSString path) useAuxiliaryFile

-- | @- writeToURL:atomically:@
writeToURL_atomically :: (IsNSData nsData, IsNSURL url) => nsData -> url -> Bool -> IO Bool
writeToURL_atomically nsData url atomically =
  sendMessage nsData writeToURL_atomicallySelector (toNSURL url) atomically

-- | @- writeToFile:options:error:@
writeToFile_options_error :: (IsNSData nsData, IsNSString path, IsNSError errorPtr) => nsData -> path -> NSDataWritingOptions -> errorPtr -> IO Bool
writeToFile_options_error nsData path writeOptionsMask errorPtr =
  sendMessage nsData writeToFile_options_errorSelector (toNSString path) writeOptionsMask (toNSError errorPtr)

-- | @- writeToURL:options:error:@
writeToURL_options_error :: (IsNSData nsData, IsNSURL url, IsNSError errorPtr) => nsData -> url -> NSDataWritingOptions -> errorPtr -> IO Bool
writeToURL_options_error nsData url writeOptionsMask errorPtr =
  sendMessage nsData writeToURL_options_errorSelector (toNSURL url) writeOptionsMask (toNSError errorPtr)

-- | @- rangeOfData:options:range:@
rangeOfData_options_range :: (IsNSData nsData, IsNSData dataToFind) => nsData -> dataToFind -> NSDataSearchOptions -> NSRange -> IO NSRange
rangeOfData_options_range nsData dataToFind mask searchRange =
  sendMessage nsData rangeOfData_options_rangeSelector (toNSData dataToFind) mask searchRange

-- | @- enumerateByteRangesUsingBlock:@
enumerateByteRangesUsingBlock :: IsNSData nsData => nsData -> Ptr () -> IO ()
enumerateByteRangesUsingBlock nsData block =
  sendMessage nsData enumerateByteRangesUsingBlockSelector block

-- | @- length@
length_ :: IsNSData nsData => nsData -> IO CULong
length_ nsData =
  sendMessage nsData lengthSelector

-- | @- bytes@
bytes :: IsNSData nsData => nsData -> IO RawId
bytes nsData =
  sendMessage nsData bytesSelector

-- | @- description@
description :: IsNSData nsData => nsData -> IO (Id NSString)
description nsData =
  sendMessage nsData descriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getBytes:@
getBytesSelector :: Selector '[Ptr ()] ()
getBytesSelector = mkSelector "getBytes:"

-- | @Selector@ for @dataWithContentsOfMappedFile:@
dataWithContentsOfMappedFileSelector :: Selector '[Id NSString] RawId
dataWithContentsOfMappedFileSelector = mkSelector "dataWithContentsOfMappedFile:"

-- | @Selector@ for @initWithContentsOfMappedFile:@
initWithContentsOfMappedFileSelector :: Selector '[Id NSString] RawId
initWithContentsOfMappedFileSelector = mkSelector "initWithContentsOfMappedFile:"

-- | @Selector@ for @initWithBase64Encoding:@
initWithBase64EncodingSelector :: Selector '[Id NSString] RawId
initWithBase64EncodingSelector = mkSelector "initWithBase64Encoding:"

-- | @Selector@ for @base64Encoding@
base64EncodingSelector :: Selector '[] (Id NSString)
base64EncodingSelector = mkSelector "base64Encoding"

-- | @Selector@ for @decompressedDataUsingAlgorithm:error:@
decompressedDataUsingAlgorithm_errorSelector :: Selector '[NSDataCompressionAlgorithm, Id NSError] (Id NSData)
decompressedDataUsingAlgorithm_errorSelector = mkSelector "decompressedDataUsingAlgorithm:error:"

-- | @Selector@ for @compressedDataUsingAlgorithm:error:@
compressedDataUsingAlgorithm_errorSelector :: Selector '[NSDataCompressionAlgorithm, Id NSError] (Id NSData)
compressedDataUsingAlgorithm_errorSelector = mkSelector "compressedDataUsingAlgorithm:error:"

-- | @Selector@ for @initWithBase64EncodedString:options:@
initWithBase64EncodedString_optionsSelector :: Selector '[Id NSString, NSDataBase64DecodingOptions] (Id NSData)
initWithBase64EncodedString_optionsSelector = mkSelector "initWithBase64EncodedString:options:"

-- | @Selector@ for @base64EncodedStringWithOptions:@
base64EncodedStringWithOptionsSelector :: Selector '[NSDataBase64EncodingOptions] (Id NSString)
base64EncodedStringWithOptionsSelector = mkSelector "base64EncodedStringWithOptions:"

-- | @Selector@ for @initWithBase64EncodedData:options:@
initWithBase64EncodedData_optionsSelector :: Selector '[Id NSData, NSDataBase64DecodingOptions] (Id NSData)
initWithBase64EncodedData_optionsSelector = mkSelector "initWithBase64EncodedData:options:"

-- | @Selector@ for @base64EncodedDataWithOptions:@
base64EncodedDataWithOptionsSelector :: Selector '[NSDataBase64EncodingOptions] (Id NSData)
base64EncodedDataWithOptionsSelector = mkSelector "base64EncodedDataWithOptions:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @dataWithBytes:length:@
dataWithBytes_lengthSelector :: Selector '[Const (Ptr ()), CULong] (Id NSData)
dataWithBytes_lengthSelector = mkSelector "dataWithBytes:length:"

-- | @Selector@ for @dataWithBytesNoCopy:length:@
dataWithBytesNoCopy_lengthSelector :: Selector '[Ptr (), CULong] (Id NSData)
dataWithBytesNoCopy_lengthSelector = mkSelector "dataWithBytesNoCopy:length:"

-- | @Selector@ for @dataWithBytesNoCopy:length:freeWhenDone:@
dataWithBytesNoCopy_length_freeWhenDoneSelector :: Selector '[Ptr (), CULong, Bool] (Id NSData)
dataWithBytesNoCopy_length_freeWhenDoneSelector = mkSelector "dataWithBytesNoCopy:length:freeWhenDone:"

-- | @Selector@ for @dataWithContentsOfFile:options:error:@
dataWithContentsOfFile_options_errorSelector :: Selector '[Id NSString, NSDataReadingOptions, Id NSError] (Id NSData)
dataWithContentsOfFile_options_errorSelector = mkSelector "dataWithContentsOfFile:options:error:"

-- | @Selector@ for @dataWithContentsOfURL:options:error:@
dataWithContentsOfURL_options_errorSelector :: Selector '[Id NSURL, NSDataReadingOptions, Id NSError] (Id NSData)
dataWithContentsOfURL_options_errorSelector = mkSelector "dataWithContentsOfURL:options:error:"

-- | @Selector@ for @dataWithContentsOfFile:@
dataWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSData)
dataWithContentsOfFileSelector = mkSelector "dataWithContentsOfFile:"

-- | @Selector@ for @dataWithContentsOfURL:@
dataWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSData)
dataWithContentsOfURLSelector = mkSelector "dataWithContentsOfURL:"

-- | @Selector@ for @initWithBytes:length:@
initWithBytes_lengthSelector :: Selector '[Const (Ptr ()), CULong] (Id NSData)
initWithBytes_lengthSelector = mkSelector "initWithBytes:length:"

-- | @Selector@ for @initWithBytesNoCopy:length:@
initWithBytesNoCopy_lengthSelector :: Selector '[Ptr (), CULong] (Id NSData)
initWithBytesNoCopy_lengthSelector = mkSelector "initWithBytesNoCopy:length:"

-- | @Selector@ for @initWithBytesNoCopy:length:freeWhenDone:@
initWithBytesNoCopy_length_freeWhenDoneSelector :: Selector '[Ptr (), CULong, Bool] (Id NSData)
initWithBytesNoCopy_length_freeWhenDoneSelector = mkSelector "initWithBytesNoCopy:length:freeWhenDone:"

-- | @Selector@ for @initWithBytesNoCopy:length:deallocator:@
initWithBytesNoCopy_length_deallocatorSelector :: Selector '[Ptr (), CULong, Ptr ()] (Id NSData)
initWithBytesNoCopy_length_deallocatorSelector = mkSelector "initWithBytesNoCopy:length:deallocator:"

-- | @Selector@ for @initWithContentsOfFile:options:error:@
initWithContentsOfFile_options_errorSelector :: Selector '[Id NSString, NSDataReadingOptions, Id NSError] (Id NSData)
initWithContentsOfFile_options_errorSelector = mkSelector "initWithContentsOfFile:options:error:"

-- | @Selector@ for @initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_errorSelector :: Selector '[Id NSURL, NSDataReadingOptions, Id NSError] (Id NSData)
initWithContentsOfURL_options_errorSelector = mkSelector "initWithContentsOfURL:options:error:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector '[Id NSString] (Id NSData)
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSData)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id NSData)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @dataWithData:@
dataWithDataSelector :: Selector '[Id NSData] (Id NSData)
dataWithDataSelector = mkSelector "dataWithData:"

-- | @Selector@ for @getBytes:length:@
getBytes_lengthSelector :: Selector '[Ptr (), CULong] ()
getBytes_lengthSelector = mkSelector "getBytes:length:"

-- | @Selector@ for @getBytes:range:@
getBytes_rangeSelector :: Selector '[Ptr (), NSRange] ()
getBytes_rangeSelector = mkSelector "getBytes:range:"

-- | @Selector@ for @isEqualToData:@
isEqualToDataSelector :: Selector '[Id NSData] Bool
isEqualToDataSelector = mkSelector "isEqualToData:"

-- | @Selector@ for @subdataWithRange:@
subdataWithRangeSelector :: Selector '[NSRange] (Id NSData)
subdataWithRangeSelector = mkSelector "subdataWithRange:"

-- | @Selector@ for @writeToFile:atomically:@
writeToFile_atomicallySelector :: Selector '[Id NSString, Bool] Bool
writeToFile_atomicallySelector = mkSelector "writeToFile:atomically:"

-- | @Selector@ for @writeToURL:atomically:@
writeToURL_atomicallySelector :: Selector '[Id NSURL, Bool] Bool
writeToURL_atomicallySelector = mkSelector "writeToURL:atomically:"

-- | @Selector@ for @writeToFile:options:error:@
writeToFile_options_errorSelector :: Selector '[Id NSString, NSDataWritingOptions, Id NSError] Bool
writeToFile_options_errorSelector = mkSelector "writeToFile:options:error:"

-- | @Selector@ for @writeToURL:options:error:@
writeToURL_options_errorSelector :: Selector '[Id NSURL, NSDataWritingOptions, Id NSError] Bool
writeToURL_options_errorSelector = mkSelector "writeToURL:options:error:"

-- | @Selector@ for @rangeOfData:options:range:@
rangeOfData_options_rangeSelector :: Selector '[Id NSData, NSDataSearchOptions, NSRange] NSRange
rangeOfData_options_rangeSelector = mkSelector "rangeOfData:options:range:"

-- | @Selector@ for @enumerateByteRangesUsingBlock:@
enumerateByteRangesUsingBlockSelector :: Selector '[Ptr ()] ()
enumerateByteRangesUsingBlockSelector = mkSelector "enumerateByteRangesUsingBlock:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

-- | @Selector@ for @bytes@
bytesSelector :: Selector '[] RawId
bytesSelector = mkSelector "bytes"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

