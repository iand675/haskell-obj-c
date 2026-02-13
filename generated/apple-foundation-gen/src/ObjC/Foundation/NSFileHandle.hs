{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileHandle@.
module ObjC.Foundation.NSFileHandle
  ( NSFileHandle
  , IsNSFileHandle(..)
  , initWithFileDescriptor_closeOnDealloc
  , initWithCoder
  , readDataToEndOfFileAndReturnError
  , readDataUpToLength_error
  , writeData_error
  , getOffset_error
  , seekToEndReturningOffset_error
  , seekToOffset_error
  , truncateAtOffset_error
  , synchronizeAndReturnError
  , closeAndReturnError
  , readDataToEndOfFile
  , readDataOfLength
  , writeData
  , seekToEndOfFile
  , seekToFileOffset
  , truncateFileAtOffset
  , synchronizeFile
  , closeFile
  , initWithFileDescriptor
  , readInBackgroundAndNotifyForModes
  , readInBackgroundAndNotify
  , readToEndOfFileInBackgroundAndNotifyForModes
  , readToEndOfFileInBackgroundAndNotify
  , acceptConnectionInBackgroundAndNotifyForModes
  , acceptConnectionInBackgroundAndNotify
  , waitForDataInBackgroundAndNotifyForModes
  , waitForDataInBackgroundAndNotify
  , fileHandleForReadingAtPath
  , fileHandleForWritingAtPath
  , fileHandleForUpdatingAtPath
  , fileHandleForReadingFromURL_error
  , fileHandleForWritingToURL_error
  , fileHandleForUpdatingURL_error
  , availableData
  , offsetInFile
  , fileDescriptor
  , readabilityHandler
  , setReadabilityHandler
  , writeabilityHandler
  , setWriteabilityHandler
  , fileHandleWithStandardInput
  , fileHandleWithStandardOutput
  , fileHandleWithStandardError
  , fileHandleWithNullDevice
  , acceptConnectionInBackgroundAndNotifyForModesSelector
  , acceptConnectionInBackgroundAndNotifySelector
  , availableDataSelector
  , closeAndReturnErrorSelector
  , closeFileSelector
  , fileDescriptorSelector
  , fileHandleForReadingAtPathSelector
  , fileHandleForReadingFromURL_errorSelector
  , fileHandleForUpdatingAtPathSelector
  , fileHandleForUpdatingURL_errorSelector
  , fileHandleForWritingAtPathSelector
  , fileHandleForWritingToURL_errorSelector
  , fileHandleWithNullDeviceSelector
  , fileHandleWithStandardErrorSelector
  , fileHandleWithStandardInputSelector
  , fileHandleWithStandardOutputSelector
  , getOffset_errorSelector
  , initWithCoderSelector
  , initWithFileDescriptorSelector
  , initWithFileDescriptor_closeOnDeallocSelector
  , offsetInFileSelector
  , readDataOfLengthSelector
  , readDataToEndOfFileAndReturnErrorSelector
  , readDataToEndOfFileSelector
  , readDataUpToLength_errorSelector
  , readInBackgroundAndNotifyForModesSelector
  , readInBackgroundAndNotifySelector
  , readToEndOfFileInBackgroundAndNotifyForModesSelector
  , readToEndOfFileInBackgroundAndNotifySelector
  , readabilityHandlerSelector
  , seekToEndOfFileSelector
  , seekToEndReturningOffset_errorSelector
  , seekToFileOffsetSelector
  , seekToOffset_errorSelector
  , setReadabilityHandlerSelector
  , setWriteabilityHandlerSelector
  , synchronizeAndReturnErrorSelector
  , synchronizeFileSelector
  , truncateAtOffset_errorSelector
  , truncateFileAtOffsetSelector
  , waitForDataInBackgroundAndNotifyForModesSelector
  , waitForDataInBackgroundAndNotifySelector
  , writeDataSelector
  , writeData_errorSelector
  , writeabilityHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithFileDescriptor:closeOnDealloc:@
initWithFileDescriptor_closeOnDealloc :: IsNSFileHandle nsFileHandle => nsFileHandle -> CInt -> Bool -> IO (Id NSFileHandle)
initWithFileDescriptor_closeOnDealloc nsFileHandle fd closeopt =
  sendOwnedMessage nsFileHandle initWithFileDescriptor_closeOnDeallocSelector fd closeopt

-- | @- initWithCoder:@
initWithCoder :: (IsNSFileHandle nsFileHandle, IsNSCoder coder) => nsFileHandle -> coder -> IO (Id NSFileHandle)
initWithCoder nsFileHandle coder =
  sendOwnedMessage nsFileHandle initWithCoderSelector (toNSCoder coder)

-- | @- readDataToEndOfFileAndReturnError:@
readDataToEndOfFileAndReturnError :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> error_ -> IO (Id NSData)
readDataToEndOfFileAndReturnError nsFileHandle error_ =
  sendMessage nsFileHandle readDataToEndOfFileAndReturnErrorSelector (toNSError error_)

-- | @- readDataUpToLength:error:@
readDataUpToLength_error :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> CULong -> error_ -> IO (Id NSData)
readDataUpToLength_error nsFileHandle length_ error_ =
  sendMessage nsFileHandle readDataUpToLength_errorSelector length_ (toNSError error_)

-- | @- writeData:error:@
writeData_error :: (IsNSFileHandle nsFileHandle, IsNSData data_, IsNSError error_) => nsFileHandle -> data_ -> error_ -> IO Bool
writeData_error nsFileHandle data_ error_ =
  sendMessage nsFileHandle writeData_errorSelector (toNSData data_) (toNSError error_)

-- | @- getOffset:error:@
getOffset_error :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> Ptr CULong -> error_ -> IO Bool
getOffset_error nsFileHandle offsetInFile error_ =
  sendMessage nsFileHandle getOffset_errorSelector offsetInFile (toNSError error_)

-- | @- seekToEndReturningOffset:error:@
seekToEndReturningOffset_error :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> Ptr CULong -> error_ -> IO Bool
seekToEndReturningOffset_error nsFileHandle offsetInFile error_ =
  sendMessage nsFileHandle seekToEndReturningOffset_errorSelector offsetInFile (toNSError error_)

-- | @- seekToOffset:error:@
seekToOffset_error :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> CULong -> error_ -> IO Bool
seekToOffset_error nsFileHandle offset error_ =
  sendMessage nsFileHandle seekToOffset_errorSelector offset (toNSError error_)

-- | @- truncateAtOffset:error:@
truncateAtOffset_error :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> CULong -> error_ -> IO Bool
truncateAtOffset_error nsFileHandle offset error_ =
  sendMessage nsFileHandle truncateAtOffset_errorSelector offset (toNSError error_)

-- | @- synchronizeAndReturnError:@
synchronizeAndReturnError :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> error_ -> IO Bool
synchronizeAndReturnError nsFileHandle error_ =
  sendMessage nsFileHandle synchronizeAndReturnErrorSelector (toNSError error_)

-- | @- closeAndReturnError:@
closeAndReturnError :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> error_ -> IO Bool
closeAndReturnError nsFileHandle error_ =
  sendMessage nsFileHandle closeAndReturnErrorSelector (toNSError error_)

-- | @- readDataToEndOfFile@
readDataToEndOfFile :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO (Id NSData)
readDataToEndOfFile nsFileHandle =
  sendMessage nsFileHandle readDataToEndOfFileSelector

-- | @- readDataOfLength:@
readDataOfLength :: IsNSFileHandle nsFileHandle => nsFileHandle -> CULong -> IO (Id NSData)
readDataOfLength nsFileHandle length_ =
  sendMessage nsFileHandle readDataOfLengthSelector length_

-- | @- writeData:@
writeData :: (IsNSFileHandle nsFileHandle, IsNSData data_) => nsFileHandle -> data_ -> IO ()
writeData nsFileHandle data_ =
  sendMessage nsFileHandle writeDataSelector (toNSData data_)

-- | @- seekToEndOfFile@
seekToEndOfFile :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO CULong
seekToEndOfFile nsFileHandle =
  sendMessage nsFileHandle seekToEndOfFileSelector

-- | @- seekToFileOffset:@
seekToFileOffset :: IsNSFileHandle nsFileHandle => nsFileHandle -> CULong -> IO ()
seekToFileOffset nsFileHandle offset =
  sendMessage nsFileHandle seekToFileOffsetSelector offset

-- | @- truncateFileAtOffset:@
truncateFileAtOffset :: IsNSFileHandle nsFileHandle => nsFileHandle -> CULong -> IO ()
truncateFileAtOffset nsFileHandle offset =
  sendMessage nsFileHandle truncateFileAtOffsetSelector offset

-- | @- synchronizeFile@
synchronizeFile :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
synchronizeFile nsFileHandle =
  sendMessage nsFileHandle synchronizeFileSelector

-- | @- closeFile@
closeFile :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
closeFile nsFileHandle =
  sendMessage nsFileHandle closeFileSelector

-- | @- initWithFileDescriptor:@
initWithFileDescriptor :: IsNSFileHandle nsFileHandle => nsFileHandle -> CInt -> IO (Id NSFileHandle)
initWithFileDescriptor nsFileHandle fd =
  sendOwnedMessage nsFileHandle initWithFileDescriptorSelector fd

-- | @- readInBackgroundAndNotifyForModes:@
readInBackgroundAndNotifyForModes :: (IsNSFileHandle nsFileHandle, IsNSArray modes) => nsFileHandle -> modes -> IO ()
readInBackgroundAndNotifyForModes nsFileHandle modes =
  sendMessage nsFileHandle readInBackgroundAndNotifyForModesSelector (toNSArray modes)

-- | @- readInBackgroundAndNotify@
readInBackgroundAndNotify :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
readInBackgroundAndNotify nsFileHandle =
  sendMessage nsFileHandle readInBackgroundAndNotifySelector

-- | @- readToEndOfFileInBackgroundAndNotifyForModes:@
readToEndOfFileInBackgroundAndNotifyForModes :: (IsNSFileHandle nsFileHandle, IsNSArray modes) => nsFileHandle -> modes -> IO ()
readToEndOfFileInBackgroundAndNotifyForModes nsFileHandle modes =
  sendMessage nsFileHandle readToEndOfFileInBackgroundAndNotifyForModesSelector (toNSArray modes)

-- | @- readToEndOfFileInBackgroundAndNotify@
readToEndOfFileInBackgroundAndNotify :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
readToEndOfFileInBackgroundAndNotify nsFileHandle =
  sendMessage nsFileHandle readToEndOfFileInBackgroundAndNotifySelector

-- | @- acceptConnectionInBackgroundAndNotifyForModes:@
acceptConnectionInBackgroundAndNotifyForModes :: (IsNSFileHandle nsFileHandle, IsNSArray modes) => nsFileHandle -> modes -> IO ()
acceptConnectionInBackgroundAndNotifyForModes nsFileHandle modes =
  sendMessage nsFileHandle acceptConnectionInBackgroundAndNotifyForModesSelector (toNSArray modes)

-- | @- acceptConnectionInBackgroundAndNotify@
acceptConnectionInBackgroundAndNotify :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
acceptConnectionInBackgroundAndNotify nsFileHandle =
  sendMessage nsFileHandle acceptConnectionInBackgroundAndNotifySelector

-- | @- waitForDataInBackgroundAndNotifyForModes:@
waitForDataInBackgroundAndNotifyForModes :: (IsNSFileHandle nsFileHandle, IsNSArray modes) => nsFileHandle -> modes -> IO ()
waitForDataInBackgroundAndNotifyForModes nsFileHandle modes =
  sendMessage nsFileHandle waitForDataInBackgroundAndNotifyForModesSelector (toNSArray modes)

-- | @- waitForDataInBackgroundAndNotify@
waitForDataInBackgroundAndNotify :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
waitForDataInBackgroundAndNotify nsFileHandle =
  sendMessage nsFileHandle waitForDataInBackgroundAndNotifySelector

-- | @+ fileHandleForReadingAtPath:@
fileHandleForReadingAtPath :: IsNSString path => path -> IO (Id NSFileHandle)
fileHandleForReadingAtPath path =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMessage cls' fileHandleForReadingAtPathSelector (toNSString path)

-- | @+ fileHandleForWritingAtPath:@
fileHandleForWritingAtPath :: IsNSString path => path -> IO (Id NSFileHandle)
fileHandleForWritingAtPath path =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMessage cls' fileHandleForWritingAtPathSelector (toNSString path)

-- | @+ fileHandleForUpdatingAtPath:@
fileHandleForUpdatingAtPath :: IsNSString path => path -> IO (Id NSFileHandle)
fileHandleForUpdatingAtPath path =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMessage cls' fileHandleForUpdatingAtPathSelector (toNSString path)

-- | @+ fileHandleForReadingFromURL:error:@
fileHandleForReadingFromURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSFileHandle)
fileHandleForReadingFromURL_error url error_ =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMessage cls' fileHandleForReadingFromURL_errorSelector (toNSURL url) (toNSError error_)

-- | @+ fileHandleForWritingToURL:error:@
fileHandleForWritingToURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSFileHandle)
fileHandleForWritingToURL_error url error_ =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMessage cls' fileHandleForWritingToURL_errorSelector (toNSURL url) (toNSError error_)

-- | @+ fileHandleForUpdatingURL:error:@
fileHandleForUpdatingURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSFileHandle)
fileHandleForUpdatingURL_error url error_ =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMessage cls' fileHandleForUpdatingURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- availableData@
availableData :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO (Id NSData)
availableData nsFileHandle =
  sendMessage nsFileHandle availableDataSelector

-- | @- offsetInFile@
offsetInFile :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO CULong
offsetInFile nsFileHandle =
  sendMessage nsFileHandle offsetInFileSelector

-- | @- fileDescriptor@
fileDescriptor :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO CInt
fileDescriptor nsFileHandle =
  sendMessage nsFileHandle fileDescriptorSelector

-- | @- readabilityHandler@
readabilityHandler :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO (Ptr ())
readabilityHandler nsFileHandle =
  sendMessage nsFileHandle readabilityHandlerSelector

-- | @- setReadabilityHandler:@
setReadabilityHandler :: IsNSFileHandle nsFileHandle => nsFileHandle -> Ptr () -> IO ()
setReadabilityHandler nsFileHandle value =
  sendMessage nsFileHandle setReadabilityHandlerSelector value

-- | @- writeabilityHandler@
writeabilityHandler :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO (Ptr ())
writeabilityHandler nsFileHandle =
  sendMessage nsFileHandle writeabilityHandlerSelector

-- | @- setWriteabilityHandler:@
setWriteabilityHandler :: IsNSFileHandle nsFileHandle => nsFileHandle -> Ptr () -> IO ()
setWriteabilityHandler nsFileHandle value =
  sendMessage nsFileHandle setWriteabilityHandlerSelector value

-- | @+ fileHandleWithStandardInput@
fileHandleWithStandardInput :: IO (Id NSFileHandle)
fileHandleWithStandardInput  =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMessage cls' fileHandleWithStandardInputSelector

-- | @+ fileHandleWithStandardOutput@
fileHandleWithStandardOutput :: IO (Id NSFileHandle)
fileHandleWithStandardOutput  =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMessage cls' fileHandleWithStandardOutputSelector

-- | @+ fileHandleWithStandardError@
fileHandleWithStandardError :: IO (Id NSFileHandle)
fileHandleWithStandardError  =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMessage cls' fileHandleWithStandardErrorSelector

-- | @+ fileHandleWithNullDevice@
fileHandleWithNullDevice :: IO (Id NSFileHandle)
fileHandleWithNullDevice  =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMessage cls' fileHandleWithNullDeviceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileDescriptor:closeOnDealloc:@
initWithFileDescriptor_closeOnDeallocSelector :: Selector '[CInt, Bool] (Id NSFileHandle)
initWithFileDescriptor_closeOnDeallocSelector = mkSelector "initWithFileDescriptor:closeOnDealloc:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSFileHandle)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @readDataToEndOfFileAndReturnError:@
readDataToEndOfFileAndReturnErrorSelector :: Selector '[Id NSError] (Id NSData)
readDataToEndOfFileAndReturnErrorSelector = mkSelector "readDataToEndOfFileAndReturnError:"

-- | @Selector@ for @readDataUpToLength:error:@
readDataUpToLength_errorSelector :: Selector '[CULong, Id NSError] (Id NSData)
readDataUpToLength_errorSelector = mkSelector "readDataUpToLength:error:"

-- | @Selector@ for @writeData:error:@
writeData_errorSelector :: Selector '[Id NSData, Id NSError] Bool
writeData_errorSelector = mkSelector "writeData:error:"

-- | @Selector@ for @getOffset:error:@
getOffset_errorSelector :: Selector '[Ptr CULong, Id NSError] Bool
getOffset_errorSelector = mkSelector "getOffset:error:"

-- | @Selector@ for @seekToEndReturningOffset:error:@
seekToEndReturningOffset_errorSelector :: Selector '[Ptr CULong, Id NSError] Bool
seekToEndReturningOffset_errorSelector = mkSelector "seekToEndReturningOffset:error:"

-- | @Selector@ for @seekToOffset:error:@
seekToOffset_errorSelector :: Selector '[CULong, Id NSError] Bool
seekToOffset_errorSelector = mkSelector "seekToOffset:error:"

-- | @Selector@ for @truncateAtOffset:error:@
truncateAtOffset_errorSelector :: Selector '[CULong, Id NSError] Bool
truncateAtOffset_errorSelector = mkSelector "truncateAtOffset:error:"

-- | @Selector@ for @synchronizeAndReturnError:@
synchronizeAndReturnErrorSelector :: Selector '[Id NSError] Bool
synchronizeAndReturnErrorSelector = mkSelector "synchronizeAndReturnError:"

-- | @Selector@ for @closeAndReturnError:@
closeAndReturnErrorSelector :: Selector '[Id NSError] Bool
closeAndReturnErrorSelector = mkSelector "closeAndReturnError:"

-- | @Selector@ for @readDataToEndOfFile@
readDataToEndOfFileSelector :: Selector '[] (Id NSData)
readDataToEndOfFileSelector = mkSelector "readDataToEndOfFile"

-- | @Selector@ for @readDataOfLength:@
readDataOfLengthSelector :: Selector '[CULong] (Id NSData)
readDataOfLengthSelector = mkSelector "readDataOfLength:"

-- | @Selector@ for @writeData:@
writeDataSelector :: Selector '[Id NSData] ()
writeDataSelector = mkSelector "writeData:"

-- | @Selector@ for @seekToEndOfFile@
seekToEndOfFileSelector :: Selector '[] CULong
seekToEndOfFileSelector = mkSelector "seekToEndOfFile"

-- | @Selector@ for @seekToFileOffset:@
seekToFileOffsetSelector :: Selector '[CULong] ()
seekToFileOffsetSelector = mkSelector "seekToFileOffset:"

-- | @Selector@ for @truncateFileAtOffset:@
truncateFileAtOffsetSelector :: Selector '[CULong] ()
truncateFileAtOffsetSelector = mkSelector "truncateFileAtOffset:"

-- | @Selector@ for @synchronizeFile@
synchronizeFileSelector :: Selector '[] ()
synchronizeFileSelector = mkSelector "synchronizeFile"

-- | @Selector@ for @closeFile@
closeFileSelector :: Selector '[] ()
closeFileSelector = mkSelector "closeFile"

-- | @Selector@ for @initWithFileDescriptor:@
initWithFileDescriptorSelector :: Selector '[CInt] (Id NSFileHandle)
initWithFileDescriptorSelector = mkSelector "initWithFileDescriptor:"

-- | @Selector@ for @readInBackgroundAndNotifyForModes:@
readInBackgroundAndNotifyForModesSelector :: Selector '[Id NSArray] ()
readInBackgroundAndNotifyForModesSelector = mkSelector "readInBackgroundAndNotifyForModes:"

-- | @Selector@ for @readInBackgroundAndNotify@
readInBackgroundAndNotifySelector :: Selector '[] ()
readInBackgroundAndNotifySelector = mkSelector "readInBackgroundAndNotify"

-- | @Selector@ for @readToEndOfFileInBackgroundAndNotifyForModes:@
readToEndOfFileInBackgroundAndNotifyForModesSelector :: Selector '[Id NSArray] ()
readToEndOfFileInBackgroundAndNotifyForModesSelector = mkSelector "readToEndOfFileInBackgroundAndNotifyForModes:"

-- | @Selector@ for @readToEndOfFileInBackgroundAndNotify@
readToEndOfFileInBackgroundAndNotifySelector :: Selector '[] ()
readToEndOfFileInBackgroundAndNotifySelector = mkSelector "readToEndOfFileInBackgroundAndNotify"

-- | @Selector@ for @acceptConnectionInBackgroundAndNotifyForModes:@
acceptConnectionInBackgroundAndNotifyForModesSelector :: Selector '[Id NSArray] ()
acceptConnectionInBackgroundAndNotifyForModesSelector = mkSelector "acceptConnectionInBackgroundAndNotifyForModes:"

-- | @Selector@ for @acceptConnectionInBackgroundAndNotify@
acceptConnectionInBackgroundAndNotifySelector :: Selector '[] ()
acceptConnectionInBackgroundAndNotifySelector = mkSelector "acceptConnectionInBackgroundAndNotify"

-- | @Selector@ for @waitForDataInBackgroundAndNotifyForModes:@
waitForDataInBackgroundAndNotifyForModesSelector :: Selector '[Id NSArray] ()
waitForDataInBackgroundAndNotifyForModesSelector = mkSelector "waitForDataInBackgroundAndNotifyForModes:"

-- | @Selector@ for @waitForDataInBackgroundAndNotify@
waitForDataInBackgroundAndNotifySelector :: Selector '[] ()
waitForDataInBackgroundAndNotifySelector = mkSelector "waitForDataInBackgroundAndNotify"

-- | @Selector@ for @fileHandleForReadingAtPath:@
fileHandleForReadingAtPathSelector :: Selector '[Id NSString] (Id NSFileHandle)
fileHandleForReadingAtPathSelector = mkSelector "fileHandleForReadingAtPath:"

-- | @Selector@ for @fileHandleForWritingAtPath:@
fileHandleForWritingAtPathSelector :: Selector '[Id NSString] (Id NSFileHandle)
fileHandleForWritingAtPathSelector = mkSelector "fileHandleForWritingAtPath:"

-- | @Selector@ for @fileHandleForUpdatingAtPath:@
fileHandleForUpdatingAtPathSelector :: Selector '[Id NSString] (Id NSFileHandle)
fileHandleForUpdatingAtPathSelector = mkSelector "fileHandleForUpdatingAtPath:"

-- | @Selector@ for @fileHandleForReadingFromURL:error:@
fileHandleForReadingFromURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSFileHandle)
fileHandleForReadingFromURL_errorSelector = mkSelector "fileHandleForReadingFromURL:error:"

-- | @Selector@ for @fileHandleForWritingToURL:error:@
fileHandleForWritingToURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSFileHandle)
fileHandleForWritingToURL_errorSelector = mkSelector "fileHandleForWritingToURL:error:"

-- | @Selector@ for @fileHandleForUpdatingURL:error:@
fileHandleForUpdatingURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSFileHandle)
fileHandleForUpdatingURL_errorSelector = mkSelector "fileHandleForUpdatingURL:error:"

-- | @Selector@ for @availableData@
availableDataSelector :: Selector '[] (Id NSData)
availableDataSelector = mkSelector "availableData"

-- | @Selector@ for @offsetInFile@
offsetInFileSelector :: Selector '[] CULong
offsetInFileSelector = mkSelector "offsetInFile"

-- | @Selector@ for @fileDescriptor@
fileDescriptorSelector :: Selector '[] CInt
fileDescriptorSelector = mkSelector "fileDescriptor"

-- | @Selector@ for @readabilityHandler@
readabilityHandlerSelector :: Selector '[] (Ptr ())
readabilityHandlerSelector = mkSelector "readabilityHandler"

-- | @Selector@ for @setReadabilityHandler:@
setReadabilityHandlerSelector :: Selector '[Ptr ()] ()
setReadabilityHandlerSelector = mkSelector "setReadabilityHandler:"

-- | @Selector@ for @writeabilityHandler@
writeabilityHandlerSelector :: Selector '[] (Ptr ())
writeabilityHandlerSelector = mkSelector "writeabilityHandler"

-- | @Selector@ for @setWriteabilityHandler:@
setWriteabilityHandlerSelector :: Selector '[Ptr ()] ()
setWriteabilityHandlerSelector = mkSelector "setWriteabilityHandler:"

-- | @Selector@ for @fileHandleWithStandardInput@
fileHandleWithStandardInputSelector :: Selector '[] (Id NSFileHandle)
fileHandleWithStandardInputSelector = mkSelector "fileHandleWithStandardInput"

-- | @Selector@ for @fileHandleWithStandardOutput@
fileHandleWithStandardOutputSelector :: Selector '[] (Id NSFileHandle)
fileHandleWithStandardOutputSelector = mkSelector "fileHandleWithStandardOutput"

-- | @Selector@ for @fileHandleWithStandardError@
fileHandleWithStandardErrorSelector :: Selector '[] (Id NSFileHandle)
fileHandleWithStandardErrorSelector = mkSelector "fileHandleWithStandardError"

-- | @Selector@ for @fileHandleWithNullDevice@
fileHandleWithNullDeviceSelector :: Selector '[] (Id NSFileHandle)
fileHandleWithNullDeviceSelector = mkSelector "fileHandleWithNullDevice"

