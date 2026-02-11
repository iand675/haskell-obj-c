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
  , initWithFileDescriptor_closeOnDeallocSelector
  , initWithCoderSelector
  , readDataToEndOfFileAndReturnErrorSelector
  , readDataUpToLength_errorSelector
  , writeData_errorSelector
  , getOffset_errorSelector
  , seekToEndReturningOffset_errorSelector
  , seekToOffset_errorSelector
  , truncateAtOffset_errorSelector
  , synchronizeAndReturnErrorSelector
  , closeAndReturnErrorSelector
  , readDataToEndOfFileSelector
  , readDataOfLengthSelector
  , writeDataSelector
  , seekToEndOfFileSelector
  , seekToFileOffsetSelector
  , truncateFileAtOffsetSelector
  , synchronizeFileSelector
  , closeFileSelector
  , initWithFileDescriptorSelector
  , readInBackgroundAndNotifyForModesSelector
  , readInBackgroundAndNotifySelector
  , readToEndOfFileInBackgroundAndNotifyForModesSelector
  , readToEndOfFileInBackgroundAndNotifySelector
  , acceptConnectionInBackgroundAndNotifyForModesSelector
  , acceptConnectionInBackgroundAndNotifySelector
  , waitForDataInBackgroundAndNotifyForModesSelector
  , waitForDataInBackgroundAndNotifySelector
  , fileHandleForReadingAtPathSelector
  , fileHandleForWritingAtPathSelector
  , fileHandleForUpdatingAtPathSelector
  , fileHandleForReadingFromURL_errorSelector
  , fileHandleForWritingToURL_errorSelector
  , fileHandleForUpdatingURL_errorSelector
  , availableDataSelector
  , offsetInFileSelector
  , fileDescriptorSelector
  , readabilityHandlerSelector
  , setReadabilityHandlerSelector
  , writeabilityHandlerSelector
  , setWriteabilityHandlerSelector
  , fileHandleWithStandardInputSelector
  , fileHandleWithStandardOutputSelector
  , fileHandleWithStandardErrorSelector
  , fileHandleWithNullDeviceSelector


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

-- | @- initWithFileDescriptor:closeOnDealloc:@
initWithFileDescriptor_closeOnDealloc :: IsNSFileHandle nsFileHandle => nsFileHandle -> CInt -> Bool -> IO (Id NSFileHandle)
initWithFileDescriptor_closeOnDealloc nsFileHandle  fd closeopt =
  sendMsg nsFileHandle (mkSelector "initWithFileDescriptor:closeOnDealloc:") (retPtr retVoid) [argCInt (fromIntegral fd), argCULong (if closeopt then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSFileHandle nsFileHandle, IsNSCoder coder) => nsFileHandle -> coder -> IO (Id NSFileHandle)
initWithCoder nsFileHandle  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsFileHandle (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- readDataToEndOfFileAndReturnError:@
readDataToEndOfFileAndReturnError :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> error_ -> IO (Id NSData)
readDataToEndOfFileAndReturnError nsFileHandle  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsFileHandle (mkSelector "readDataToEndOfFileAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- readDataUpToLength:error:@
readDataUpToLength_error :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> CULong -> error_ -> IO (Id NSData)
readDataUpToLength_error nsFileHandle  length_ error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsFileHandle (mkSelector "readDataUpToLength:error:") (retPtr retVoid) [argCULong (fromIntegral length_), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeData:error:@
writeData_error :: (IsNSFileHandle nsFileHandle, IsNSData data_, IsNSError error_) => nsFileHandle -> data_ -> error_ -> IO Bool
writeData_error nsFileHandle  data_ error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileHandle (mkSelector "writeData:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- getOffset:error:@
getOffset_error :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> Ptr CULong -> error_ -> IO Bool
getOffset_error nsFileHandle  offsetInFile error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileHandle (mkSelector "getOffset:error:") retCULong [argPtr offsetInFile, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- seekToEndReturningOffset:error:@
seekToEndReturningOffset_error :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> Ptr CULong -> error_ -> IO Bool
seekToEndReturningOffset_error nsFileHandle  offsetInFile error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileHandle (mkSelector "seekToEndReturningOffset:error:") retCULong [argPtr offsetInFile, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- seekToOffset:error:@
seekToOffset_error :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> CULong -> error_ -> IO Bool
seekToOffset_error nsFileHandle  offset error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileHandle (mkSelector "seekToOffset:error:") retCULong [argCULong (fromIntegral offset), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- truncateAtOffset:error:@
truncateAtOffset_error :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> CULong -> error_ -> IO Bool
truncateAtOffset_error nsFileHandle  offset error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileHandle (mkSelector "truncateAtOffset:error:") retCULong [argCULong (fromIntegral offset), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- synchronizeAndReturnError:@
synchronizeAndReturnError :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> error_ -> IO Bool
synchronizeAndReturnError nsFileHandle  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileHandle (mkSelector "synchronizeAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- closeAndReturnError:@
closeAndReturnError :: (IsNSFileHandle nsFileHandle, IsNSError error_) => nsFileHandle -> error_ -> IO Bool
closeAndReturnError nsFileHandle  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileHandle (mkSelector "closeAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- readDataToEndOfFile@
readDataToEndOfFile :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO (Id NSData)
readDataToEndOfFile nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "readDataToEndOfFile") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- readDataOfLength:@
readDataOfLength :: IsNSFileHandle nsFileHandle => nsFileHandle -> CULong -> IO (Id NSData)
readDataOfLength nsFileHandle  length_ =
  sendMsg nsFileHandle (mkSelector "readDataOfLength:") (retPtr retVoid) [argCULong (fromIntegral length_)] >>= retainedObject . castPtr

-- | @- writeData:@
writeData :: (IsNSFileHandle nsFileHandle, IsNSData data_) => nsFileHandle -> data_ -> IO ()
writeData nsFileHandle  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsFileHandle (mkSelector "writeData:") retVoid [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- seekToEndOfFile@
seekToEndOfFile :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO CULong
seekToEndOfFile nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "seekToEndOfFile") retCULong []

-- | @- seekToFileOffset:@
seekToFileOffset :: IsNSFileHandle nsFileHandle => nsFileHandle -> CULong -> IO ()
seekToFileOffset nsFileHandle  offset =
  sendMsg nsFileHandle (mkSelector "seekToFileOffset:") retVoid [argCULong (fromIntegral offset)]

-- | @- truncateFileAtOffset:@
truncateFileAtOffset :: IsNSFileHandle nsFileHandle => nsFileHandle -> CULong -> IO ()
truncateFileAtOffset nsFileHandle  offset =
  sendMsg nsFileHandle (mkSelector "truncateFileAtOffset:") retVoid [argCULong (fromIntegral offset)]

-- | @- synchronizeFile@
synchronizeFile :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
synchronizeFile nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "synchronizeFile") retVoid []

-- | @- closeFile@
closeFile :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
closeFile nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "closeFile") retVoid []

-- | @- initWithFileDescriptor:@
initWithFileDescriptor :: IsNSFileHandle nsFileHandle => nsFileHandle -> CInt -> IO (Id NSFileHandle)
initWithFileDescriptor nsFileHandle  fd =
  sendMsg nsFileHandle (mkSelector "initWithFileDescriptor:") (retPtr retVoid) [argCInt (fromIntegral fd)] >>= ownedObject . castPtr

-- | @- readInBackgroundAndNotifyForModes:@
readInBackgroundAndNotifyForModes :: (IsNSFileHandle nsFileHandle, IsNSArray modes) => nsFileHandle -> modes -> IO ()
readInBackgroundAndNotifyForModes nsFileHandle  modes =
withObjCPtr modes $ \raw_modes ->
    sendMsg nsFileHandle (mkSelector "readInBackgroundAndNotifyForModes:") retVoid [argPtr (castPtr raw_modes :: Ptr ())]

-- | @- readInBackgroundAndNotify@
readInBackgroundAndNotify :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
readInBackgroundAndNotify nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "readInBackgroundAndNotify") retVoid []

-- | @- readToEndOfFileInBackgroundAndNotifyForModes:@
readToEndOfFileInBackgroundAndNotifyForModes :: (IsNSFileHandle nsFileHandle, IsNSArray modes) => nsFileHandle -> modes -> IO ()
readToEndOfFileInBackgroundAndNotifyForModes nsFileHandle  modes =
withObjCPtr modes $ \raw_modes ->
    sendMsg nsFileHandle (mkSelector "readToEndOfFileInBackgroundAndNotifyForModes:") retVoid [argPtr (castPtr raw_modes :: Ptr ())]

-- | @- readToEndOfFileInBackgroundAndNotify@
readToEndOfFileInBackgroundAndNotify :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
readToEndOfFileInBackgroundAndNotify nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "readToEndOfFileInBackgroundAndNotify") retVoid []

-- | @- acceptConnectionInBackgroundAndNotifyForModes:@
acceptConnectionInBackgroundAndNotifyForModes :: (IsNSFileHandle nsFileHandle, IsNSArray modes) => nsFileHandle -> modes -> IO ()
acceptConnectionInBackgroundAndNotifyForModes nsFileHandle  modes =
withObjCPtr modes $ \raw_modes ->
    sendMsg nsFileHandle (mkSelector "acceptConnectionInBackgroundAndNotifyForModes:") retVoid [argPtr (castPtr raw_modes :: Ptr ())]

-- | @- acceptConnectionInBackgroundAndNotify@
acceptConnectionInBackgroundAndNotify :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
acceptConnectionInBackgroundAndNotify nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "acceptConnectionInBackgroundAndNotify") retVoid []

-- | @- waitForDataInBackgroundAndNotifyForModes:@
waitForDataInBackgroundAndNotifyForModes :: (IsNSFileHandle nsFileHandle, IsNSArray modes) => nsFileHandle -> modes -> IO ()
waitForDataInBackgroundAndNotifyForModes nsFileHandle  modes =
withObjCPtr modes $ \raw_modes ->
    sendMsg nsFileHandle (mkSelector "waitForDataInBackgroundAndNotifyForModes:") retVoid [argPtr (castPtr raw_modes :: Ptr ())]

-- | @- waitForDataInBackgroundAndNotify@
waitForDataInBackgroundAndNotify :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO ()
waitForDataInBackgroundAndNotify nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "waitForDataInBackgroundAndNotify") retVoid []

-- | @+ fileHandleForReadingAtPath:@
fileHandleForReadingAtPath :: IsNSString path => path -> IO (Id NSFileHandle)
fileHandleForReadingAtPath path =
  do
    cls' <- getRequiredClass "NSFileHandle"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "fileHandleForReadingAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fileHandleForWritingAtPath:@
fileHandleForWritingAtPath :: IsNSString path => path -> IO (Id NSFileHandle)
fileHandleForWritingAtPath path =
  do
    cls' <- getRequiredClass "NSFileHandle"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "fileHandleForWritingAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fileHandleForUpdatingAtPath:@
fileHandleForUpdatingAtPath :: IsNSString path => path -> IO (Id NSFileHandle)
fileHandleForUpdatingAtPath path =
  do
    cls' <- getRequiredClass "NSFileHandle"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "fileHandleForUpdatingAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fileHandleForReadingFromURL:error:@
fileHandleForReadingFromURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSFileHandle)
fileHandleForReadingFromURL_error url error_ =
  do
    cls' <- getRequiredClass "NSFileHandle"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "fileHandleForReadingFromURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fileHandleForWritingToURL:error:@
fileHandleForWritingToURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSFileHandle)
fileHandleForWritingToURL_error url error_ =
  do
    cls' <- getRequiredClass "NSFileHandle"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "fileHandleForWritingToURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fileHandleForUpdatingURL:error:@
fileHandleForUpdatingURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSFileHandle)
fileHandleForUpdatingURL_error url error_ =
  do
    cls' <- getRequiredClass "NSFileHandle"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "fileHandleForUpdatingURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- availableData@
availableData :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO (Id NSData)
availableData nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "availableData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- offsetInFile@
offsetInFile :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO CULong
offsetInFile nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "offsetInFile") retCULong []

-- | @- fileDescriptor@
fileDescriptor :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO CInt
fileDescriptor nsFileHandle  =
  sendMsg nsFileHandle (mkSelector "fileDescriptor") retCInt []

-- | @- readabilityHandler@
readabilityHandler :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO (Ptr ())
readabilityHandler nsFileHandle  =
  fmap castPtr $ sendMsg nsFileHandle (mkSelector "readabilityHandler") (retPtr retVoid) []

-- | @- setReadabilityHandler:@
setReadabilityHandler :: IsNSFileHandle nsFileHandle => nsFileHandle -> Ptr () -> IO ()
setReadabilityHandler nsFileHandle  value =
  sendMsg nsFileHandle (mkSelector "setReadabilityHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- writeabilityHandler@
writeabilityHandler :: IsNSFileHandle nsFileHandle => nsFileHandle -> IO (Ptr ())
writeabilityHandler nsFileHandle  =
  fmap castPtr $ sendMsg nsFileHandle (mkSelector "writeabilityHandler") (retPtr retVoid) []

-- | @- setWriteabilityHandler:@
setWriteabilityHandler :: IsNSFileHandle nsFileHandle => nsFileHandle -> Ptr () -> IO ()
setWriteabilityHandler nsFileHandle  value =
  sendMsg nsFileHandle (mkSelector "setWriteabilityHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @+ fileHandleWithStandardInput@
fileHandleWithStandardInput :: IO (Id NSFileHandle)
fileHandleWithStandardInput  =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMsg cls' (mkSelector "fileHandleWithStandardInput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fileHandleWithStandardOutput@
fileHandleWithStandardOutput :: IO (Id NSFileHandle)
fileHandleWithStandardOutput  =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMsg cls' (mkSelector "fileHandleWithStandardOutput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fileHandleWithStandardError@
fileHandleWithStandardError :: IO (Id NSFileHandle)
fileHandleWithStandardError  =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMsg cls' (mkSelector "fileHandleWithStandardError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fileHandleWithNullDevice@
fileHandleWithNullDevice :: IO (Id NSFileHandle)
fileHandleWithNullDevice  =
  do
    cls' <- getRequiredClass "NSFileHandle"
    sendClassMsg cls' (mkSelector "fileHandleWithNullDevice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileDescriptor:closeOnDealloc:@
initWithFileDescriptor_closeOnDeallocSelector :: Selector
initWithFileDescriptor_closeOnDeallocSelector = mkSelector "initWithFileDescriptor:closeOnDealloc:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @readDataToEndOfFileAndReturnError:@
readDataToEndOfFileAndReturnErrorSelector :: Selector
readDataToEndOfFileAndReturnErrorSelector = mkSelector "readDataToEndOfFileAndReturnError:"

-- | @Selector@ for @readDataUpToLength:error:@
readDataUpToLength_errorSelector :: Selector
readDataUpToLength_errorSelector = mkSelector "readDataUpToLength:error:"

-- | @Selector@ for @writeData:error:@
writeData_errorSelector :: Selector
writeData_errorSelector = mkSelector "writeData:error:"

-- | @Selector@ for @getOffset:error:@
getOffset_errorSelector :: Selector
getOffset_errorSelector = mkSelector "getOffset:error:"

-- | @Selector@ for @seekToEndReturningOffset:error:@
seekToEndReturningOffset_errorSelector :: Selector
seekToEndReturningOffset_errorSelector = mkSelector "seekToEndReturningOffset:error:"

-- | @Selector@ for @seekToOffset:error:@
seekToOffset_errorSelector :: Selector
seekToOffset_errorSelector = mkSelector "seekToOffset:error:"

-- | @Selector@ for @truncateAtOffset:error:@
truncateAtOffset_errorSelector :: Selector
truncateAtOffset_errorSelector = mkSelector "truncateAtOffset:error:"

-- | @Selector@ for @synchronizeAndReturnError:@
synchronizeAndReturnErrorSelector :: Selector
synchronizeAndReturnErrorSelector = mkSelector "synchronizeAndReturnError:"

-- | @Selector@ for @closeAndReturnError:@
closeAndReturnErrorSelector :: Selector
closeAndReturnErrorSelector = mkSelector "closeAndReturnError:"

-- | @Selector@ for @readDataToEndOfFile@
readDataToEndOfFileSelector :: Selector
readDataToEndOfFileSelector = mkSelector "readDataToEndOfFile"

-- | @Selector@ for @readDataOfLength:@
readDataOfLengthSelector :: Selector
readDataOfLengthSelector = mkSelector "readDataOfLength:"

-- | @Selector@ for @writeData:@
writeDataSelector :: Selector
writeDataSelector = mkSelector "writeData:"

-- | @Selector@ for @seekToEndOfFile@
seekToEndOfFileSelector :: Selector
seekToEndOfFileSelector = mkSelector "seekToEndOfFile"

-- | @Selector@ for @seekToFileOffset:@
seekToFileOffsetSelector :: Selector
seekToFileOffsetSelector = mkSelector "seekToFileOffset:"

-- | @Selector@ for @truncateFileAtOffset:@
truncateFileAtOffsetSelector :: Selector
truncateFileAtOffsetSelector = mkSelector "truncateFileAtOffset:"

-- | @Selector@ for @synchronizeFile@
synchronizeFileSelector :: Selector
synchronizeFileSelector = mkSelector "synchronizeFile"

-- | @Selector@ for @closeFile@
closeFileSelector :: Selector
closeFileSelector = mkSelector "closeFile"

-- | @Selector@ for @initWithFileDescriptor:@
initWithFileDescriptorSelector :: Selector
initWithFileDescriptorSelector = mkSelector "initWithFileDescriptor:"

-- | @Selector@ for @readInBackgroundAndNotifyForModes:@
readInBackgroundAndNotifyForModesSelector :: Selector
readInBackgroundAndNotifyForModesSelector = mkSelector "readInBackgroundAndNotifyForModes:"

-- | @Selector@ for @readInBackgroundAndNotify@
readInBackgroundAndNotifySelector :: Selector
readInBackgroundAndNotifySelector = mkSelector "readInBackgroundAndNotify"

-- | @Selector@ for @readToEndOfFileInBackgroundAndNotifyForModes:@
readToEndOfFileInBackgroundAndNotifyForModesSelector :: Selector
readToEndOfFileInBackgroundAndNotifyForModesSelector = mkSelector "readToEndOfFileInBackgroundAndNotifyForModes:"

-- | @Selector@ for @readToEndOfFileInBackgroundAndNotify@
readToEndOfFileInBackgroundAndNotifySelector :: Selector
readToEndOfFileInBackgroundAndNotifySelector = mkSelector "readToEndOfFileInBackgroundAndNotify"

-- | @Selector@ for @acceptConnectionInBackgroundAndNotifyForModes:@
acceptConnectionInBackgroundAndNotifyForModesSelector :: Selector
acceptConnectionInBackgroundAndNotifyForModesSelector = mkSelector "acceptConnectionInBackgroundAndNotifyForModes:"

-- | @Selector@ for @acceptConnectionInBackgroundAndNotify@
acceptConnectionInBackgroundAndNotifySelector :: Selector
acceptConnectionInBackgroundAndNotifySelector = mkSelector "acceptConnectionInBackgroundAndNotify"

-- | @Selector@ for @waitForDataInBackgroundAndNotifyForModes:@
waitForDataInBackgroundAndNotifyForModesSelector :: Selector
waitForDataInBackgroundAndNotifyForModesSelector = mkSelector "waitForDataInBackgroundAndNotifyForModes:"

-- | @Selector@ for @waitForDataInBackgroundAndNotify@
waitForDataInBackgroundAndNotifySelector :: Selector
waitForDataInBackgroundAndNotifySelector = mkSelector "waitForDataInBackgroundAndNotify"

-- | @Selector@ for @fileHandleForReadingAtPath:@
fileHandleForReadingAtPathSelector :: Selector
fileHandleForReadingAtPathSelector = mkSelector "fileHandleForReadingAtPath:"

-- | @Selector@ for @fileHandleForWritingAtPath:@
fileHandleForWritingAtPathSelector :: Selector
fileHandleForWritingAtPathSelector = mkSelector "fileHandleForWritingAtPath:"

-- | @Selector@ for @fileHandleForUpdatingAtPath:@
fileHandleForUpdatingAtPathSelector :: Selector
fileHandleForUpdatingAtPathSelector = mkSelector "fileHandleForUpdatingAtPath:"

-- | @Selector@ for @fileHandleForReadingFromURL:error:@
fileHandleForReadingFromURL_errorSelector :: Selector
fileHandleForReadingFromURL_errorSelector = mkSelector "fileHandleForReadingFromURL:error:"

-- | @Selector@ for @fileHandleForWritingToURL:error:@
fileHandleForWritingToURL_errorSelector :: Selector
fileHandleForWritingToURL_errorSelector = mkSelector "fileHandleForWritingToURL:error:"

-- | @Selector@ for @fileHandleForUpdatingURL:error:@
fileHandleForUpdatingURL_errorSelector :: Selector
fileHandleForUpdatingURL_errorSelector = mkSelector "fileHandleForUpdatingURL:error:"

-- | @Selector@ for @availableData@
availableDataSelector :: Selector
availableDataSelector = mkSelector "availableData"

-- | @Selector@ for @offsetInFile@
offsetInFileSelector :: Selector
offsetInFileSelector = mkSelector "offsetInFile"

-- | @Selector@ for @fileDescriptor@
fileDescriptorSelector :: Selector
fileDescriptorSelector = mkSelector "fileDescriptor"

-- | @Selector@ for @readabilityHandler@
readabilityHandlerSelector :: Selector
readabilityHandlerSelector = mkSelector "readabilityHandler"

-- | @Selector@ for @setReadabilityHandler:@
setReadabilityHandlerSelector :: Selector
setReadabilityHandlerSelector = mkSelector "setReadabilityHandler:"

-- | @Selector@ for @writeabilityHandler@
writeabilityHandlerSelector :: Selector
writeabilityHandlerSelector = mkSelector "writeabilityHandler"

-- | @Selector@ for @setWriteabilityHandler:@
setWriteabilityHandlerSelector :: Selector
setWriteabilityHandlerSelector = mkSelector "setWriteabilityHandler:"

-- | @Selector@ for @fileHandleWithStandardInput@
fileHandleWithStandardInputSelector :: Selector
fileHandleWithStandardInputSelector = mkSelector "fileHandleWithStandardInput"

-- | @Selector@ for @fileHandleWithStandardOutput@
fileHandleWithStandardOutputSelector :: Selector
fileHandleWithStandardOutputSelector = mkSelector "fileHandleWithStandardOutput"

-- | @Selector@ for @fileHandleWithStandardError@
fileHandleWithStandardErrorSelector :: Selector
fileHandleWithStandardErrorSelector = mkSelector "fileHandleWithStandardError"

-- | @Selector@ for @fileHandleWithNullDevice@
fileHandleWithNullDeviceSelector :: Selector
fileHandleWithNullDeviceSelector = mkSelector "fileHandleWithNullDevice"

