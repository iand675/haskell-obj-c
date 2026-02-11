{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioFile
--
-- An audio file opened for reading or writing.
--
-- Regardless of the file's actual format, reading and writing the file is done via 		@AVAudioPCMBuffer@ objects, containing samples in an @AVAudioCommonFormat@,		referred to as the file's "processing format." Conversions are performed to and from		the file's actual format.
--
-- Reads and writes are always sequential, but random access is possible by setting the		framePosition property.
--
-- Generated bindings for @AVAudioFile@.
module ObjC.AVFAudio.AVAudioFile
  ( AVAudioFile
  , IsAVAudioFile(..)
  , init_
  , initForReading_error
  , initForReading_commonFormat_interleaved_error
  , initForWriting_settings_error
  , initForWriting_settings_commonFormat_interleaved_error
  , close
  , readIntoBuffer_error
  , readIntoBuffer_frameCount_error
  , writeFromBuffer_error
  , isOpen
  , url
  , fileFormat
  , processingFormat
  , length_
  , framePosition
  , setFramePosition
  , initSelector
  , initForReading_errorSelector
  , initForReading_commonFormat_interleaved_errorSelector
  , initForWriting_settings_errorSelector
  , initForWriting_settings_commonFormat_interleaved_errorSelector
  , closeSelector
  , readIntoBuffer_errorSelector
  , readIntoBuffer_frameCount_errorSelector
  , writeFromBuffer_errorSelector
  , isOpenSelector
  , urlSelector
  , fileFormatSelector
  , processingFormatSelector
  , lengthSelector
  , framePositionSelector
  , setFramePositionSelector

  -- * Enum types
  , AVAudioCommonFormat(AVAudioCommonFormat)
  , pattern AVAudioOtherFormat
  , pattern AVAudioPCMFormatFloat32
  , pattern AVAudioPCMFormatFloat64
  , pattern AVAudioPCMFormatInt16
  , pattern AVAudioPCMFormatInt32

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

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioFile avAudioFile => avAudioFile -> IO (Id AVAudioFile)
init_ avAudioFile  =
  sendMsg avAudioFile (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initForReading:error:
--
-- Open a file for reading.
--
-- @fileURL@ — the file to open
--
-- @outError@ — on exit, if an error occurs, a description of the error
--
-- This opens the file for reading using the standard format (deinterleaved floating point).
--
-- ObjC selector: @- initForReading:error:@
initForReading_error :: (IsAVAudioFile avAudioFile, IsNSURL fileURL, IsNSError outError) => avAudioFile -> fileURL -> outError -> IO (Id AVAudioFile)
initForReading_error avAudioFile  fileURL outError =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg avAudioFile (mkSelector "initForReading:error:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | initForReading:commonFormat:interleaved:error:
--
-- Open a file for reading, using a specified processing format.
--
-- @fileURL@ — the file to open
--
-- @format@ — the processing format to use when reading from the file
--
-- @interleaved@ — whether to use an interleaved processing format
--
-- @outError@ — on exit, if an error occurs, a description of the error
--
-- ObjC selector: @- initForReading:commonFormat:interleaved:error:@
initForReading_commonFormat_interleaved_error :: (IsAVAudioFile avAudioFile, IsNSURL fileURL, IsNSError outError) => avAudioFile -> fileURL -> AVAudioCommonFormat -> Bool -> outError -> IO (Id AVAudioFile)
initForReading_commonFormat_interleaved_error avAudioFile  fileURL format interleaved outError =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg avAudioFile (mkSelector "initForReading:commonFormat:interleaved:error:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ()), argCULong (coerce format), argCULong (if interleaved then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | initForWriting:settings:error:
--
-- Open a file for writing.
--
-- @fileURL@ — the path at which to create the file
--
-- @settings@ — the format of the file to create (See @AVAudioRecorder@.)  For linear PCM,		only interleaved formats are supported for the saved file, non interleaved setting will be		ignored and a warning is shown.
--
-- @outError@ — on exit, if an error occurs, a description of the error
--
-- The file type to create can be set through the corresponding settings key. If not set, it will be		inferred from the file extension. Will overwrite a file at the specified URL if a file exists.
--
-- This opens the file for writing using the standard format (deinterleaved floating point).
--
-- ObjC selector: @- initForWriting:settings:error:@
initForWriting_settings_error :: (IsAVAudioFile avAudioFile, IsNSURL fileURL, IsNSDictionary settings, IsNSError outError) => avAudioFile -> fileURL -> settings -> outError -> IO (Id AVAudioFile)
initForWriting_settings_error avAudioFile  fileURL settings outError =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr settings $ \raw_settings ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avAudioFile (mkSelector "initForWriting:settings:error:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_settings :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | initForWriting:settings:commonFormat:interleaved:error:
--
-- Open a file for writing.
--
-- @fileURL@ — the path at which to create the file
--
-- @settings@ — the format of the file to create (See @AVAudioRecorder@.) For linear PCM,		only interleaved formats are supported for the saved file, non interleaved setting will be		ignored and a warning is shown.
--
-- @format@ — the processing format to use when writing to the file.
--
-- @interleaved@ — whether to use an interleaved processing format
--
-- @outError@ — on exit, if an error occurs, a description of the error
--
-- The file type to create can be set through the corresponding settings key. If not set, it will be		inferred from the file extension. Will overwrite a file at the specified URL if a file exists.
--
-- ObjC selector: @- initForWriting:settings:commonFormat:interleaved:error:@
initForWriting_settings_commonFormat_interleaved_error :: (IsAVAudioFile avAudioFile, IsNSURL fileURL, IsNSDictionary settings, IsNSError outError) => avAudioFile -> fileURL -> settings -> AVAudioCommonFormat -> Bool -> outError -> IO (Id AVAudioFile)
initForWriting_settings_commonFormat_interleaved_error avAudioFile  fileURL settings format interleaved outError =
withObjCPtr fileURL $ \raw_fileURL ->
  withObjCPtr settings $ \raw_settings ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avAudioFile (mkSelector "initForWriting:settings:commonFormat:interleaved:error:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_settings :: Ptr ()), argCULong (coerce format), argCULong (if interleaved then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | close
--
-- Close the audio file.
--
-- The underlying file will be closed if open.
--
-- - It is normally unnecessary to close a file opened for reading (it will be automatically closed		when the object is released)		- It is only necessary to close a file opened for writing in order to achieve specific control over		when the file's header is updated.
--
-- Note: Once closed, further file read or write operations will fail with kAudio_FileNotFoundError.
--
-- ObjC selector: @- close@
close :: IsAVAudioFile avAudioFile => avAudioFile -> IO ()
close avAudioFile  =
  sendMsg avAudioFile (mkSelector "close") retVoid []

-- | readIntoBuffer:error:
--
-- Read an entire buffer.
--
-- @buffer@ — The buffer into which to read from the file. Its format must match the file's		processing format.
--
-- @outError@ — on exit, if an error occurs, a description of the error
--
-- Returns: YES for success.
--
-- Reading sequentially from framePosition, attempts to fill the buffer to its capacity. On		return, the buffer's length indicates the number of sample frames successfully read.
--
-- ObjC selector: @- readIntoBuffer:error:@
readIntoBuffer_error :: (IsAVAudioFile avAudioFile, IsAVAudioPCMBuffer buffer, IsNSError outError) => avAudioFile -> buffer -> outError -> IO Bool
readIntoBuffer_error avAudioFile  buffer outError =
withObjCPtr buffer $ \raw_buffer ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioFile (mkSelector "readIntoBuffer:error:") retCULong [argPtr (castPtr raw_buffer :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | readIntoBuffer:frameCount:error:
--
-- Read a portion of a buffer.
--
-- @frames@ — The number of frames to read.
--
-- @buffer@ — The buffer into which to read from the file. Its format must match the file's		processing format.
--
-- @outError@ — on exit, if an error occurs, a description of the error
--
-- Returns: YES for success.
--
-- Like @readIntoBuffer:error:@, but can be used to read fewer frames than buffer.frameCapacity.
--
-- ObjC selector: @- readIntoBuffer:frameCount:error:@
readIntoBuffer_frameCount_error :: (IsAVAudioFile avAudioFile, IsAVAudioPCMBuffer buffer, IsNSError outError) => avAudioFile -> buffer -> CUInt -> outError -> IO Bool
readIntoBuffer_frameCount_error avAudioFile  buffer frames outError =
withObjCPtr buffer $ \raw_buffer ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioFile (mkSelector "readIntoBuffer:frameCount:error:") retCULong [argPtr (castPtr raw_buffer :: Ptr ()), argCUInt (fromIntegral frames), argPtr (castPtr raw_outError :: Ptr ())]

-- | writeFromBuffer:error:
--
-- Write a buffer.
--
-- @buffer@ — The buffer from which to write to the file. Its format must match the file's		processing format.
--
-- @outError@ — on exit, if an error occurs, a description of the error
--
-- Returns: YES for success.
--
-- Writes sequentially. The buffer's frameLength signifies how much of the buffer is to be written.
--
-- ObjC selector: @- writeFromBuffer:error:@
writeFromBuffer_error :: (IsAVAudioFile avAudioFile, IsNSError outError) => avAudioFile -> Const (Id AVAudioPCMBuffer) -> outError -> IO Bool
writeFromBuffer_error avAudioFile  buffer outError =
withObjCPtr buffer $ \raw_buffer ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioFile (mkSelector "writeFromBuffer:error:") retCULong [argPtr (castPtr raw_buffer :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | isOpen
--
-- Whether the file is open or not.
--
-- ObjC selector: @- isOpen@
isOpen :: IsAVAudioFile avAudioFile => avAudioFile -> IO Bool
isOpen avAudioFile  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioFile (mkSelector "isOpen") retCULong []

-- | url
--
-- The URL the file is reading or writing.
--
-- ObjC selector: @- url@
url :: IsAVAudioFile avAudioFile => avAudioFile -> IO (Id NSURL)
url avAudioFile  =
  sendMsg avAudioFile (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fileFormat
--
-- The on-disk format of the file.
--
-- ObjC selector: @- fileFormat@
fileFormat :: IsAVAudioFile avAudioFile => avAudioFile -> IO (Id AVAudioFormat)
fileFormat avAudioFile  =
  sendMsg avAudioFile (mkSelector "fileFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | processingFormat
--
-- The processing format of the file.
--
-- ObjC selector: @- processingFormat@
processingFormat :: IsAVAudioFile avAudioFile => avAudioFile -> IO (Id AVAudioFormat)
processingFormat avAudioFile  =
  sendMsg avAudioFile (mkSelector "processingFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | length
--
-- The number of sample frames in the file.
--
-- Note: this can be expensive to compute for the first time.
--
-- ObjC selector: @- length@
length_ :: IsAVAudioFile avAudioFile => avAudioFile -> IO CLong
length_ avAudioFile  =
  sendMsg avAudioFile (mkSelector "length") retCLong []

-- | framePosition
--
-- The position in the file at which the next read or write will occur.
--
-- Set framePosition to perform a seek before a read or write. A read or write operation advances the frame position by the number of frames read or written.
--
-- ObjC selector: @- framePosition@
framePosition :: IsAVAudioFile avAudioFile => avAudioFile -> IO CLong
framePosition avAudioFile  =
  sendMsg avAudioFile (mkSelector "framePosition") retCLong []

-- | framePosition
--
-- The position in the file at which the next read or write will occur.
--
-- Set framePosition to perform a seek before a read or write. A read or write operation advances the frame position by the number of frames read or written.
--
-- ObjC selector: @- setFramePosition:@
setFramePosition :: IsAVAudioFile avAudioFile => avAudioFile -> CLong -> IO ()
setFramePosition avAudioFile  value =
  sendMsg avAudioFile (mkSelector "setFramePosition:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initForReading:error:@
initForReading_errorSelector :: Selector
initForReading_errorSelector = mkSelector "initForReading:error:"

-- | @Selector@ for @initForReading:commonFormat:interleaved:error:@
initForReading_commonFormat_interleaved_errorSelector :: Selector
initForReading_commonFormat_interleaved_errorSelector = mkSelector "initForReading:commonFormat:interleaved:error:"

-- | @Selector@ for @initForWriting:settings:error:@
initForWriting_settings_errorSelector :: Selector
initForWriting_settings_errorSelector = mkSelector "initForWriting:settings:error:"

-- | @Selector@ for @initForWriting:settings:commonFormat:interleaved:error:@
initForWriting_settings_commonFormat_interleaved_errorSelector :: Selector
initForWriting_settings_commonFormat_interleaved_errorSelector = mkSelector "initForWriting:settings:commonFormat:interleaved:error:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @readIntoBuffer:error:@
readIntoBuffer_errorSelector :: Selector
readIntoBuffer_errorSelector = mkSelector "readIntoBuffer:error:"

-- | @Selector@ for @readIntoBuffer:frameCount:error:@
readIntoBuffer_frameCount_errorSelector :: Selector
readIntoBuffer_frameCount_errorSelector = mkSelector "readIntoBuffer:frameCount:error:"

-- | @Selector@ for @writeFromBuffer:error:@
writeFromBuffer_errorSelector :: Selector
writeFromBuffer_errorSelector = mkSelector "writeFromBuffer:error:"

-- | @Selector@ for @isOpen@
isOpenSelector :: Selector
isOpenSelector = mkSelector "isOpen"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @fileFormat@
fileFormatSelector :: Selector
fileFormatSelector = mkSelector "fileFormat"

-- | @Selector@ for @processingFormat@
processingFormatSelector :: Selector
processingFormatSelector = mkSelector "processingFormat"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @framePosition@
framePositionSelector :: Selector
framePositionSelector = mkSelector "framePosition"

-- | @Selector@ for @setFramePosition:@
setFramePositionSelector :: Selector
setFramePositionSelector = mkSelector "setFramePosition:"

