{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , closeSelector
  , fileFormatSelector
  , framePositionSelector
  , initForReading_commonFormat_interleaved_errorSelector
  , initForReading_errorSelector
  , initForWriting_settings_commonFormat_interleaved_errorSelector
  , initForWriting_settings_errorSelector
  , initSelector
  , isOpenSelector
  , lengthSelector
  , processingFormatSelector
  , readIntoBuffer_errorSelector
  , readIntoBuffer_frameCount_errorSelector
  , setFramePositionSelector
  , urlSelector
  , writeFromBuffer_errorSelector

  -- * Enum types
  , AVAudioCommonFormat(AVAudioCommonFormat)
  , pattern AVAudioOtherFormat
  , pattern AVAudioPCMFormatFloat32
  , pattern AVAudioPCMFormatFloat64
  , pattern AVAudioPCMFormatInt16
  , pattern AVAudioPCMFormatInt32

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioFile avAudioFile => avAudioFile -> IO (Id AVAudioFile)
init_ avAudioFile =
  sendOwnedMessage avAudioFile initSelector

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
initForReading_error avAudioFile fileURL outError =
  sendOwnedMessage avAudioFile initForReading_errorSelector (toNSURL fileURL) (toNSError outError)

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
initForReading_commonFormat_interleaved_error avAudioFile fileURL format interleaved outError =
  sendOwnedMessage avAudioFile initForReading_commonFormat_interleaved_errorSelector (toNSURL fileURL) format interleaved (toNSError outError)

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
initForWriting_settings_error avAudioFile fileURL settings outError =
  sendOwnedMessage avAudioFile initForWriting_settings_errorSelector (toNSURL fileURL) (toNSDictionary settings) (toNSError outError)

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
initForWriting_settings_commonFormat_interleaved_error avAudioFile fileURL settings format interleaved outError =
  sendOwnedMessage avAudioFile initForWriting_settings_commonFormat_interleaved_errorSelector (toNSURL fileURL) (toNSDictionary settings) format interleaved (toNSError outError)

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
close avAudioFile =
  sendMessage avAudioFile closeSelector

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
readIntoBuffer_error avAudioFile buffer outError =
  sendMessage avAudioFile readIntoBuffer_errorSelector (toAVAudioPCMBuffer buffer) (toNSError outError)

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
readIntoBuffer_frameCount_error avAudioFile buffer frames outError =
  sendMessage avAudioFile readIntoBuffer_frameCount_errorSelector (toAVAudioPCMBuffer buffer) frames (toNSError outError)

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
writeFromBuffer_error avAudioFile buffer outError =
  sendMessage avAudioFile writeFromBuffer_errorSelector buffer (toNSError outError)

-- | isOpen
--
-- Whether the file is open or not.
--
-- ObjC selector: @- isOpen@
isOpen :: IsAVAudioFile avAudioFile => avAudioFile -> IO Bool
isOpen avAudioFile =
  sendMessage avAudioFile isOpenSelector

-- | url
--
-- The URL the file is reading or writing.
--
-- ObjC selector: @- url@
url :: IsAVAudioFile avAudioFile => avAudioFile -> IO (Id NSURL)
url avAudioFile =
  sendMessage avAudioFile urlSelector

-- | fileFormat
--
-- The on-disk format of the file.
--
-- ObjC selector: @- fileFormat@
fileFormat :: IsAVAudioFile avAudioFile => avAudioFile -> IO (Id AVAudioFormat)
fileFormat avAudioFile =
  sendMessage avAudioFile fileFormatSelector

-- | processingFormat
--
-- The processing format of the file.
--
-- ObjC selector: @- processingFormat@
processingFormat :: IsAVAudioFile avAudioFile => avAudioFile -> IO (Id AVAudioFormat)
processingFormat avAudioFile =
  sendMessage avAudioFile processingFormatSelector

-- | length
--
-- The number of sample frames in the file.
--
-- Note: this can be expensive to compute for the first time.
--
-- ObjC selector: @- length@
length_ :: IsAVAudioFile avAudioFile => avAudioFile -> IO CLong
length_ avAudioFile =
  sendMessage avAudioFile lengthSelector

-- | framePosition
--
-- The position in the file at which the next read or write will occur.
--
-- Set framePosition to perform a seek before a read or write. A read or write operation advances the frame position by the number of frames read or written.
--
-- ObjC selector: @- framePosition@
framePosition :: IsAVAudioFile avAudioFile => avAudioFile -> IO CLong
framePosition avAudioFile =
  sendMessage avAudioFile framePositionSelector

-- | framePosition
--
-- The position in the file at which the next read or write will occur.
--
-- Set framePosition to perform a seek before a read or write. A read or write operation advances the frame position by the number of frames read or written.
--
-- ObjC selector: @- setFramePosition:@
setFramePosition :: IsAVAudioFile avAudioFile => avAudioFile -> CLong -> IO ()
setFramePosition avAudioFile value =
  sendMessage avAudioFile setFramePositionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioFile)
initSelector = mkSelector "init"

-- | @Selector@ for @initForReading:error:@
initForReading_errorSelector :: Selector '[Id NSURL, Id NSError] (Id AVAudioFile)
initForReading_errorSelector = mkSelector "initForReading:error:"

-- | @Selector@ for @initForReading:commonFormat:interleaved:error:@
initForReading_commonFormat_interleaved_errorSelector :: Selector '[Id NSURL, AVAudioCommonFormat, Bool, Id NSError] (Id AVAudioFile)
initForReading_commonFormat_interleaved_errorSelector = mkSelector "initForReading:commonFormat:interleaved:error:"

-- | @Selector@ for @initForWriting:settings:error:@
initForWriting_settings_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSError] (Id AVAudioFile)
initForWriting_settings_errorSelector = mkSelector "initForWriting:settings:error:"

-- | @Selector@ for @initForWriting:settings:commonFormat:interleaved:error:@
initForWriting_settings_commonFormat_interleaved_errorSelector :: Selector '[Id NSURL, Id NSDictionary, AVAudioCommonFormat, Bool, Id NSError] (Id AVAudioFile)
initForWriting_settings_commonFormat_interleaved_errorSelector = mkSelector "initForWriting:settings:commonFormat:interleaved:error:"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @readIntoBuffer:error:@
readIntoBuffer_errorSelector :: Selector '[Id AVAudioPCMBuffer, Id NSError] Bool
readIntoBuffer_errorSelector = mkSelector "readIntoBuffer:error:"

-- | @Selector@ for @readIntoBuffer:frameCount:error:@
readIntoBuffer_frameCount_errorSelector :: Selector '[Id AVAudioPCMBuffer, CUInt, Id NSError] Bool
readIntoBuffer_frameCount_errorSelector = mkSelector "readIntoBuffer:frameCount:error:"

-- | @Selector@ for @writeFromBuffer:error:@
writeFromBuffer_errorSelector :: Selector '[Const (Id AVAudioPCMBuffer), Id NSError] Bool
writeFromBuffer_errorSelector = mkSelector "writeFromBuffer:error:"

-- | @Selector@ for @isOpen@
isOpenSelector :: Selector '[] Bool
isOpenSelector = mkSelector "isOpen"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @fileFormat@
fileFormatSelector :: Selector '[] (Id AVAudioFormat)
fileFormatSelector = mkSelector "fileFormat"

-- | @Selector@ for @processingFormat@
processingFormatSelector :: Selector '[] (Id AVAudioFormat)
processingFormatSelector = mkSelector "processingFormat"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CLong
lengthSelector = mkSelector "length"

-- | @Selector@ for @framePosition@
framePositionSelector :: Selector '[] CLong
framePositionSelector = mkSelector "framePosition"

-- | @Selector@ for @setFramePosition:@
setFramePositionSelector :: Selector '[CLong] ()
setFramePositionSelector = mkSelector "setFramePosition:"

