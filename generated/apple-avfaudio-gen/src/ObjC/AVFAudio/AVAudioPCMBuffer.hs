{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioPCMBuffer
--
-- A subclass of AVAudioBuffer for use with PCM audio formats.
--
-- AVAudioPCMBuffer provides a number of methods useful for manipulating buffers of		audio in PCM format.
--
-- Generated bindings for @AVAudioPCMBuffer@.
module ObjC.AVFAudio.AVAudioPCMBuffer
  ( AVAudioPCMBuffer
  , IsAVAudioPCMBuffer(..)
  , initWithPCMFormat_frameCapacity
  , initWithPCMFormat_bufferListNoCopy_deallocator
  , frameCapacity
  , frameLength
  , setFrameLength
  , stride
  , initWithPCMFormat_frameCapacitySelector
  , initWithPCMFormat_bufferListNoCopy_deallocatorSelector
  , frameCapacitySelector
  , frameLengthSelector
  , setFrameLengthSelector
  , strideSelector


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
import ObjC.Foundation.Internal.Classes

-- | initWithPCMFormat:frameCapacity:
--
-- Initialize a buffer that is to contain PCM audio samples.
--
-- @format@ — The format of the PCM audio to be contained in the buffer.
--
-- @frameCapacity@ — The capacity of the buffer in PCM sample frames.
--
-- An exception is raised if the format is not PCM.
--
-- Returns nil in the following cases:		- if the format has zero bytes per frame (format.streamDescription->mBytesPerFrame == 0)		- if the buffer byte capacity (frameCapacity * format.streamDescription->mBytesPerFrame)		  cannot be represented by an uint32_t
--
-- ObjC selector: @- initWithPCMFormat:frameCapacity:@
initWithPCMFormat_frameCapacity :: (IsAVAudioPCMBuffer avAudioPCMBuffer, IsAVAudioFormat format) => avAudioPCMBuffer -> format -> CUInt -> IO (Id AVAudioPCMBuffer)
initWithPCMFormat_frameCapacity avAudioPCMBuffer  format frameCapacity =
  withObjCPtr format $ \raw_format ->
      sendMsg avAudioPCMBuffer (mkSelector "initWithPCMFormat:frameCapacity:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argCUInt frameCapacity] >>= ownedObject . castPtr

-- | initWithPCMFormat:bufferListNoCopy:deallocator:
--
-- Initialize a buffer that is to contain PCM audio samples with a given AudioBufferList			  without copying samples and a custom deallocator block.
--
-- @format@ — The format of the PCM audio to be contained in the buffer.
--
-- @bufferList@ — The buffer list with allocated memory to contain the PCM audio data.
--
-- @deallocator@ — A block to invoke when the resulting AVAudioPCMBuffer object is deallocated.
--
-- An exception is raised if the format is not PCM.
--
-- Returns nil in the following cases:		- if the format has zero bytes per frame (format.streamDescription->mBytesPerFrame == 0)		- if supplied buffer has zero number of buffers		- if each buffer's data byte size are not equal or if any of the buffers' data byte size is zero		- if there is a mismatch between the format's number of buffers and the AudioBufferList's size			(1 if interleaved, mChannelsPerFrame if deinterleaved)		- if the AudioBufferList's pointer to the buffer of audio data is null.
--
-- Use the deallocator block to define your own deallocation behavior for the provided AudioBufferList's		underlying memory.
--
-- The AudioBufferList passed to the deallocator is identical to the one which was passed to the initializer,		in terms of the buffer count, and each buffer's mData and mDataByteSize members.
--
-- ObjC selector: @- initWithPCMFormat:bufferListNoCopy:deallocator:@
initWithPCMFormat_bufferListNoCopy_deallocator :: (IsAVAudioPCMBuffer avAudioPCMBuffer, IsAVAudioFormat format) => avAudioPCMBuffer -> format -> Const RawId -> Ptr () -> IO (Id AVAudioPCMBuffer)
initWithPCMFormat_bufferListNoCopy_deallocator avAudioPCMBuffer  format bufferList deallocator =
  withObjCPtr format $ \raw_format ->
      sendMsg avAudioPCMBuffer (mkSelector "initWithPCMFormat:bufferListNoCopy:deallocator:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr (unRawId (unConst bufferList)) :: Ptr ()), argPtr (castPtr deallocator :: Ptr ())] >>= ownedObject . castPtr

-- | frameCapacity
--
-- The buffer's capacity, in audio sample frames.
--
-- ObjC selector: @- frameCapacity@
frameCapacity :: IsAVAudioPCMBuffer avAudioPCMBuffer => avAudioPCMBuffer -> IO CUInt
frameCapacity avAudioPCMBuffer  =
    sendMsg avAudioPCMBuffer (mkSelector "frameCapacity") retCUInt []

-- | frameLength
--
-- The current number of valid sample frames in the buffer.
--
-- You may modify the length of the buffer as part of an operation that modifies its contents.		The length must be less than or equal to the frameCapacity. Modifying frameLength will update		the mDataByteSize in each of the underlying AudioBufferList's AudioBuffer's correspondingly,		and vice versa. Note that in the case of deinterleaved formats, mDataByteSize will refers		the size of one channel's worth of audio samples.
--
-- ObjC selector: @- frameLength@
frameLength :: IsAVAudioPCMBuffer avAudioPCMBuffer => avAudioPCMBuffer -> IO CUInt
frameLength avAudioPCMBuffer  =
    sendMsg avAudioPCMBuffer (mkSelector "frameLength") retCUInt []

-- | frameLength
--
-- The current number of valid sample frames in the buffer.
--
-- You may modify the length of the buffer as part of an operation that modifies its contents.		The length must be less than or equal to the frameCapacity. Modifying frameLength will update		the mDataByteSize in each of the underlying AudioBufferList's AudioBuffer's correspondingly,		and vice versa. Note that in the case of deinterleaved formats, mDataByteSize will refers		the size of one channel's worth of audio samples.
--
-- ObjC selector: @- setFrameLength:@
setFrameLength :: IsAVAudioPCMBuffer avAudioPCMBuffer => avAudioPCMBuffer -> CUInt -> IO ()
setFrameLength avAudioPCMBuffer  value =
    sendMsg avAudioPCMBuffer (mkSelector "setFrameLength:") retVoid [argCUInt value]

-- | stride
--
-- The buffer's number of interleaved channels.
--
-- Useful in conjunction with floatChannelData etc.
--
-- ObjC selector: @- stride@
stride :: IsAVAudioPCMBuffer avAudioPCMBuffer => avAudioPCMBuffer -> IO CULong
stride avAudioPCMBuffer  =
    sendMsg avAudioPCMBuffer (mkSelector "stride") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPCMFormat:frameCapacity:@
initWithPCMFormat_frameCapacitySelector :: Selector
initWithPCMFormat_frameCapacitySelector = mkSelector "initWithPCMFormat:frameCapacity:"

-- | @Selector@ for @initWithPCMFormat:bufferListNoCopy:deallocator:@
initWithPCMFormat_bufferListNoCopy_deallocatorSelector :: Selector
initWithPCMFormat_bufferListNoCopy_deallocatorSelector = mkSelector "initWithPCMFormat:bufferListNoCopy:deallocator:"

-- | @Selector@ for @frameCapacity@
frameCapacitySelector :: Selector
frameCapacitySelector = mkSelector "frameCapacity"

-- | @Selector@ for @frameLength@
frameLengthSelector :: Selector
frameLengthSelector = mkSelector "frameLength"

-- | @Selector@ for @setFrameLength:@
setFrameLengthSelector :: Selector
setFrameLengthSelector = mkSelector "setFrameLength:"

-- | @Selector@ for @stride@
strideSelector :: Selector
strideSelector = mkSelector "stride"

