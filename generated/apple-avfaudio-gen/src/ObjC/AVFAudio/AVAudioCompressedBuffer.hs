{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioCompressedBuffer
--
-- A subclass of AVAudioBuffer for use with compressed audio formats.
--
-- Generated bindings for @AVAudioCompressedBuffer@.
module ObjC.AVFAudio.AVAudioCompressedBuffer
  ( AVAudioCompressedBuffer
  , IsAVAudioCompressedBuffer(..)
  , initWithFormat_packetCapacity_maximumPacketSize
  , initWithFormat_packetCapacity
  , packetCapacity
  , packetCount
  , setPacketCount
  , maximumPacketSize
  , data_
  , byteCapacity
  , byteLength
  , setByteLength
  , packetDescriptions
  , packetDependencies
  , byteCapacitySelector
  , byteLengthSelector
  , dataSelector
  , initWithFormat_packetCapacitySelector
  , initWithFormat_packetCapacity_maximumPacketSizeSelector
  , maximumPacketSizeSelector
  , packetCapacitySelector
  , packetCountSelector
  , packetDependenciesSelector
  , packetDescriptionsSelector
  , setByteLengthSelector
  , setPacketCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithFormat:packetCapacity:maximumPacketSize:
--
-- Initialize a buffer that is to contain compressed audio data.
--
-- @format@ — The format of the audio to be contained in the buffer.
--
-- @packetCapacity@ — The capacity of the buffer in packets.
--
-- @maximumPacketSize@ — The maximum size in bytes of a compressed packet. 		The maximum packet size can be obtained from the maximumOutputPacketSize property of an AVAudioConverter configured for encoding this format.
--
-- An exception is raised if the format is PCM.
--
-- ObjC selector: @- initWithFormat:packetCapacity:maximumPacketSize:@
initWithFormat_packetCapacity_maximumPacketSize :: (IsAVAudioCompressedBuffer avAudioCompressedBuffer, IsAVAudioFormat format) => avAudioCompressedBuffer -> format -> CUInt -> CLong -> IO (Id AVAudioCompressedBuffer)
initWithFormat_packetCapacity_maximumPacketSize avAudioCompressedBuffer format packetCapacity maximumPacketSize =
  sendOwnedMessage avAudioCompressedBuffer initWithFormat_packetCapacity_maximumPacketSizeSelector (toAVAudioFormat format) packetCapacity maximumPacketSize

-- | initWithFormat:packetCapacity:
--
-- Initialize a buffer that is to contain constant bytes per packet compressed audio data.
--
-- @format@ — The format of the audio to be contained in the buffer.
--
-- @packetCapacity@ — The capacity of the buffer in packets.
--
-- This fails if the format is PCM or if the format has variable bytes per packet (format.streamDescription->mBytesPerPacket == 0).
--
-- ObjC selector: @- initWithFormat:packetCapacity:@
initWithFormat_packetCapacity :: (IsAVAudioCompressedBuffer avAudioCompressedBuffer, IsAVAudioFormat format) => avAudioCompressedBuffer -> format -> CUInt -> IO (Id AVAudioCompressedBuffer)
initWithFormat_packetCapacity avAudioCompressedBuffer format packetCapacity =
  sendOwnedMessage avAudioCompressedBuffer initWithFormat_packetCapacitySelector (toAVAudioFormat format) packetCapacity

-- | packetCapacity
--
-- The number of compressed packets the buffer can contain.
--
-- ObjC selector: @- packetCapacity@
packetCapacity :: IsAVAudioCompressedBuffer avAudioCompressedBuffer => avAudioCompressedBuffer -> IO CUInt
packetCapacity avAudioCompressedBuffer =
  sendMessage avAudioCompressedBuffer packetCapacitySelector

-- | packetCount
--
-- The current number of compressed packets in the buffer.
--
-- You may modify the packetCount as part of an operation that modifies its contents.		The packetCount must be less than or equal to the packetCapacity.
--
-- ObjC selector: @- packetCount@
packetCount :: IsAVAudioCompressedBuffer avAudioCompressedBuffer => avAudioCompressedBuffer -> IO CUInt
packetCount avAudioCompressedBuffer =
  sendMessage avAudioCompressedBuffer packetCountSelector

-- | packetCount
--
-- The current number of compressed packets in the buffer.
--
-- You may modify the packetCount as part of an operation that modifies its contents.		The packetCount must be less than or equal to the packetCapacity.
--
-- ObjC selector: @- setPacketCount:@
setPacketCount :: IsAVAudioCompressedBuffer avAudioCompressedBuffer => avAudioCompressedBuffer -> CUInt -> IO ()
setPacketCount avAudioCompressedBuffer value =
  sendMessage avAudioCompressedBuffer setPacketCountSelector value

-- | maximumPacketSize
--
-- The maximum size of a compressed packet in bytes.
--
-- ObjC selector: @- maximumPacketSize@
maximumPacketSize :: IsAVAudioCompressedBuffer avAudioCompressedBuffer => avAudioCompressedBuffer -> IO CLong
maximumPacketSize avAudioCompressedBuffer =
  sendMessage avAudioCompressedBuffer maximumPacketSizeSelector

-- | data
--
-- Access the buffer's data bytes.
--
-- ObjC selector: @- data@
data_ :: IsAVAudioCompressedBuffer avAudioCompressedBuffer => avAudioCompressedBuffer -> IO (Ptr ())
data_ avAudioCompressedBuffer =
  sendMessage avAudioCompressedBuffer dataSelector

-- | byteCapacity
--
-- The buffer's capacity in bytes
--
-- ObjC selector: @- byteCapacity@
byteCapacity :: IsAVAudioCompressedBuffer avAudioCompressedBuffer => avAudioCompressedBuffer -> IO CUInt
byteCapacity avAudioCompressedBuffer =
  sendMessage avAudioCompressedBuffer byteCapacitySelector

-- | byteLength
--
-- The current number of valid bytes in the buffer.
--
-- Can be changed as part of an operation that modifies the contents.
--
-- ObjC selector: @- byteLength@
byteLength :: IsAVAudioCompressedBuffer avAudioCompressedBuffer => avAudioCompressedBuffer -> IO CUInt
byteLength avAudioCompressedBuffer =
  sendMessage avAudioCompressedBuffer byteLengthSelector

-- | byteLength
--
-- The current number of valid bytes in the buffer.
--
-- Can be changed as part of an operation that modifies the contents.
--
-- ObjC selector: @- setByteLength:@
setByteLength :: IsAVAudioCompressedBuffer avAudioCompressedBuffer => avAudioCompressedBuffer -> CUInt -> IO ()
setByteLength avAudioCompressedBuffer value =
  sendMessage avAudioCompressedBuffer setByteLengthSelector value

-- | packetDescriptions
--
-- Access the buffer's array of packet descriptions, if any.
--
-- If the format has constant bytes per packet (format.streamDescription->mBytesPerPacket != 0), then this will return nil.
--
-- ObjC selector: @- packetDescriptions@
packetDescriptions :: IsAVAudioCompressedBuffer avAudioCompressedBuffer => avAudioCompressedBuffer -> IO RawId
packetDescriptions avAudioCompressedBuffer =
  sendMessage avAudioCompressedBuffer packetDescriptionsSelector

-- | packetDependencies
--
-- Access the buffer's array of packet dependencies, if any.
--
-- If the format doesn't employ packet dependencies, this will be nil.
--
-- ObjC selector: @- packetDependencies@
packetDependencies :: IsAVAudioCompressedBuffer avAudioCompressedBuffer => avAudioCompressedBuffer -> IO RawId
packetDependencies avAudioCompressedBuffer =
  sendMessage avAudioCompressedBuffer packetDependenciesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFormat:packetCapacity:maximumPacketSize:@
initWithFormat_packetCapacity_maximumPacketSizeSelector :: Selector '[Id AVAudioFormat, CUInt, CLong] (Id AVAudioCompressedBuffer)
initWithFormat_packetCapacity_maximumPacketSizeSelector = mkSelector "initWithFormat:packetCapacity:maximumPacketSize:"

-- | @Selector@ for @initWithFormat:packetCapacity:@
initWithFormat_packetCapacitySelector :: Selector '[Id AVAudioFormat, CUInt] (Id AVAudioCompressedBuffer)
initWithFormat_packetCapacitySelector = mkSelector "initWithFormat:packetCapacity:"

-- | @Selector@ for @packetCapacity@
packetCapacitySelector :: Selector '[] CUInt
packetCapacitySelector = mkSelector "packetCapacity"

-- | @Selector@ for @packetCount@
packetCountSelector :: Selector '[] CUInt
packetCountSelector = mkSelector "packetCount"

-- | @Selector@ for @setPacketCount:@
setPacketCountSelector :: Selector '[CUInt] ()
setPacketCountSelector = mkSelector "setPacketCount:"

-- | @Selector@ for @maximumPacketSize@
maximumPacketSizeSelector :: Selector '[] CLong
maximumPacketSizeSelector = mkSelector "maximumPacketSize"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Ptr ())
dataSelector = mkSelector "data"

-- | @Selector@ for @byteCapacity@
byteCapacitySelector :: Selector '[] CUInt
byteCapacitySelector = mkSelector "byteCapacity"

-- | @Selector@ for @byteLength@
byteLengthSelector :: Selector '[] CUInt
byteLengthSelector = mkSelector "byteLength"

-- | @Selector@ for @setByteLength:@
setByteLengthSelector :: Selector '[CUInt] ()
setByteLengthSelector = mkSelector "setByteLength:"

-- | @Selector@ for @packetDescriptions@
packetDescriptionsSelector :: Selector '[] RawId
packetDescriptionsSelector = mkSelector "packetDescriptions"

-- | @Selector@ for @packetDependencies@
packetDependenciesSelector :: Selector '[] RawId
packetDependenciesSelector = mkSelector "packetDependencies"

