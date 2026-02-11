{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioBuffer
--
-- A buffer of audio data, with a format.
--
-- AVAudioBuffer represents a buffer of audio data and its format.
--
-- Generated bindings for @AVAudioBuffer@.
module ObjC.AVFAudio.AVAudioBuffer
  ( AVAudioBuffer
  , IsAVAudioBuffer(..)
  , format
  , audioBufferList
  , mutableAudioBufferList
  , formatSelector
  , audioBufferListSelector
  , mutableAudioBufferListSelector


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

-- | format
--
-- The format of the audio in the buffer.
--
-- ObjC selector: @- format@
format :: IsAVAudioBuffer avAudioBuffer => avAudioBuffer -> IO (Id AVAudioFormat)
format avAudioBuffer  =
    sendMsg avAudioBuffer (mkSelector "format") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | audioBufferList
--
-- The buffer's underlying AudioBufferList.
--
-- For compatibility with lower-level CoreAudio and AudioToolbox API's, this method accesses		the buffer implementation's internal AudioBufferList. The buffer list structure must		not be modified, though you may modify buffer contents.
--
-- The mDataByteSize fields of this AudioBufferList express the buffer's current frameLength.
--
-- ObjC selector: @- audioBufferList@
audioBufferList :: IsAVAudioBuffer avAudioBuffer => avAudioBuffer -> IO (Const RawId)
audioBufferList avAudioBuffer  =
    fmap Const $ fmap (RawId . castPtr) $ sendMsg avAudioBuffer (mkSelector "audioBufferList") (retPtr retVoid) []

-- | mutableAudioBufferList
--
-- A mutable version of the buffer's underlying AudioBufferList.
--
-- Some lower-level CoreAudio and AudioToolbox API's require a mutable AudioBufferList,		for example, AudioConverterConvertComplexBuffer.
--
-- The mDataByteSize fields of this AudioBufferList express the buffer's current frameCapacity.		If they are altered, you should modify the buffer's frameLength to match.
--
-- ObjC selector: @- mutableAudioBufferList@
mutableAudioBufferList :: IsAVAudioBuffer avAudioBuffer => avAudioBuffer -> IO RawId
mutableAudioBufferList avAudioBuffer  =
    fmap (RawId . castPtr) $ sendMsg avAudioBuffer (mkSelector "mutableAudioBufferList") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

-- | @Selector@ for @audioBufferList@
audioBufferListSelector :: Selector
audioBufferListSelector = mkSelector "audioBufferList"

-- | @Selector@ for @mutableAudioBufferList@
mutableAudioBufferListSelector :: Selector
mutableAudioBufferListSelector = mkSelector "mutableAudioBufferList"

