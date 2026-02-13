{-# LANGUAGE DataKinds #-}
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
  , audioBufferListSelector
  , formatSelector
  , mutableAudioBufferListSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
format avAudioBuffer =
  sendMessage avAudioBuffer formatSelector

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
audioBufferList avAudioBuffer =
  sendMessage avAudioBuffer audioBufferListSelector

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
mutableAudioBufferList avAudioBuffer =
  sendMessage avAudioBuffer mutableAudioBufferListSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @format@
formatSelector :: Selector '[] (Id AVAudioFormat)
formatSelector = mkSelector "format"

-- | @Selector@ for @audioBufferList@
audioBufferListSelector :: Selector '[] (Const RawId)
audioBufferListSelector = mkSelector "audioBufferList"

-- | @Selector@ for @mutableAudioBufferList@
mutableAudioBufferListSelector :: Selector '[] RawId
mutableAudioBufferListSelector = mkSelector "mutableAudioBufferList"

