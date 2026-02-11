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
  , formatSelector


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

