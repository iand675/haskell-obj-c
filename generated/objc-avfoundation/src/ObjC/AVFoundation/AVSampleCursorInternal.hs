{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVSampleCursor
--
-- An AVSampleCursor is always positioned at a specific media sample in a sequence of media samples as defined by a higher-level construct, such as an AVAssetTrack. It can be moved to a new position in that sequence either backwards or forwards, either in decode order or in presentation order. Movement can be requested according to a count of samples or according to a delta in time.
--
-- AVSampleCursors can be compared by position within the sample sequence.	  AVSampleCursors can be used synchronously to perform I/O in order to load media data of one or more media samples into memory.	  An AVSampleCursor can provide information about the media sample at its current position, such as its duration, its presentation and decode timestamps, whether it can be decoded independently of other media samples, its offset and length in its storage container, and whether the track signals that the sample is intended to be loaded with other contiguous media samples in a "chunk".	  Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVSampleCursorInternal@.
module ObjC.AVFoundation.AVSampleCursorInternal
  ( AVSampleCursorInternal
  , IsAVSampleCursorInternal(..)


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

import ObjC.AVFoundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

