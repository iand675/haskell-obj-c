{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerItemMediaDataCollector
--
-- AVPlayerItemMediaDataCollector is an abstract class encapsulating the common API for all AVPlayerItemMediaDataCollector subclasses.
--
-- Instances of AVPlayerItemMediaDataCollector permit the collection of media data from an AVAsset during playback by an AVPlayer. As opposed to AVPlayerItemOutputs, AVPlayerItemMediaDataCollectors collect all media data across an AVPlayerItem's timebase, relevant to the specific collector being used. Attaching an AVPlayerItemMediaDataCollector may incur additional I/O accordingly.
--
-- You manage an association of an AVPlayerItemMediaDataCollector instance with an AVPlayerItem as the source input using the AVPlayerItem methods:
--
-- • addMediaDataCollector:		• removeMediaDataCollector:
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemMediaDataCollector@.
module ObjC.AVFoundation.AVPlayerItemMediaDataCollector
  ( AVPlayerItemMediaDataCollector
  , IsAVPlayerItemMediaDataCollector(..)


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
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

