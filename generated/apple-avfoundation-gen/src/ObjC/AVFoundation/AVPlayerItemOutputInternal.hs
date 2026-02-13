{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerItemOutput
--
-- AVPlayerItemOutput is an abstract class encapsulating the common API for all AVPlayerItemOutput subclasses.
--
-- Instances of AVPlayerItemOutput permit the acquisition of individual samples from an AVAsset during playback by an AVPlayer. To provide graceful degradation of service across multiple AVPlayerItemOutput instances for a single source, all AVPlayerItemOutput subclasses only offer the current sample and/or any readily available future samples. All samples earlier than the current sample are automatically discarded by the AVPlayerItemOutput.
--
-- You manage an association of an AVPlayerItemOutput instance with an AVPlayerItem as the source input using the AVPlayerItem methods:
--
-- • addOutput:		• removeOutput:
--
-- When an AVPlayerItemOutput is associated with an AVPlayerItem, samples are provided for a media type in accordance with the rules for mixing, composition, or exclusion that the AVPlayer honors among multiple enabled tracks of that media type for its own rendering purposes. For example, video media will be composed according to the instructions provided via AVPlayerItem.videoComposition, if present.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemOutputInternal@.
module ObjC.AVFoundation.AVPlayerItemOutputInternal
  ( AVPlayerItemOutputInternal
  , IsAVPlayerItemOutputInternal(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

