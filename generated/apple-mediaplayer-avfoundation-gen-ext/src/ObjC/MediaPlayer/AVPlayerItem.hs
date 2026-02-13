{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlayerItem carries a reference to an AVAsset as well as presentation settings for that asset.
--
-- Note that inspection of media assets is provided by AVAsset. This class is intended to represent presentation state for an asset that's played by an AVPlayer and to permit observation of that state.
--
-- It is important to avoid key-value observation with a key path containing the asset's property. Observe the AVPlayerItem's property instead. For example, use the "duration" key path instead of the "asset.duration" key path.
--
-- To allow clients to add and remove their objects as key-value observers safely, AVPlayerItem serializes notifications of changes that occur dynamically during playback on the same dispatch queue on which notifications of playback state changes are serialized by its associated AVPlayer. By default, this queue is the main queue. See dispatch_get_main_queue().
--
-- Generated bindings for @AVPlayerItem@.
module ObjC.MediaPlayer.AVPlayerItem
  ( AVPlayerItem
  , IsAVPlayerItem(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.AVFoundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

