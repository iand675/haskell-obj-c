{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVPlayerItemOutput@.
module ObjC.AVFoundation.AVPlayerItemOutput
  ( AVPlayerItemOutput
  , IsAVPlayerItemOutput(..)
  , suppressesPlayerRendering
  , setSuppressesPlayerRendering
  , setSuppressesPlayerRenderingSelector
  , suppressesPlayerRenderingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | suppressesPlayerRendering
--
-- Indicates whether the output, when added to an AVPlayerItem, will be used in addition to normal rendering of media data by the player or instead of normal rendering.
--
-- The default value is NO, indicating that the output will be used in addition to normal rendering. If you want to render the media data provided by the output yourself instead of allowing it to be rendered as in normally would be by AVPlayer, set suppressesPlayerRendering to YES.
--
-- Whenever any output is added to an AVPlayerItem that has suppressesPlayerRendering set to YES, the media data supplied to the output will not be rendered by AVPlayer. Other media data associated with the item but not provided to such an output is not affected. For example, if an output of class AVPlayerItemVideoOutput with a value of YES for suppressesPlayerRendering is added to an AVPlayerItem, video media for that item will not be rendered by the AVPlayer, while audio media, subtitle media, and other kinds of media, if present, will be rendered.
--
-- ObjC selector: @- suppressesPlayerRendering@
suppressesPlayerRendering :: IsAVPlayerItemOutput avPlayerItemOutput => avPlayerItemOutput -> IO Bool
suppressesPlayerRendering avPlayerItemOutput =
  sendMessage avPlayerItemOutput suppressesPlayerRenderingSelector

-- | suppressesPlayerRendering
--
-- Indicates whether the output, when added to an AVPlayerItem, will be used in addition to normal rendering of media data by the player or instead of normal rendering.
--
-- The default value is NO, indicating that the output will be used in addition to normal rendering. If you want to render the media data provided by the output yourself instead of allowing it to be rendered as in normally would be by AVPlayer, set suppressesPlayerRendering to YES.
--
-- Whenever any output is added to an AVPlayerItem that has suppressesPlayerRendering set to YES, the media data supplied to the output will not be rendered by AVPlayer. Other media data associated with the item but not provided to such an output is not affected. For example, if an output of class AVPlayerItemVideoOutput with a value of YES for suppressesPlayerRendering is added to an AVPlayerItem, video media for that item will not be rendered by the AVPlayer, while audio media, subtitle media, and other kinds of media, if present, will be rendered.
--
-- ObjC selector: @- setSuppressesPlayerRendering:@
setSuppressesPlayerRendering :: IsAVPlayerItemOutput avPlayerItemOutput => avPlayerItemOutput -> Bool -> IO ()
setSuppressesPlayerRendering avPlayerItemOutput value =
  sendMessage avPlayerItemOutput setSuppressesPlayerRenderingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @suppressesPlayerRendering@
suppressesPlayerRenderingSelector :: Selector '[] Bool
suppressesPlayerRenderingSelector = mkSelector "suppressesPlayerRendering"

-- | @Selector@ for @setSuppressesPlayerRendering:@
setSuppressesPlayerRenderingSelector :: Selector '[Bool] ()
setSuppressesPlayerRenderingSelector = mkSelector "setSuppressesPlayerRendering:"

