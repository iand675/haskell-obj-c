{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MEDecodeFrameOptions
--
-- Conveys directives or options from the VideoToolbox to guide decoder operation on a per-frame basis.
--
-- Generated bindings for @MEDecodeFrameOptions@.
module ObjC.MediaExtension.MEDecodeFrameOptions
  ( MEDecodeFrameOptions
  , IsMEDecodeFrameOptions(..)
  , doNotOutputFrame
  , setDoNotOutputFrame
  , realTimePlayback
  , setRealTimePlayback
  , doNotOutputFrameSelector
  , realTimePlaybackSelector
  , setDoNotOutputFrameSelector
  , setRealTimePlaybackSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | doNotOutputFrame
--
-- A hint to the video decoder that a CVImageBuffer should not be emitted for this frame.  NULL will be returned instead.
--
-- ObjC selector: @- doNotOutputFrame@
doNotOutputFrame :: IsMEDecodeFrameOptions meDecodeFrameOptions => meDecodeFrameOptions -> IO Bool
doNotOutputFrame meDecodeFrameOptions =
  sendMessage meDecodeFrameOptions doNotOutputFrameSelector

-- | doNotOutputFrame
--
-- A hint to the video decoder that a CVImageBuffer should not be emitted for this frame.  NULL will be returned instead.
--
-- ObjC selector: @- setDoNotOutputFrame:@
setDoNotOutputFrame :: IsMEDecodeFrameOptions meDecodeFrameOptions => meDecodeFrameOptions -> Bool -> IO ()
setDoNotOutputFrame meDecodeFrameOptions value =
  sendMessage meDecodeFrameOptions setDoNotOutputFrameSelector value

-- | realTimePlayback
--
-- A hint to the video decoder that it would be OK to use a low-power mode that can not decode faster than 1x realtime.
--
-- Note that this hint only takes the current decode session into account.  For example, if multiple instances of a decoder are operating at once, it may not actually be OK to use such a low-power mode if real-time playback might not be sustained across all the streams. This hint will be set to false during all uses other than 1x forward real-time playback, including seeking, playback at other rates, and export.
--
-- ObjC selector: @- realTimePlayback@
realTimePlayback :: IsMEDecodeFrameOptions meDecodeFrameOptions => meDecodeFrameOptions -> IO Bool
realTimePlayback meDecodeFrameOptions =
  sendMessage meDecodeFrameOptions realTimePlaybackSelector

-- | realTimePlayback
--
-- A hint to the video decoder that it would be OK to use a low-power mode that can not decode faster than 1x realtime.
--
-- Note that this hint only takes the current decode session into account.  For example, if multiple instances of a decoder are operating at once, it may not actually be OK to use such a low-power mode if real-time playback might not be sustained across all the streams. This hint will be set to false during all uses other than 1x forward real-time playback, including seeking, playback at other rates, and export.
--
-- ObjC selector: @- setRealTimePlayback:@
setRealTimePlayback :: IsMEDecodeFrameOptions meDecodeFrameOptions => meDecodeFrameOptions -> Bool -> IO ()
setRealTimePlayback meDecodeFrameOptions value =
  sendMessage meDecodeFrameOptions setRealTimePlaybackSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @doNotOutputFrame@
doNotOutputFrameSelector :: Selector '[] Bool
doNotOutputFrameSelector = mkSelector "doNotOutputFrame"

-- | @Selector@ for @setDoNotOutputFrame:@
setDoNotOutputFrameSelector :: Selector '[Bool] ()
setDoNotOutputFrameSelector = mkSelector "setDoNotOutputFrame:"

-- | @Selector@ for @realTimePlayback@
realTimePlaybackSelector :: Selector '[] Bool
realTimePlaybackSelector = mkSelector "realTimePlayback"

-- | @Selector@ for @setRealTimePlayback:@
setRealTimePlaybackSelector :: Selector '[Bool] ()
setRealTimePlaybackSelector = mkSelector "setRealTimePlayback:"

