{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when playback seek completed.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemSeekDidCompleteEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemSeekDidCompleteEvent
  ( AVMetricPlayerItemSeekDidCompleteEvent
  , IsAVMetricPlayerItemSeekDidCompleteEvent(..)
  , init_
  , new
  , didSeekInBuffer
  , initSelector
  , newSelector
  , didSeekInBufferSelector


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

-- | @- init@
init_ :: IsAVMetricPlayerItemSeekDidCompleteEvent avMetricPlayerItemSeekDidCompleteEvent => avMetricPlayerItemSeekDidCompleteEvent -> IO (Id AVMetricPlayerItemSeekDidCompleteEvent)
init_ avMetricPlayerItemSeekDidCompleteEvent  =
  sendMsg avMetricPlayerItemSeekDidCompleteEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricPlayerItemSeekDidCompleteEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemSeekDidCompleteEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns whether the seek was performed within the available buffer.
--
-- ObjC selector: @- didSeekInBuffer@
didSeekInBuffer :: IsAVMetricPlayerItemSeekDidCompleteEvent avMetricPlayerItemSeekDidCompleteEvent => avMetricPlayerItemSeekDidCompleteEvent -> IO Bool
didSeekInBuffer avMetricPlayerItemSeekDidCompleteEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetricPlayerItemSeekDidCompleteEvent (mkSelector "didSeekInBuffer") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @didSeekInBuffer@
didSeekInBufferSelector :: Selector
didSeekInBufferSelector = mkSelector "didSeekInBuffer"

