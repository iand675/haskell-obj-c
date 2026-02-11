{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when playback was likely to play through without stalling.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemLikelyToKeepUpEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemLikelyToKeepUpEvent
  ( AVMetricPlayerItemLikelyToKeepUpEvent
  , IsAVMetricPlayerItemLikelyToKeepUpEvent(..)
  , init_
  , new
  , variant
  , timeTaken
  , initSelector
  , newSelector
  , variantSelector
  , timeTakenSelector


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
init_ :: IsAVMetricPlayerItemLikelyToKeepUpEvent avMetricPlayerItemLikelyToKeepUpEvent => avMetricPlayerItemLikelyToKeepUpEvent -> IO (Id AVMetricPlayerItemLikelyToKeepUpEvent)
init_ avMetricPlayerItemLikelyToKeepUpEvent  =
  sendMsg avMetricPlayerItemLikelyToKeepUpEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricPlayerItemLikelyToKeepUpEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemLikelyToKeepUpEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the variant selected at the time likely to keep up is achieved. If no value is present, returns nil.
--
-- ObjC selector: @- variant@
variant :: IsAVMetricPlayerItemLikelyToKeepUpEvent avMetricPlayerItemLikelyToKeepUpEvent => avMetricPlayerItemLikelyToKeepUpEvent -> IO (Id AVAssetVariant)
variant avMetricPlayerItemLikelyToKeepUpEvent  =
  sendMsg avMetricPlayerItemLikelyToKeepUpEvent (mkSelector "variant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the total time taken to reach likely to keep up.
--
-- ObjC selector: @- timeTaken@
timeTaken :: IsAVMetricPlayerItemLikelyToKeepUpEvent avMetricPlayerItemLikelyToKeepUpEvent => avMetricPlayerItemLikelyToKeepUpEvent -> IO CDouble
timeTaken avMetricPlayerItemLikelyToKeepUpEvent  =
  sendMsg avMetricPlayerItemLikelyToKeepUpEvent (mkSelector "timeTaken") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @variant@
variantSelector :: Selector
variantSelector = mkSelector "variant"

-- | @Selector@ for @timeTaken@
timeTakenSelector :: Selector
timeTakenSelector = mkSelector "timeTaken"

