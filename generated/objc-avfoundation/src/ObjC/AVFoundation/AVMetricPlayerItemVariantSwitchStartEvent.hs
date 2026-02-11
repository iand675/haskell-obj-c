{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when variant switch was attempted.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemVariantSwitchStartEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemVariantSwitchStartEvent
  ( AVMetricPlayerItemVariantSwitchStartEvent
  , IsAVMetricPlayerItemVariantSwitchStartEvent(..)
  , init_
  , new
  , fromVariant
  , toVariant
  , videoRendition
  , audioRendition
  , subtitleRendition
  , initSelector
  , newSelector
  , fromVariantSelector
  , toVariantSelector
  , videoRenditionSelector
  , audioRenditionSelector
  , subtitleRenditionSelector


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
init_ :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVMetricPlayerItemVariantSwitchStartEvent)
init_ avMetricPlayerItemVariantSwitchStartEvent  =
  sendMsg avMetricPlayerItemVariantSwitchStartEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricPlayerItemVariantSwitchStartEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemVariantSwitchStartEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the variant from which the switch is attempted. If no value is available, returns nil
--
-- ObjC selector: @- fromVariant@
fromVariant :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVAssetVariant)
fromVariant avMetricPlayerItemVariantSwitchStartEvent  =
  sendMsg avMetricPlayerItemVariantSwitchStartEvent (mkSelector "fromVariant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the variant to which the switch is attempted.
--
-- ObjC selector: @- toVariant@
toVariant :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVAssetVariant)
toVariant avMetricPlayerItemVariantSwitchStartEvent  =
  sendMsg avMetricPlayerItemVariantSwitchStartEvent (mkSelector "toVariant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoRendition
--
-- Contains information corresponding to the currently selected video rendition.
--
-- ObjC selector: @- videoRendition@
videoRendition :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVMetricMediaRendition)
videoRendition avMetricPlayerItemVariantSwitchStartEvent  =
  sendMsg avMetricPlayerItemVariantSwitchStartEvent (mkSelector "videoRendition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | audioRendition
--
-- Contains information corresponding to the currently selected audio rendition.
--
-- ObjC selector: @- audioRendition@
audioRendition :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVMetricMediaRendition)
audioRendition avMetricPlayerItemVariantSwitchStartEvent  =
  sendMsg avMetricPlayerItemVariantSwitchStartEvent (mkSelector "audioRendition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | subtitleRendition
--
-- Contains information corresponding to the currently selected subtitle rendition.
--
-- ObjC selector: @- subtitleRendition@
subtitleRendition :: IsAVMetricPlayerItemVariantSwitchStartEvent avMetricPlayerItemVariantSwitchStartEvent => avMetricPlayerItemVariantSwitchStartEvent -> IO (Id AVMetricMediaRendition)
subtitleRendition avMetricPlayerItemVariantSwitchStartEvent  =
  sendMsg avMetricPlayerItemVariantSwitchStartEvent (mkSelector "subtitleRendition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @fromVariant@
fromVariantSelector :: Selector
fromVariantSelector = mkSelector "fromVariant"

-- | @Selector@ for @toVariant@
toVariantSelector :: Selector
toVariantSelector = mkSelector "toVariant"

-- | @Selector@ for @videoRendition@
videoRenditionSelector :: Selector
videoRenditionSelector = mkSelector "videoRendition"

-- | @Selector@ for @audioRendition@
audioRenditionSelector :: Selector
audioRenditionSelector = mkSelector "audioRendition"

-- | @Selector@ for @subtitleRendition@
subtitleRenditionSelector :: Selector
subtitleRenditionSelector = mkSelector "subtitleRendition"

