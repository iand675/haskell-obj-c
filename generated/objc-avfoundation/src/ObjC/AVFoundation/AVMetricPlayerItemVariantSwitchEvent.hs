{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when variant switch was completed.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemVariantSwitchEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemVariantSwitchEvent
  ( AVMetricPlayerItemVariantSwitchEvent
  , IsAVMetricPlayerItemVariantSwitchEvent(..)
  , init_
  , new
  , fromVariant
  , toVariant
  , videoRendition
  , audioRendition
  , subtitleRendition
  , didSucceed
  , initSelector
  , newSelector
  , fromVariantSelector
  , toVariantSelector
  , videoRenditionSelector
  , audioRenditionSelector
  , subtitleRenditionSelector
  , didSucceedSelector


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
init_ :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVMetricPlayerItemVariantSwitchEvent)
init_ avMetricPlayerItemVariantSwitchEvent  =
  sendMsg avMetricPlayerItemVariantSwitchEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricPlayerItemVariantSwitchEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemVariantSwitchEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the variant before the switch. If no value is available, returns nil
--
-- ObjC selector: @- fromVariant@
fromVariant :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVAssetVariant)
fromVariant avMetricPlayerItemVariantSwitchEvent  =
  sendMsg avMetricPlayerItemVariantSwitchEvent (mkSelector "fromVariant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the variant after the switch.
--
-- ObjC selector: @- toVariant@
toVariant :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVAssetVariant)
toVariant avMetricPlayerItemVariantSwitchEvent  =
  sendMsg avMetricPlayerItemVariantSwitchEvent (mkSelector "toVariant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Represents the currently selected video rendition's identifiers.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- ObjC selector: @- videoRendition@
videoRendition :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVMetricMediaRendition)
videoRendition avMetricPlayerItemVariantSwitchEvent  =
  sendMsg avMetricPlayerItemVariantSwitchEvent (mkSelector "videoRendition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Represents the currently selected video rendition's identifiers.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- ObjC selector: @- audioRendition@
audioRendition :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVMetricMediaRendition)
audioRendition avMetricPlayerItemVariantSwitchEvent  =
  sendMsg avMetricPlayerItemVariantSwitchEvent (mkSelector "audioRendition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Represents the currently selected audio rendition's identifiers.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- ObjC selector: @- subtitleRendition@
subtitleRendition :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO (Id AVMetricMediaRendition)
subtitleRendition avMetricPlayerItemVariantSwitchEvent  =
  sendMsg avMetricPlayerItemVariantSwitchEvent (mkSelector "subtitleRendition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns if the switch did succeed.
--
-- ObjC selector: @- didSucceed@
didSucceed :: IsAVMetricPlayerItemVariantSwitchEvent avMetricPlayerItemVariantSwitchEvent => avMetricPlayerItemVariantSwitchEvent -> IO Bool
didSucceed avMetricPlayerItemVariantSwitchEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetricPlayerItemVariantSwitchEvent (mkSelector "didSucceed") retCULong []

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

-- | @Selector@ for @didSucceed@
didSucceedSelector :: Selector
didSucceedSelector = mkSelector "didSucceed"

