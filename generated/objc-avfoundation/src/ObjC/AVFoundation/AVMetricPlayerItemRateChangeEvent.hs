{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event when playback rate change occurred.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricPlayerItemRateChangeEvent@.
module ObjC.AVFoundation.AVMetricPlayerItemRateChangeEvent
  ( AVMetricPlayerItemRateChangeEvent
  , IsAVMetricPlayerItemRateChangeEvent(..)
  , init_
  , new
  , rate
  , previousRate
  , variant
  , initSelector
  , newSelector
  , rateSelector
  , previousRateSelector
  , variantSelector


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
init_ :: IsAVMetricPlayerItemRateChangeEvent avMetricPlayerItemRateChangeEvent => avMetricPlayerItemRateChangeEvent -> IO (Id AVMetricPlayerItemRateChangeEvent)
init_ avMetricPlayerItemRateChangeEvent  =
  sendMsg avMetricPlayerItemRateChangeEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricPlayerItemRateChangeEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemRateChangeEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the playback rate after the rate change event.
--
-- ObjC selector: @- rate@
rate :: IsAVMetricPlayerItemRateChangeEvent avMetricPlayerItemRateChangeEvent => avMetricPlayerItemRateChangeEvent -> IO CDouble
rate avMetricPlayerItemRateChangeEvent  =
  sendMsg avMetricPlayerItemRateChangeEvent (mkSelector "rate") retCDouble []

-- | Returns the playback rate before the rate change event.
--
-- ObjC selector: @- previousRate@
previousRate :: IsAVMetricPlayerItemRateChangeEvent avMetricPlayerItemRateChangeEvent => avMetricPlayerItemRateChangeEvent -> IO CDouble
previousRate avMetricPlayerItemRateChangeEvent  =
  sendMsg avMetricPlayerItemRateChangeEvent (mkSelector "previousRate") retCDouble []

-- | Returns the variant being played at the time of rate change. If no value is present, returns nil.
--
-- ObjC selector: @- variant@
variant :: IsAVMetricPlayerItemRateChangeEvent avMetricPlayerItemRateChangeEvent => avMetricPlayerItemRateChangeEvent -> IO (Id AVAssetVariant)
variant avMetricPlayerItemRateChangeEvent  =
  sendMsg avMetricPlayerItemRateChangeEvent (mkSelector "variant") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @previousRate@
previousRateSelector :: Selector
previousRateSelector = mkSelector "previousRate"

-- | @Selector@ for @variant@
variantSelector :: Selector
variantSelector = mkSelector "variant"

