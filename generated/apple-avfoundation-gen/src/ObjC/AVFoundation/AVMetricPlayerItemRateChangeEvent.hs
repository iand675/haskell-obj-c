{-# LANGUAGE DataKinds #-}
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
  , previousRateSelector
  , rateSelector
  , variantSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricPlayerItemRateChangeEvent avMetricPlayerItemRateChangeEvent => avMetricPlayerItemRateChangeEvent -> IO (Id AVMetricPlayerItemRateChangeEvent)
init_ avMetricPlayerItemRateChangeEvent =
  sendOwnedMessage avMetricPlayerItemRateChangeEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricPlayerItemRateChangeEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricPlayerItemRateChangeEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the playback rate after the rate change event.
--
-- ObjC selector: @- rate@
rate :: IsAVMetricPlayerItemRateChangeEvent avMetricPlayerItemRateChangeEvent => avMetricPlayerItemRateChangeEvent -> IO CDouble
rate avMetricPlayerItemRateChangeEvent =
  sendMessage avMetricPlayerItemRateChangeEvent rateSelector

-- | Returns the playback rate before the rate change event.
--
-- ObjC selector: @- previousRate@
previousRate :: IsAVMetricPlayerItemRateChangeEvent avMetricPlayerItemRateChangeEvent => avMetricPlayerItemRateChangeEvent -> IO CDouble
previousRate avMetricPlayerItemRateChangeEvent =
  sendMessage avMetricPlayerItemRateChangeEvent previousRateSelector

-- | Returns the variant being played at the time of rate change. If no value is present, returns nil.
--
-- ObjC selector: @- variant@
variant :: IsAVMetricPlayerItemRateChangeEvent avMetricPlayerItemRateChangeEvent => avMetricPlayerItemRateChangeEvent -> IO (Id AVAssetVariant)
variant avMetricPlayerItemRateChangeEvent =
  sendMessage avMetricPlayerItemRateChangeEvent variantSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricPlayerItemRateChangeEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricPlayerItemRateChangeEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CDouble
rateSelector = mkSelector "rate"

-- | @Selector@ for @previousRate@
previousRateSelector :: Selector '[] CDouble
previousRateSelector = mkSelector "previousRate"

-- | @Selector@ for @variant@
variantSelector :: Selector '[] (Id AVAssetVariant)
variantSelector = mkSelector "variant"

