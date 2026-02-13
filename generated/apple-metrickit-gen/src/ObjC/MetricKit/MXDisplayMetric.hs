{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXDisplayMetric
--
-- An MXMetric subclass that encapsulates display metrics.
--
-- Generated bindings for @MXDisplayMetric@.
module ObjC.MetricKit.MXDisplayMetric
  ( MXDisplayMetric
  , IsMXDisplayMetric(..)
  , averagePixelLuminance
  , averagePixelLuminanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | averagePixelLuminance
--
-- Average Pixel Luminance for the application.
--
-- APL data ranges from 0-100, in increments of 1.
--
-- This value is null when the device does not support APL.
--
-- Dimensioned as MXUnitAveragePixelLuminance.
--
-- ObjC selector: @- averagePixelLuminance@
averagePixelLuminance :: IsMXDisplayMetric mxDisplayMetric => mxDisplayMetric -> IO (Id MXAverage)
averagePixelLuminance mxDisplayMetric =
  sendMessage mxDisplayMetric averagePixelLuminanceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @averagePixelLuminance@
averagePixelLuminanceSelector :: Selector '[] (Id MXAverage)
averagePixelLuminanceSelector = mkSelector "averagePixelLuminance"

