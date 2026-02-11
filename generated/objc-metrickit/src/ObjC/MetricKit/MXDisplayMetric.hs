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
averagePixelLuminance mxDisplayMetric  =
  sendMsg mxDisplayMetric (mkSelector "averagePixelLuminance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @averagePixelLuminance@
averagePixelLuminanceSelector :: Selector
averagePixelLuminanceSelector = mkSelector "averagePixelLuminance"

