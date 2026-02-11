{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXAppResponsivenessMetric
--
-- An MXMetric subclass that encapsulates app responsiveness metrics.
--
-- Generated bindings for @MXAppResponsivenessMetric@.
module ObjC.MetricKit.MXAppResponsivenessMetric
  ( MXAppResponsivenessMetric
  , IsMXAppResponsivenessMetric(..)
  , histogrammedApplicationHangTime
  , histogrammedApplicationHangTimeSelector


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

-- | histogrammedApplicationHangTime
--
-- Histogrammed app hang time data.
--
-- Applications are considered to be "hanging" when they are unable to handle user input responsively.
--
-- The durations for periods of hangs will be reported in the histogram returned here.
--
-- Application hang times that exceeds 9 seconds of wall clock time are reported in the final bucket of the histogram.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- histogrammedApplicationHangTime@
histogrammedApplicationHangTime :: IsMXAppResponsivenessMetric mxAppResponsivenessMetric => mxAppResponsivenessMetric -> IO (Id MXHistogram)
histogrammedApplicationHangTime mxAppResponsivenessMetric  =
  sendMsg mxAppResponsivenessMetric (mkSelector "histogrammedApplicationHangTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @histogrammedApplicationHangTime@
histogrammedApplicationHangTimeSelector :: Selector
histogrammedApplicationHangTimeSelector = mkSelector "histogrammedApplicationHangTime"

