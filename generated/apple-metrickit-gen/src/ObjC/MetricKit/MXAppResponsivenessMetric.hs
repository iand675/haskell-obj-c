{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
histogrammedApplicationHangTime mxAppResponsivenessMetric =
  sendMessage mxAppResponsivenessMetric histogrammedApplicationHangTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @histogrammedApplicationHangTime@
histogrammedApplicationHangTimeSelector :: Selector '[] (Id MXHistogram)
histogrammedApplicationHangTimeSelector = mkSelector "histogrammedApplicationHangTime"

