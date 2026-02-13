{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXAppLaunchMetric
--
-- An MXMetric subclass that encapsulates app launch metrics.
--
-- Generated bindings for @MXAppLaunchMetric@.
module ObjC.MetricKit.MXAppLaunchMetric
  ( MXAppLaunchMetric
  , IsMXAppLaunchMetric(..)
  , histogrammedTimeToFirstDraw
  , histogrammedApplicationResumeTime
  , histogrammedOptimizedTimeToFirstDraw
  , histogrammedExtendedLaunch
  , histogrammedApplicationResumeTimeSelector
  , histogrammedExtendedLaunchSelector
  , histogrammedOptimizedTimeToFirstDrawSelector
  , histogrammedTimeToFirstDrawSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | histogrammedTimeToFirstDraw
--
-- Histogrammed application time-to-first-draw data.
--
-- Dimensioned as NSUnitDuration.
--
-- This represents the time when the first CA commit is finished.
--
-- ObjC selector: @- histogrammedTimeToFirstDraw@
histogrammedTimeToFirstDraw :: IsMXAppLaunchMetric mxAppLaunchMetric => mxAppLaunchMetric -> IO (Id MXHistogram)
histogrammedTimeToFirstDraw mxAppLaunchMetric =
  sendMessage mxAppLaunchMetric histogrammedTimeToFirstDrawSelector

-- | histogrammedApplicationResumeTime
--
-- Histogrammed application resume time data.
--
-- Dimensioned as NSUnitDuration.
--
-- ObjC selector: @- histogrammedApplicationResumeTime@
histogrammedApplicationResumeTime :: IsMXAppLaunchMetric mxAppLaunchMetric => mxAppLaunchMetric -> IO (Id MXHistogram)
histogrammedApplicationResumeTime mxAppLaunchMetric =
  sendMessage mxAppLaunchMetric histogrammedApplicationResumeTimeSelector

-- | histogrammedOptimizedTimeToFirstDraw
--
-- Histogrammed optimized application time-to-first-draw data.
--
-- Dimensioned as NSUnitDuration.
--
-- This represents the time when the first CA commit is finished where the application launch has been optimized by the system.
--
-- In iOS 15, the system will opportunistically start applications that are not running in the background to reduce the amount of time a user may have to wait before an application is usable. These launches can occur after a system reboot and periodically as system conditions allow.
--
-- ObjC selector: @- histogrammedOptimizedTimeToFirstDraw@
histogrammedOptimizedTimeToFirstDraw :: IsMXAppLaunchMetric mxAppLaunchMetric => mxAppLaunchMetric -> IO (Id MXHistogram)
histogrammedOptimizedTimeToFirstDraw mxAppLaunchMetric =
  sendMessage mxAppLaunchMetric histogrammedOptimizedTimeToFirstDrawSelector

-- | histogrammedExtendedLaunch
--
-- Histogrammed extended launch data.
--
-- Dimensioned as NSUnitDuration.
--
-- This represents the time when the app has drawn the first frame and finishes all extended launch tasks that assigned by the developer.
--
-- ObjC selector: @- histogrammedExtendedLaunch@
histogrammedExtendedLaunch :: IsMXAppLaunchMetric mxAppLaunchMetric => mxAppLaunchMetric -> IO (Id MXHistogram)
histogrammedExtendedLaunch mxAppLaunchMetric =
  sendMessage mxAppLaunchMetric histogrammedExtendedLaunchSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @histogrammedTimeToFirstDraw@
histogrammedTimeToFirstDrawSelector :: Selector '[] (Id MXHistogram)
histogrammedTimeToFirstDrawSelector = mkSelector "histogrammedTimeToFirstDraw"

-- | @Selector@ for @histogrammedApplicationResumeTime@
histogrammedApplicationResumeTimeSelector :: Selector '[] (Id MXHistogram)
histogrammedApplicationResumeTimeSelector = mkSelector "histogrammedApplicationResumeTime"

-- | @Selector@ for @histogrammedOptimizedTimeToFirstDraw@
histogrammedOptimizedTimeToFirstDrawSelector :: Selector '[] (Id MXHistogram)
histogrammedOptimizedTimeToFirstDrawSelector = mkSelector "histogrammedOptimizedTimeToFirstDraw"

-- | @Selector@ for @histogrammedExtendedLaunch@
histogrammedExtendedLaunchSelector :: Selector '[] (Id MXHistogram)
histogrammedExtendedLaunchSelector = mkSelector "histogrammedExtendedLaunch"

