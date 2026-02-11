{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXMemoryMetric
--
-- An MXMetric subclass that encapsulates memory metrics.
--
-- Generated bindings for @MXMemoryMetric@.
module ObjC.MetricKit.MXMemoryMetric
  ( MXMemoryMetric
  , IsMXMemoryMetric(..)
  , peakMemoryUsage
  , averageSuspendedMemory
  , peakMemoryUsageSelector
  , averageSuspendedMemorySelector


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

-- | peakMemoryUsage
--
-- A single value representing the peak memory consumption of the application.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- peakMemoryUsage@
peakMemoryUsage :: IsMXMemoryMetric mxMemoryMetric => mxMemoryMetric -> IO (Id NSMeasurement)
peakMemoryUsage mxMemoryMetric  =
  sendMsg mxMemoryMetric (mkSelector "peakMemoryUsage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | averageSuspendedMemory
--
-- Average memory of the application upon suspend.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- averageSuspendedMemory@
averageSuspendedMemory :: IsMXMemoryMetric mxMemoryMetric => mxMemoryMetric -> IO (Id MXAverage)
averageSuspendedMemory mxMemoryMetric  =
  sendMsg mxMemoryMetric (mkSelector "averageSuspendedMemory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @peakMemoryUsage@
peakMemoryUsageSelector :: Selector
peakMemoryUsageSelector = mkSelector "peakMemoryUsage"

-- | @Selector@ for @averageSuspendedMemory@
averageSuspendedMemorySelector :: Selector
averageSuspendedMemorySelector = mkSelector "averageSuspendedMemory"

