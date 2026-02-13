{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXDiskIOMetric
--
-- An MXMetric subclass that encapsulates disk IO metrics.
--
-- Generated bindings for @MXDiskIOMetric@.
module ObjC.MetricKit.MXDiskIOMetric
  ( MXDiskIOMetric
  , IsMXDiskIOMetric(..)
  , cumulativeLogicalWrites
  , cumulativeLogicalWritesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | cumulativeLogicalWrites
--
-- Cumulative amount of logical writes.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- cumulativeLogicalWrites@
cumulativeLogicalWrites :: IsMXDiskIOMetric mxDiskIOMetric => mxDiskIOMetric -> IO (Id NSMeasurement)
cumulativeLogicalWrites mxDiskIOMetric =
  sendMessage mxDiskIOMetric cumulativeLogicalWritesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeLogicalWrites@
cumulativeLogicalWritesSelector :: Selector '[] (Id NSMeasurement)
cumulativeLogicalWritesSelector = mkSelector "cumulativeLogicalWrites"

