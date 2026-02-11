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

-- | cumulativeLogicalWrites
--
-- Cumulative amount of logical writes.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- cumulativeLogicalWrites@
cumulativeLogicalWrites :: IsMXDiskIOMetric mxDiskIOMetric => mxDiskIOMetric -> IO (Id NSMeasurement)
cumulativeLogicalWrites mxDiskIOMetric  =
  sendMsg mxDiskIOMetric (mkSelector "cumulativeLogicalWrites") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cumulativeLogicalWrites@
cumulativeLogicalWritesSelector :: Selector
cumulativeLogicalWritesSelector = mkSelector "cumulativeLogicalWrites"

