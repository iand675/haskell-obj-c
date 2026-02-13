{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterPeakPeriodStruct@.
module ObjC.Matter.MTRCommodityTariffClusterPeakPeriodStruct
  ( MTRCommodityTariffClusterPeakPeriodStruct
  , IsMTRCommodityTariffClusterPeakPeriodStruct(..)
  , severity
  , setSeverity
  , peakPeriod
  , setPeakPeriod
  , peakPeriodSelector
  , setPeakPeriodSelector
  , setSeveritySelector
  , severitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- severity@
severity :: IsMTRCommodityTariffClusterPeakPeriodStruct mtrCommodityTariffClusterPeakPeriodStruct => mtrCommodityTariffClusterPeakPeriodStruct -> IO (Id NSNumber)
severity mtrCommodityTariffClusterPeakPeriodStruct =
  sendMessage mtrCommodityTariffClusterPeakPeriodStruct severitySelector

-- | @- setSeverity:@
setSeverity :: (IsMTRCommodityTariffClusterPeakPeriodStruct mtrCommodityTariffClusterPeakPeriodStruct, IsNSNumber value) => mtrCommodityTariffClusterPeakPeriodStruct -> value -> IO ()
setSeverity mtrCommodityTariffClusterPeakPeriodStruct value =
  sendMessage mtrCommodityTariffClusterPeakPeriodStruct setSeveritySelector (toNSNumber value)

-- | @- peakPeriod@
peakPeriod :: IsMTRCommodityTariffClusterPeakPeriodStruct mtrCommodityTariffClusterPeakPeriodStruct => mtrCommodityTariffClusterPeakPeriodStruct -> IO (Id NSNumber)
peakPeriod mtrCommodityTariffClusterPeakPeriodStruct =
  sendMessage mtrCommodityTariffClusterPeakPeriodStruct peakPeriodSelector

-- | @- setPeakPeriod:@
setPeakPeriod :: (IsMTRCommodityTariffClusterPeakPeriodStruct mtrCommodityTariffClusterPeakPeriodStruct, IsNSNumber value) => mtrCommodityTariffClusterPeakPeriodStruct -> value -> IO ()
setPeakPeriod mtrCommodityTariffClusterPeakPeriodStruct value =
  sendMessage mtrCommodityTariffClusterPeakPeriodStruct setPeakPeriodSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @severity@
severitySelector :: Selector '[] (Id NSNumber)
severitySelector = mkSelector "severity"

-- | @Selector@ for @setSeverity:@
setSeveritySelector :: Selector '[Id NSNumber] ()
setSeveritySelector = mkSelector "setSeverity:"

-- | @Selector@ for @peakPeriod@
peakPeriodSelector :: Selector '[] (Id NSNumber)
peakPeriodSelector = mkSelector "peakPeriod"

-- | @Selector@ for @setPeakPeriod:@
setPeakPeriodSelector :: Selector '[Id NSNumber] ()
setPeakPeriodSelector = mkSelector "setPeakPeriod:"

