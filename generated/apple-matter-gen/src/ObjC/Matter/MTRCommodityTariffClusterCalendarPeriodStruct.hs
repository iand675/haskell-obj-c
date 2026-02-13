{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterCalendarPeriodStruct@.
module ObjC.Matter.MTRCommodityTariffClusterCalendarPeriodStruct
  ( MTRCommodityTariffClusterCalendarPeriodStruct
  , IsMTRCommodityTariffClusterCalendarPeriodStruct(..)
  , startDate
  , setStartDate
  , dayPatternIDs
  , setDayPatternIDs
  , dayPatternIDsSelector
  , setDayPatternIDsSelector
  , setStartDateSelector
  , startDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- startDate@
startDate :: IsMTRCommodityTariffClusterCalendarPeriodStruct mtrCommodityTariffClusterCalendarPeriodStruct => mtrCommodityTariffClusterCalendarPeriodStruct -> IO (Id NSNumber)
startDate mtrCommodityTariffClusterCalendarPeriodStruct =
  sendMessage mtrCommodityTariffClusterCalendarPeriodStruct startDateSelector

-- | @- setStartDate:@
setStartDate :: (IsMTRCommodityTariffClusterCalendarPeriodStruct mtrCommodityTariffClusterCalendarPeriodStruct, IsNSNumber value) => mtrCommodityTariffClusterCalendarPeriodStruct -> value -> IO ()
setStartDate mtrCommodityTariffClusterCalendarPeriodStruct value =
  sendMessage mtrCommodityTariffClusterCalendarPeriodStruct setStartDateSelector (toNSNumber value)

-- | @- dayPatternIDs@
dayPatternIDs :: IsMTRCommodityTariffClusterCalendarPeriodStruct mtrCommodityTariffClusterCalendarPeriodStruct => mtrCommodityTariffClusterCalendarPeriodStruct -> IO (Id NSArray)
dayPatternIDs mtrCommodityTariffClusterCalendarPeriodStruct =
  sendMessage mtrCommodityTariffClusterCalendarPeriodStruct dayPatternIDsSelector

-- | @- setDayPatternIDs:@
setDayPatternIDs :: (IsMTRCommodityTariffClusterCalendarPeriodStruct mtrCommodityTariffClusterCalendarPeriodStruct, IsNSArray value) => mtrCommodityTariffClusterCalendarPeriodStruct -> value -> IO ()
setDayPatternIDs mtrCommodityTariffClusterCalendarPeriodStruct value =
  sendMessage mtrCommodityTariffClusterCalendarPeriodStruct setDayPatternIDsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSNumber)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector '[Id NSNumber] ()
setStartDateSelector = mkSelector "setStartDate:"

-- | @Selector@ for @dayPatternIDs@
dayPatternIDsSelector :: Selector '[] (Id NSArray)
dayPatternIDsSelector = mkSelector "dayPatternIDs"

-- | @Selector@ for @setDayPatternIDs:@
setDayPatternIDsSelector :: Selector '[Id NSArray] ()
setDayPatternIDsSelector = mkSelector "setDayPatternIDs:"

