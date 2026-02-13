{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterDayPatternStruct@.
module ObjC.Matter.MTRCommodityTariffClusterDayPatternStruct
  ( MTRCommodityTariffClusterDayPatternStruct
  , IsMTRCommodityTariffClusterDayPatternStruct(..)
  , dayPatternID
  , setDayPatternID
  , daysOfWeek
  , setDaysOfWeek
  , dayEntryIDs
  , setDayEntryIDs
  , dayEntryIDsSelector
  , dayPatternIDSelector
  , daysOfWeekSelector
  , setDayEntryIDsSelector
  , setDayPatternIDSelector
  , setDaysOfWeekSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- dayPatternID@
dayPatternID :: IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct => mtrCommodityTariffClusterDayPatternStruct -> IO (Id NSNumber)
dayPatternID mtrCommodityTariffClusterDayPatternStruct =
  sendMessage mtrCommodityTariffClusterDayPatternStruct dayPatternIDSelector

-- | @- setDayPatternID:@
setDayPatternID :: (IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct, IsNSNumber value) => mtrCommodityTariffClusterDayPatternStruct -> value -> IO ()
setDayPatternID mtrCommodityTariffClusterDayPatternStruct value =
  sendMessage mtrCommodityTariffClusterDayPatternStruct setDayPatternIDSelector (toNSNumber value)

-- | @- daysOfWeek@
daysOfWeek :: IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct => mtrCommodityTariffClusterDayPatternStruct -> IO (Id NSNumber)
daysOfWeek mtrCommodityTariffClusterDayPatternStruct =
  sendMessage mtrCommodityTariffClusterDayPatternStruct daysOfWeekSelector

-- | @- setDaysOfWeek:@
setDaysOfWeek :: (IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct, IsNSNumber value) => mtrCommodityTariffClusterDayPatternStruct -> value -> IO ()
setDaysOfWeek mtrCommodityTariffClusterDayPatternStruct value =
  sendMessage mtrCommodityTariffClusterDayPatternStruct setDaysOfWeekSelector (toNSNumber value)

-- | @- dayEntryIDs@
dayEntryIDs :: IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct => mtrCommodityTariffClusterDayPatternStruct -> IO (Id NSArray)
dayEntryIDs mtrCommodityTariffClusterDayPatternStruct =
  sendMessage mtrCommodityTariffClusterDayPatternStruct dayEntryIDsSelector

-- | @- setDayEntryIDs:@
setDayEntryIDs :: (IsMTRCommodityTariffClusterDayPatternStruct mtrCommodityTariffClusterDayPatternStruct, IsNSArray value) => mtrCommodityTariffClusterDayPatternStruct -> value -> IO ()
setDayEntryIDs mtrCommodityTariffClusterDayPatternStruct value =
  sendMessage mtrCommodityTariffClusterDayPatternStruct setDayEntryIDsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayPatternID@
dayPatternIDSelector :: Selector '[] (Id NSNumber)
dayPatternIDSelector = mkSelector "dayPatternID"

-- | @Selector@ for @setDayPatternID:@
setDayPatternIDSelector :: Selector '[Id NSNumber] ()
setDayPatternIDSelector = mkSelector "setDayPatternID:"

-- | @Selector@ for @daysOfWeek@
daysOfWeekSelector :: Selector '[] (Id NSNumber)
daysOfWeekSelector = mkSelector "daysOfWeek"

-- | @Selector@ for @setDaysOfWeek:@
setDaysOfWeekSelector :: Selector '[Id NSNumber] ()
setDaysOfWeekSelector = mkSelector "setDaysOfWeek:"

-- | @Selector@ for @dayEntryIDs@
dayEntryIDsSelector :: Selector '[] (Id NSArray)
dayEntryIDsSelector = mkSelector "dayEntryIDs"

-- | @Selector@ for @setDayEntryIDs:@
setDayEntryIDsSelector :: Selector '[Id NSArray] ()
setDayEntryIDsSelector = mkSelector "setDayEntryIDs:"

