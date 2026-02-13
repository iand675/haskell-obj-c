{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterDayEntryStruct@.
module ObjC.Matter.MTRCommodityTariffClusterDayEntryStruct
  ( MTRCommodityTariffClusterDayEntryStruct
  , IsMTRCommodityTariffClusterDayEntryStruct(..)
  , dayEntryID
  , setDayEntryID
  , startTime
  , setStartTime
  , duration
  , setDuration
  , randomizationOffset
  , setRandomizationOffset
  , randomizationType
  , setRandomizationType
  , dayEntryIDSelector
  , durationSelector
  , randomizationOffsetSelector
  , randomizationTypeSelector
  , setDayEntryIDSelector
  , setDurationSelector
  , setRandomizationOffsetSelector
  , setRandomizationTypeSelector
  , setStartTimeSelector
  , startTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- dayEntryID@
dayEntryID :: IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct => mtrCommodityTariffClusterDayEntryStruct -> IO (Id NSNumber)
dayEntryID mtrCommodityTariffClusterDayEntryStruct =
  sendMessage mtrCommodityTariffClusterDayEntryStruct dayEntryIDSelector

-- | @- setDayEntryID:@
setDayEntryID :: (IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct, IsNSNumber value) => mtrCommodityTariffClusterDayEntryStruct -> value -> IO ()
setDayEntryID mtrCommodityTariffClusterDayEntryStruct value =
  sendMessage mtrCommodityTariffClusterDayEntryStruct setDayEntryIDSelector (toNSNumber value)

-- | @- startTime@
startTime :: IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct => mtrCommodityTariffClusterDayEntryStruct -> IO (Id NSNumber)
startTime mtrCommodityTariffClusterDayEntryStruct =
  sendMessage mtrCommodityTariffClusterDayEntryStruct startTimeSelector

-- | @- setStartTime:@
setStartTime :: (IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct, IsNSNumber value) => mtrCommodityTariffClusterDayEntryStruct -> value -> IO ()
setStartTime mtrCommodityTariffClusterDayEntryStruct value =
  sendMessage mtrCommodityTariffClusterDayEntryStruct setStartTimeSelector (toNSNumber value)

-- | @- duration@
duration :: IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct => mtrCommodityTariffClusterDayEntryStruct -> IO (Id NSNumber)
duration mtrCommodityTariffClusterDayEntryStruct =
  sendMessage mtrCommodityTariffClusterDayEntryStruct durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct, IsNSNumber value) => mtrCommodityTariffClusterDayEntryStruct -> value -> IO ()
setDuration mtrCommodityTariffClusterDayEntryStruct value =
  sendMessage mtrCommodityTariffClusterDayEntryStruct setDurationSelector (toNSNumber value)

-- | @- randomizationOffset@
randomizationOffset :: IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct => mtrCommodityTariffClusterDayEntryStruct -> IO (Id NSNumber)
randomizationOffset mtrCommodityTariffClusterDayEntryStruct =
  sendMessage mtrCommodityTariffClusterDayEntryStruct randomizationOffsetSelector

-- | @- setRandomizationOffset:@
setRandomizationOffset :: (IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct, IsNSNumber value) => mtrCommodityTariffClusterDayEntryStruct -> value -> IO ()
setRandomizationOffset mtrCommodityTariffClusterDayEntryStruct value =
  sendMessage mtrCommodityTariffClusterDayEntryStruct setRandomizationOffsetSelector (toNSNumber value)

-- | @- randomizationType@
randomizationType :: IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct => mtrCommodityTariffClusterDayEntryStruct -> IO (Id NSNumber)
randomizationType mtrCommodityTariffClusterDayEntryStruct =
  sendMessage mtrCommodityTariffClusterDayEntryStruct randomizationTypeSelector

-- | @- setRandomizationType:@
setRandomizationType :: (IsMTRCommodityTariffClusterDayEntryStruct mtrCommodityTariffClusterDayEntryStruct, IsNSNumber value) => mtrCommodityTariffClusterDayEntryStruct -> value -> IO ()
setRandomizationType mtrCommodityTariffClusterDayEntryStruct value =
  sendMessage mtrCommodityTariffClusterDayEntryStruct setRandomizationTypeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayEntryID@
dayEntryIDSelector :: Selector '[] (Id NSNumber)
dayEntryIDSelector = mkSelector "dayEntryID"

-- | @Selector@ for @setDayEntryID:@
setDayEntryIDSelector :: Selector '[Id NSNumber] ()
setDayEntryIDSelector = mkSelector "setDayEntryID:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] (Id NSNumber)
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector '[Id NSNumber] ()
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[Id NSNumber] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @randomizationOffset@
randomizationOffsetSelector :: Selector '[] (Id NSNumber)
randomizationOffsetSelector = mkSelector "randomizationOffset"

-- | @Selector@ for @setRandomizationOffset:@
setRandomizationOffsetSelector :: Selector '[Id NSNumber] ()
setRandomizationOffsetSelector = mkSelector "setRandomizationOffset:"

-- | @Selector@ for @randomizationType@
randomizationTypeSelector :: Selector '[] (Id NSNumber)
randomizationTypeSelector = mkSelector "randomizationType"

-- | @Selector@ for @setRandomizationType:@
setRandomizationTypeSelector :: Selector '[Id NSNumber] ()
setRandomizationTypeSelector = mkSelector "setRandomizationType:"

