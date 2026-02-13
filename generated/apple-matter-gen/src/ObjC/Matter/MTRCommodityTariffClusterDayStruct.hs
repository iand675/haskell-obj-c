{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterDayStruct@.
module ObjC.Matter.MTRCommodityTariffClusterDayStruct
  ( MTRCommodityTariffClusterDayStruct
  , IsMTRCommodityTariffClusterDayStruct(..)
  , date
  , setDate
  , dayType
  , setDayType
  , dayEntryIDs
  , setDayEntryIDs
  , dateSelector
  , dayEntryIDsSelector
  , dayTypeSelector
  , setDateSelector
  , setDayEntryIDsSelector
  , setDayTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- date@
date :: IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct => mtrCommodityTariffClusterDayStruct -> IO (Id NSNumber)
date mtrCommodityTariffClusterDayStruct =
  sendMessage mtrCommodityTariffClusterDayStruct dateSelector

-- | @- setDate:@
setDate :: (IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct, IsNSNumber value) => mtrCommodityTariffClusterDayStruct -> value -> IO ()
setDate mtrCommodityTariffClusterDayStruct value =
  sendMessage mtrCommodityTariffClusterDayStruct setDateSelector (toNSNumber value)

-- | @- dayType@
dayType :: IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct => mtrCommodityTariffClusterDayStruct -> IO (Id NSNumber)
dayType mtrCommodityTariffClusterDayStruct =
  sendMessage mtrCommodityTariffClusterDayStruct dayTypeSelector

-- | @- setDayType:@
setDayType :: (IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct, IsNSNumber value) => mtrCommodityTariffClusterDayStruct -> value -> IO ()
setDayType mtrCommodityTariffClusterDayStruct value =
  sendMessage mtrCommodityTariffClusterDayStruct setDayTypeSelector (toNSNumber value)

-- | @- dayEntryIDs@
dayEntryIDs :: IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct => mtrCommodityTariffClusterDayStruct -> IO (Id NSArray)
dayEntryIDs mtrCommodityTariffClusterDayStruct =
  sendMessage mtrCommodityTariffClusterDayStruct dayEntryIDsSelector

-- | @- setDayEntryIDs:@
setDayEntryIDs :: (IsMTRCommodityTariffClusterDayStruct mtrCommodityTariffClusterDayStruct, IsNSArray value) => mtrCommodityTariffClusterDayStruct -> value -> IO ()
setDayEntryIDs mtrCommodityTariffClusterDayStruct value =
  sendMessage mtrCommodityTariffClusterDayStruct setDayEntryIDsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSNumber)
dateSelector = mkSelector "date"

-- | @Selector@ for @setDate:@
setDateSelector :: Selector '[Id NSNumber] ()
setDateSelector = mkSelector "setDate:"

-- | @Selector@ for @dayType@
dayTypeSelector :: Selector '[] (Id NSNumber)
dayTypeSelector = mkSelector "dayType"

-- | @Selector@ for @setDayType:@
setDayTypeSelector :: Selector '[Id NSNumber] ()
setDayTypeSelector = mkSelector "setDayType:"

-- | @Selector@ for @dayEntryIDs@
dayEntryIDsSelector :: Selector '[] (Id NSArray)
dayEntryIDsSelector = mkSelector "dayEntryIDs"

-- | @Selector@ for @setDayEntryIDs:@
setDayEntryIDsSelector :: Selector '[Id NSArray] ()
setDayEntryIDsSelector = mkSelector "setDayEntryIDs:"

