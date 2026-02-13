{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterTariffPeriodStruct@.
module ObjC.Matter.MTRCommodityTariffClusterTariffPeriodStruct
  ( MTRCommodityTariffClusterTariffPeriodStruct
  , IsMTRCommodityTariffClusterTariffPeriodStruct(..)
  , label
  , setLabel
  , dayEntryIDs
  , setDayEntryIDs
  , tariffComponentIDs
  , setTariffComponentIDs
  , dayEntryIDsSelector
  , labelSelector
  , setDayEntryIDsSelector
  , setLabelSelector
  , setTariffComponentIDsSelector
  , tariffComponentIDsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- label@
label :: IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct => mtrCommodityTariffClusterTariffPeriodStruct -> IO (Id NSString)
label mtrCommodityTariffClusterTariffPeriodStruct =
  sendMessage mtrCommodityTariffClusterTariffPeriodStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct, IsNSString value) => mtrCommodityTariffClusterTariffPeriodStruct -> value -> IO ()
setLabel mtrCommodityTariffClusterTariffPeriodStruct value =
  sendMessage mtrCommodityTariffClusterTariffPeriodStruct setLabelSelector (toNSString value)

-- | @- dayEntryIDs@
dayEntryIDs :: IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct => mtrCommodityTariffClusterTariffPeriodStruct -> IO (Id NSArray)
dayEntryIDs mtrCommodityTariffClusterTariffPeriodStruct =
  sendMessage mtrCommodityTariffClusterTariffPeriodStruct dayEntryIDsSelector

-- | @- setDayEntryIDs:@
setDayEntryIDs :: (IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct, IsNSArray value) => mtrCommodityTariffClusterTariffPeriodStruct -> value -> IO ()
setDayEntryIDs mtrCommodityTariffClusterTariffPeriodStruct value =
  sendMessage mtrCommodityTariffClusterTariffPeriodStruct setDayEntryIDsSelector (toNSArray value)

-- | @- tariffComponentIDs@
tariffComponentIDs :: IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct => mtrCommodityTariffClusterTariffPeriodStruct -> IO (Id NSArray)
tariffComponentIDs mtrCommodityTariffClusterTariffPeriodStruct =
  sendMessage mtrCommodityTariffClusterTariffPeriodStruct tariffComponentIDsSelector

-- | @- setTariffComponentIDs:@
setTariffComponentIDs :: (IsMTRCommodityTariffClusterTariffPeriodStruct mtrCommodityTariffClusterTariffPeriodStruct, IsNSArray value) => mtrCommodityTariffClusterTariffPeriodStruct -> value -> IO ()
setTariffComponentIDs mtrCommodityTariffClusterTariffPeriodStruct value =
  sendMessage mtrCommodityTariffClusterTariffPeriodStruct setTariffComponentIDsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @dayEntryIDs@
dayEntryIDsSelector :: Selector '[] (Id NSArray)
dayEntryIDsSelector = mkSelector "dayEntryIDs"

-- | @Selector@ for @setDayEntryIDs:@
setDayEntryIDsSelector :: Selector '[Id NSArray] ()
setDayEntryIDsSelector = mkSelector "setDayEntryIDs:"

-- | @Selector@ for @tariffComponentIDs@
tariffComponentIDsSelector :: Selector '[] (Id NSArray)
tariffComponentIDsSelector = mkSelector "tariffComponentIDs"

-- | @Selector@ for @setTariffComponentIDs:@
setTariffComponentIDsSelector :: Selector '[Id NSArray] ()
setTariffComponentIDsSelector = mkSelector "setTariffComponentIDs:"

