{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterNeighborTable@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterNeighborTable
  ( MTRThreadNetworkDiagnosticsClusterNeighborTable
  , IsMTRThreadNetworkDiagnosticsClusterNeighborTable(..)
  , extAddress
  , setExtAddress
  , age
  , setAge
  , rloc16
  , setRloc16
  , linkFrameCounter
  , setLinkFrameCounter
  , mleFrameCounter
  , setMleFrameCounter
  , lqi
  , setLqi
  , averageRssi
  , setAverageRssi
  , lastRssi
  , setLastRssi
  , frameErrorRate
  , setFrameErrorRate
  , messageErrorRate
  , setMessageErrorRate
  , rxOnWhenIdle
  , setRxOnWhenIdle
  , fullThreadDevice
  , setFullThreadDevice
  , fullNetworkData
  , setFullNetworkData
  , isChild
  , setIsChild
  , ageSelector
  , averageRssiSelector
  , extAddressSelector
  , frameErrorRateSelector
  , fullNetworkDataSelector
  , fullThreadDeviceSelector
  , isChildSelector
  , lastRssiSelector
  , linkFrameCounterSelector
  , lqiSelector
  , messageErrorRateSelector
  , mleFrameCounterSelector
  , rloc16Selector
  , rxOnWhenIdleSelector
  , setAgeSelector
  , setAverageRssiSelector
  , setExtAddressSelector
  , setFrameErrorRateSelector
  , setFullNetworkDataSelector
  , setFullThreadDeviceSelector
  , setIsChildSelector
  , setLastRssiSelector
  , setLinkFrameCounterSelector
  , setLqiSelector
  , setMessageErrorRateSelector
  , setMleFrameCounterSelector
  , setRloc16Selector
  , setRxOnWhenIdleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- extAddress@
extAddress :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
extAddress mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable extAddressSelector

-- | @- setExtAddress:@
setExtAddress :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setExtAddress mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setExtAddressSelector (toNSNumber value)

-- | @- age@
age :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
age mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable ageSelector

-- | @- setAge:@
setAge :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setAge mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setAgeSelector (toNSNumber value)

-- | @- rloc16@
rloc16 :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
rloc16 mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable rloc16Selector

-- | @- setRloc16:@
setRloc16 :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setRloc16 mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setRloc16Selector (toNSNumber value)

-- | @- linkFrameCounter@
linkFrameCounter :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
linkFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable linkFrameCounterSelector

-- | @- setLinkFrameCounter:@
setLinkFrameCounter :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setLinkFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setLinkFrameCounterSelector (toNSNumber value)

-- | @- mleFrameCounter@
mleFrameCounter :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
mleFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable mleFrameCounterSelector

-- | @- setMleFrameCounter:@
setMleFrameCounter :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setMleFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setMleFrameCounterSelector (toNSNumber value)

-- | @- lqi@
lqi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
lqi mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable lqiSelector

-- | @- setLqi:@
setLqi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setLqi mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setLqiSelector (toNSNumber value)

-- | @- averageRssi@
averageRssi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
averageRssi mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable averageRssiSelector

-- | @- setAverageRssi:@
setAverageRssi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setAverageRssi mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setAverageRssiSelector (toNSNumber value)

-- | @- lastRssi@
lastRssi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
lastRssi mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable lastRssiSelector

-- | @- setLastRssi:@
setLastRssi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setLastRssi mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setLastRssiSelector (toNSNumber value)

-- | @- frameErrorRate@
frameErrorRate :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
frameErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable frameErrorRateSelector

-- | @- setFrameErrorRate:@
setFrameErrorRate :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setFrameErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setFrameErrorRateSelector (toNSNumber value)

-- | @- messageErrorRate@
messageErrorRate :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
messageErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable messageErrorRateSelector

-- | @- setMessageErrorRate:@
setMessageErrorRate :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setMessageErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setMessageErrorRateSelector (toNSNumber value)

-- | @- rxOnWhenIdle@
rxOnWhenIdle :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
rxOnWhenIdle mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable rxOnWhenIdleSelector

-- | @- setRxOnWhenIdle:@
setRxOnWhenIdle :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setRxOnWhenIdle mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setRxOnWhenIdleSelector (toNSNumber value)

-- | @- fullThreadDevice@
fullThreadDevice :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
fullThreadDevice mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable fullThreadDeviceSelector

-- | @- setFullThreadDevice:@
setFullThreadDevice :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setFullThreadDevice mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setFullThreadDeviceSelector (toNSNumber value)

-- | @- fullNetworkData@
fullNetworkData :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
fullNetworkData mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable fullNetworkDataSelector

-- | @- setFullNetworkData:@
setFullNetworkData :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setFullNetworkData mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setFullNetworkDataSelector (toNSNumber value)

-- | @- isChild@
isChild :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
isChild mtrThreadNetworkDiagnosticsClusterNeighborTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable isChildSelector

-- | @- setIsChild:@
setIsChild :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setIsChild mtrThreadNetworkDiagnosticsClusterNeighborTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTable setIsChildSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @extAddress@
extAddressSelector :: Selector '[] (Id NSNumber)
extAddressSelector = mkSelector "extAddress"

-- | @Selector@ for @setExtAddress:@
setExtAddressSelector :: Selector '[Id NSNumber] ()
setExtAddressSelector = mkSelector "setExtAddress:"

-- | @Selector@ for @age@
ageSelector :: Selector '[] (Id NSNumber)
ageSelector = mkSelector "age"

-- | @Selector@ for @setAge:@
setAgeSelector :: Selector '[Id NSNumber] ()
setAgeSelector = mkSelector "setAge:"

-- | @Selector@ for @rloc16@
rloc16Selector :: Selector '[] (Id NSNumber)
rloc16Selector = mkSelector "rloc16"

-- | @Selector@ for @setRloc16:@
setRloc16Selector :: Selector '[Id NSNumber] ()
setRloc16Selector = mkSelector "setRloc16:"

-- | @Selector@ for @linkFrameCounter@
linkFrameCounterSelector :: Selector '[] (Id NSNumber)
linkFrameCounterSelector = mkSelector "linkFrameCounter"

-- | @Selector@ for @setLinkFrameCounter:@
setLinkFrameCounterSelector :: Selector '[Id NSNumber] ()
setLinkFrameCounterSelector = mkSelector "setLinkFrameCounter:"

-- | @Selector@ for @mleFrameCounter@
mleFrameCounterSelector :: Selector '[] (Id NSNumber)
mleFrameCounterSelector = mkSelector "mleFrameCounter"

-- | @Selector@ for @setMleFrameCounter:@
setMleFrameCounterSelector :: Selector '[Id NSNumber] ()
setMleFrameCounterSelector = mkSelector "setMleFrameCounter:"

-- | @Selector@ for @lqi@
lqiSelector :: Selector '[] (Id NSNumber)
lqiSelector = mkSelector "lqi"

-- | @Selector@ for @setLqi:@
setLqiSelector :: Selector '[Id NSNumber] ()
setLqiSelector = mkSelector "setLqi:"

-- | @Selector@ for @averageRssi@
averageRssiSelector :: Selector '[] (Id NSNumber)
averageRssiSelector = mkSelector "averageRssi"

-- | @Selector@ for @setAverageRssi:@
setAverageRssiSelector :: Selector '[Id NSNumber] ()
setAverageRssiSelector = mkSelector "setAverageRssi:"

-- | @Selector@ for @lastRssi@
lastRssiSelector :: Selector '[] (Id NSNumber)
lastRssiSelector = mkSelector "lastRssi"

-- | @Selector@ for @setLastRssi:@
setLastRssiSelector :: Selector '[Id NSNumber] ()
setLastRssiSelector = mkSelector "setLastRssi:"

-- | @Selector@ for @frameErrorRate@
frameErrorRateSelector :: Selector '[] (Id NSNumber)
frameErrorRateSelector = mkSelector "frameErrorRate"

-- | @Selector@ for @setFrameErrorRate:@
setFrameErrorRateSelector :: Selector '[Id NSNumber] ()
setFrameErrorRateSelector = mkSelector "setFrameErrorRate:"

-- | @Selector@ for @messageErrorRate@
messageErrorRateSelector :: Selector '[] (Id NSNumber)
messageErrorRateSelector = mkSelector "messageErrorRate"

-- | @Selector@ for @setMessageErrorRate:@
setMessageErrorRateSelector :: Selector '[Id NSNumber] ()
setMessageErrorRateSelector = mkSelector "setMessageErrorRate:"

-- | @Selector@ for @rxOnWhenIdle@
rxOnWhenIdleSelector :: Selector '[] (Id NSNumber)
rxOnWhenIdleSelector = mkSelector "rxOnWhenIdle"

-- | @Selector@ for @setRxOnWhenIdle:@
setRxOnWhenIdleSelector :: Selector '[Id NSNumber] ()
setRxOnWhenIdleSelector = mkSelector "setRxOnWhenIdle:"

-- | @Selector@ for @fullThreadDevice@
fullThreadDeviceSelector :: Selector '[] (Id NSNumber)
fullThreadDeviceSelector = mkSelector "fullThreadDevice"

-- | @Selector@ for @setFullThreadDevice:@
setFullThreadDeviceSelector :: Selector '[Id NSNumber] ()
setFullThreadDeviceSelector = mkSelector "setFullThreadDevice:"

-- | @Selector@ for @fullNetworkData@
fullNetworkDataSelector :: Selector '[] (Id NSNumber)
fullNetworkDataSelector = mkSelector "fullNetworkData"

-- | @Selector@ for @setFullNetworkData:@
setFullNetworkDataSelector :: Selector '[Id NSNumber] ()
setFullNetworkDataSelector = mkSelector "setFullNetworkData:"

-- | @Selector@ for @isChild@
isChildSelector :: Selector '[] (Id NSNumber)
isChildSelector = mkSelector "isChild"

-- | @Selector@ for @setIsChild:@
setIsChildSelector :: Selector '[Id NSNumber] ()
setIsChildSelector = mkSelector "setIsChild:"

