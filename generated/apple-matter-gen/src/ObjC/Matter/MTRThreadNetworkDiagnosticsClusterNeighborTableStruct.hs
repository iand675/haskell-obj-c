{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterNeighborTableStruct@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterNeighborTableStruct
  ( MTRThreadNetworkDiagnosticsClusterNeighborTableStruct
  , IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct(..)
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
extAddress :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
extAddress mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct extAddressSelector

-- | @- setExtAddress:@
setExtAddress :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setExtAddress mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setExtAddressSelector (toNSNumber value)

-- | @- age@
age :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
age mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct ageSelector

-- | @- setAge:@
setAge :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setAge mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setAgeSelector (toNSNumber value)

-- | @- rloc16@
rloc16 :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
rloc16 mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct rloc16Selector

-- | @- setRloc16:@
setRloc16 :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setRloc16 mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setRloc16Selector (toNSNumber value)

-- | @- linkFrameCounter@
linkFrameCounter :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
linkFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct linkFrameCounterSelector

-- | @- setLinkFrameCounter:@
setLinkFrameCounter :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setLinkFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setLinkFrameCounterSelector (toNSNumber value)

-- | @- mleFrameCounter@
mleFrameCounter :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
mleFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct mleFrameCounterSelector

-- | @- setMleFrameCounter:@
setMleFrameCounter :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setMleFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setMleFrameCounterSelector (toNSNumber value)

-- | @- lqi@
lqi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
lqi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct lqiSelector

-- | @- setLqi:@
setLqi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setLqi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setLqiSelector (toNSNumber value)

-- | @- averageRssi@
averageRssi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
averageRssi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct averageRssiSelector

-- | @- setAverageRssi:@
setAverageRssi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setAverageRssi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setAverageRssiSelector (toNSNumber value)

-- | @- lastRssi@
lastRssi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
lastRssi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct lastRssiSelector

-- | @- setLastRssi:@
setLastRssi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setLastRssi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setLastRssiSelector (toNSNumber value)

-- | @- frameErrorRate@
frameErrorRate :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
frameErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct frameErrorRateSelector

-- | @- setFrameErrorRate:@
setFrameErrorRate :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setFrameErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setFrameErrorRateSelector (toNSNumber value)

-- | @- messageErrorRate@
messageErrorRate :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
messageErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct messageErrorRateSelector

-- | @- setMessageErrorRate:@
setMessageErrorRate :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setMessageErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setMessageErrorRateSelector (toNSNumber value)

-- | @- rxOnWhenIdle@
rxOnWhenIdle :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
rxOnWhenIdle mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct rxOnWhenIdleSelector

-- | @- setRxOnWhenIdle:@
setRxOnWhenIdle :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setRxOnWhenIdle mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setRxOnWhenIdleSelector (toNSNumber value)

-- | @- fullThreadDevice@
fullThreadDevice :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
fullThreadDevice mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct fullThreadDeviceSelector

-- | @- setFullThreadDevice:@
setFullThreadDevice :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setFullThreadDevice mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setFullThreadDeviceSelector (toNSNumber value)

-- | @- fullNetworkData@
fullNetworkData :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
fullNetworkData mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct fullNetworkDataSelector

-- | @- setFullNetworkData:@
setFullNetworkData :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setFullNetworkData mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setFullNetworkDataSelector (toNSNumber value)

-- | @- isChild@
isChild :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
isChild mtrThreadNetworkDiagnosticsClusterNeighborTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct isChildSelector

-- | @- setIsChild:@
setIsChild :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setIsChild mtrThreadNetworkDiagnosticsClusterNeighborTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterNeighborTableStruct setIsChildSelector (toNSNumber value)

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

