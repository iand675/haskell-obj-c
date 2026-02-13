{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterRouteTable@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterRouteTable
  ( MTRThreadNetworkDiagnosticsClusterRouteTable
  , IsMTRThreadNetworkDiagnosticsClusterRouteTable(..)
  , extAddress
  , setExtAddress
  , rloc16
  , setRloc16
  , routerId
  , setRouterId
  , nextHop
  , setNextHop
  , pathCost
  , setPathCost
  , lqiIn
  , setLqiIn
  , lqiOut
  , setLqiOut
  , age
  , setAge
  , allocated
  , setAllocated
  , linkEstablished
  , setLinkEstablished
  , ageSelector
  , allocatedSelector
  , extAddressSelector
  , linkEstablishedSelector
  , lqiInSelector
  , lqiOutSelector
  , nextHopSelector
  , pathCostSelector
  , rloc16Selector
  , routerIdSelector
  , setAgeSelector
  , setAllocatedSelector
  , setExtAddressSelector
  , setLinkEstablishedSelector
  , setLqiInSelector
  , setLqiOutSelector
  , setNextHopSelector
  , setPathCostSelector
  , setRloc16Selector
  , setRouterIdSelector


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
extAddress :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
extAddress mtrThreadNetworkDiagnosticsClusterRouteTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable extAddressSelector

-- | @- setExtAddress:@
setExtAddress :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setExtAddress mtrThreadNetworkDiagnosticsClusterRouteTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable setExtAddressSelector (toNSNumber value)

-- | @- rloc16@
rloc16 :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
rloc16 mtrThreadNetworkDiagnosticsClusterRouteTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable rloc16Selector

-- | @- setRloc16:@
setRloc16 :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setRloc16 mtrThreadNetworkDiagnosticsClusterRouteTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable setRloc16Selector (toNSNumber value)

-- | @- routerId@
routerId :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
routerId mtrThreadNetworkDiagnosticsClusterRouteTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable routerIdSelector

-- | @- setRouterId:@
setRouterId :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setRouterId mtrThreadNetworkDiagnosticsClusterRouteTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable setRouterIdSelector (toNSNumber value)

-- | @- nextHop@
nextHop :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
nextHop mtrThreadNetworkDiagnosticsClusterRouteTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable nextHopSelector

-- | @- setNextHop:@
setNextHop :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setNextHop mtrThreadNetworkDiagnosticsClusterRouteTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable setNextHopSelector (toNSNumber value)

-- | @- pathCost@
pathCost :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
pathCost mtrThreadNetworkDiagnosticsClusterRouteTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable pathCostSelector

-- | @- setPathCost:@
setPathCost :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setPathCost mtrThreadNetworkDiagnosticsClusterRouteTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable setPathCostSelector (toNSNumber value)

-- | @- lqiIn@
lqiIn :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
lqiIn mtrThreadNetworkDiagnosticsClusterRouteTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable lqiInSelector

-- | @- setLqiIn:@
setLqiIn :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setLqiIn mtrThreadNetworkDiagnosticsClusterRouteTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable setLqiInSelector (toNSNumber value)

-- | @- lqiOut@
lqiOut :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
lqiOut mtrThreadNetworkDiagnosticsClusterRouteTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable lqiOutSelector

-- | @- setLqiOut:@
setLqiOut :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setLqiOut mtrThreadNetworkDiagnosticsClusterRouteTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable setLqiOutSelector (toNSNumber value)

-- | @- age@
age :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
age mtrThreadNetworkDiagnosticsClusterRouteTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable ageSelector

-- | @- setAge:@
setAge :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setAge mtrThreadNetworkDiagnosticsClusterRouteTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable setAgeSelector (toNSNumber value)

-- | @- allocated@
allocated :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
allocated mtrThreadNetworkDiagnosticsClusterRouteTable =
  sendOwnedMessage mtrThreadNetworkDiagnosticsClusterRouteTable allocatedSelector

-- | @- setAllocated:@
setAllocated :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setAllocated mtrThreadNetworkDiagnosticsClusterRouteTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable setAllocatedSelector (toNSNumber value)

-- | @- linkEstablished@
linkEstablished :: IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable => mtrThreadNetworkDiagnosticsClusterRouteTable -> IO (Id NSNumber)
linkEstablished mtrThreadNetworkDiagnosticsClusterRouteTable =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable linkEstablishedSelector

-- | @- setLinkEstablished:@
setLinkEstablished :: (IsMTRThreadNetworkDiagnosticsClusterRouteTable mtrThreadNetworkDiagnosticsClusterRouteTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTable -> value -> IO ()
setLinkEstablished mtrThreadNetworkDiagnosticsClusterRouteTable value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTable setLinkEstablishedSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @extAddress@
extAddressSelector :: Selector '[] (Id NSNumber)
extAddressSelector = mkSelector "extAddress"

-- | @Selector@ for @setExtAddress:@
setExtAddressSelector :: Selector '[Id NSNumber] ()
setExtAddressSelector = mkSelector "setExtAddress:"

-- | @Selector@ for @rloc16@
rloc16Selector :: Selector '[] (Id NSNumber)
rloc16Selector = mkSelector "rloc16"

-- | @Selector@ for @setRloc16:@
setRloc16Selector :: Selector '[Id NSNumber] ()
setRloc16Selector = mkSelector "setRloc16:"

-- | @Selector@ for @routerId@
routerIdSelector :: Selector '[] (Id NSNumber)
routerIdSelector = mkSelector "routerId"

-- | @Selector@ for @setRouterId:@
setRouterIdSelector :: Selector '[Id NSNumber] ()
setRouterIdSelector = mkSelector "setRouterId:"

-- | @Selector@ for @nextHop@
nextHopSelector :: Selector '[] (Id NSNumber)
nextHopSelector = mkSelector "nextHop"

-- | @Selector@ for @setNextHop:@
setNextHopSelector :: Selector '[Id NSNumber] ()
setNextHopSelector = mkSelector "setNextHop:"

-- | @Selector@ for @pathCost@
pathCostSelector :: Selector '[] (Id NSNumber)
pathCostSelector = mkSelector "pathCost"

-- | @Selector@ for @setPathCost:@
setPathCostSelector :: Selector '[Id NSNumber] ()
setPathCostSelector = mkSelector "setPathCost:"

-- | @Selector@ for @lqiIn@
lqiInSelector :: Selector '[] (Id NSNumber)
lqiInSelector = mkSelector "lqiIn"

-- | @Selector@ for @setLqiIn:@
setLqiInSelector :: Selector '[Id NSNumber] ()
setLqiInSelector = mkSelector "setLqiIn:"

-- | @Selector@ for @lqiOut@
lqiOutSelector :: Selector '[] (Id NSNumber)
lqiOutSelector = mkSelector "lqiOut"

-- | @Selector@ for @setLqiOut:@
setLqiOutSelector :: Selector '[Id NSNumber] ()
setLqiOutSelector = mkSelector "setLqiOut:"

-- | @Selector@ for @age@
ageSelector :: Selector '[] (Id NSNumber)
ageSelector = mkSelector "age"

-- | @Selector@ for @setAge:@
setAgeSelector :: Selector '[Id NSNumber] ()
setAgeSelector = mkSelector "setAge:"

-- | @Selector@ for @allocated@
allocatedSelector :: Selector '[] (Id NSNumber)
allocatedSelector = mkSelector "allocated"

-- | @Selector@ for @setAllocated:@
setAllocatedSelector :: Selector '[Id NSNumber] ()
setAllocatedSelector = mkSelector "setAllocated:"

-- | @Selector@ for @linkEstablished@
linkEstablishedSelector :: Selector '[] (Id NSNumber)
linkEstablishedSelector = mkSelector "linkEstablished"

-- | @Selector@ for @setLinkEstablished:@
setLinkEstablishedSelector :: Selector '[Id NSNumber] ()
setLinkEstablishedSelector = mkSelector "setLinkEstablished:"

