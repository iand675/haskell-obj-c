{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterRouteTableStruct@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterRouteTableStruct
  ( MTRThreadNetworkDiagnosticsClusterRouteTableStruct
  , IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct(..)
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
extAddress :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
extAddress mtrThreadNetworkDiagnosticsClusterRouteTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct extAddressSelector

-- | @- setExtAddress:@
setExtAddress :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setExtAddress mtrThreadNetworkDiagnosticsClusterRouteTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct setExtAddressSelector (toNSNumber value)

-- | @- rloc16@
rloc16 :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
rloc16 mtrThreadNetworkDiagnosticsClusterRouteTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct rloc16Selector

-- | @- setRloc16:@
setRloc16 :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setRloc16 mtrThreadNetworkDiagnosticsClusterRouteTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct setRloc16Selector (toNSNumber value)

-- | @- routerId@
routerId :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
routerId mtrThreadNetworkDiagnosticsClusterRouteTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct routerIdSelector

-- | @- setRouterId:@
setRouterId :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setRouterId mtrThreadNetworkDiagnosticsClusterRouteTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct setRouterIdSelector (toNSNumber value)

-- | @- nextHop@
nextHop :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
nextHop mtrThreadNetworkDiagnosticsClusterRouteTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct nextHopSelector

-- | @- setNextHop:@
setNextHop :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setNextHop mtrThreadNetworkDiagnosticsClusterRouteTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct setNextHopSelector (toNSNumber value)

-- | @- pathCost@
pathCost :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
pathCost mtrThreadNetworkDiagnosticsClusterRouteTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct pathCostSelector

-- | @- setPathCost:@
setPathCost :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setPathCost mtrThreadNetworkDiagnosticsClusterRouteTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct setPathCostSelector (toNSNumber value)

-- | @- lqiIn@
lqiIn :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
lqiIn mtrThreadNetworkDiagnosticsClusterRouteTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct lqiInSelector

-- | @- setLqiIn:@
setLqiIn :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setLqiIn mtrThreadNetworkDiagnosticsClusterRouteTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct setLqiInSelector (toNSNumber value)

-- | @- lqiOut@
lqiOut :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
lqiOut mtrThreadNetworkDiagnosticsClusterRouteTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct lqiOutSelector

-- | @- setLqiOut:@
setLqiOut :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setLqiOut mtrThreadNetworkDiagnosticsClusterRouteTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct setLqiOutSelector (toNSNumber value)

-- | @- age@
age :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
age mtrThreadNetworkDiagnosticsClusterRouteTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct ageSelector

-- | @- setAge:@
setAge :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setAge mtrThreadNetworkDiagnosticsClusterRouteTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct setAgeSelector (toNSNumber value)

-- | @- allocated@
allocated :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
allocated mtrThreadNetworkDiagnosticsClusterRouteTableStruct =
  sendOwnedMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct allocatedSelector

-- | @- setAllocated:@
setAllocated :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setAllocated mtrThreadNetworkDiagnosticsClusterRouteTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct setAllocatedSelector (toNSNumber value)

-- | @- linkEstablished@
linkEstablished :: IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> IO (Id NSNumber)
linkEstablished mtrThreadNetworkDiagnosticsClusterRouteTableStruct =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct linkEstablishedSelector

-- | @- setLinkEstablished:@
setLinkEstablished :: (IsMTRThreadNetworkDiagnosticsClusterRouteTableStruct mtrThreadNetworkDiagnosticsClusterRouteTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterRouteTableStruct -> value -> IO ()
setLinkEstablished mtrThreadNetworkDiagnosticsClusterRouteTableStruct value =
  sendMessage mtrThreadNetworkDiagnosticsClusterRouteTableStruct setLinkEstablishedSelector (toNSNumber value)

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

