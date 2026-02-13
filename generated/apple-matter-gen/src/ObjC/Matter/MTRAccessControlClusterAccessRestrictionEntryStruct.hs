{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessRestrictionEntryStruct@.
module ObjC.Matter.MTRAccessControlClusterAccessRestrictionEntryStruct
  ( MTRAccessControlClusterAccessRestrictionEntryStruct
  , IsMTRAccessControlClusterAccessRestrictionEntryStruct(..)
  , endpoint
  , setEndpoint
  , cluster
  , setCluster
  , restrictions
  , setRestrictions
  , fabricIndex
  , setFabricIndex
  , clusterSelector
  , endpointSelector
  , fabricIndexSelector
  , restrictionsSelector
  , setClusterSelector
  , setEndpointSelector
  , setFabricIndexSelector
  , setRestrictionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- endpoint@
endpoint :: IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct => mtrAccessControlClusterAccessRestrictionEntryStruct -> IO (Id NSNumber)
endpoint mtrAccessControlClusterAccessRestrictionEntryStruct =
  sendMessage mtrAccessControlClusterAccessRestrictionEntryStruct endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessRestrictionEntryStruct -> value -> IO ()
setEndpoint mtrAccessControlClusterAccessRestrictionEntryStruct value =
  sendMessage mtrAccessControlClusterAccessRestrictionEntryStruct setEndpointSelector (toNSNumber value)

-- | @- cluster@
cluster :: IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct => mtrAccessControlClusterAccessRestrictionEntryStruct -> IO (Id NSNumber)
cluster mtrAccessControlClusterAccessRestrictionEntryStruct =
  sendMessage mtrAccessControlClusterAccessRestrictionEntryStruct clusterSelector

-- | @- setCluster:@
setCluster :: (IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessRestrictionEntryStruct -> value -> IO ()
setCluster mtrAccessControlClusterAccessRestrictionEntryStruct value =
  sendMessage mtrAccessControlClusterAccessRestrictionEntryStruct setClusterSelector (toNSNumber value)

-- | @- restrictions@
restrictions :: IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct => mtrAccessControlClusterAccessRestrictionEntryStruct -> IO (Id NSArray)
restrictions mtrAccessControlClusterAccessRestrictionEntryStruct =
  sendMessage mtrAccessControlClusterAccessRestrictionEntryStruct restrictionsSelector

-- | @- setRestrictions:@
setRestrictions :: (IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct, IsNSArray value) => mtrAccessControlClusterAccessRestrictionEntryStruct -> value -> IO ()
setRestrictions mtrAccessControlClusterAccessRestrictionEntryStruct value =
  sendMessage mtrAccessControlClusterAccessRestrictionEntryStruct setRestrictionsSelector (toNSArray value)

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct => mtrAccessControlClusterAccessRestrictionEntryStruct -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessRestrictionEntryStruct =
  sendMessage mtrAccessControlClusterAccessRestrictionEntryStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessRestrictionEntryStruct -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessRestrictionEntryStruct value =
  sendMessage mtrAccessControlClusterAccessRestrictionEntryStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector '[Id NSNumber] ()
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @cluster@
clusterSelector :: Selector '[] (Id NSNumber)
clusterSelector = mkSelector "cluster"

-- | @Selector@ for @setCluster:@
setClusterSelector :: Selector '[Id NSNumber] ()
setClusterSelector = mkSelector "setCluster:"

-- | @Selector@ for @restrictions@
restrictionsSelector :: Selector '[] (Id NSArray)
restrictionsSelector = mkSelector "restrictions"

-- | @Selector@ for @setRestrictions:@
setRestrictionsSelector :: Selector '[Id NSArray] ()
setRestrictionsSelector = mkSelector "setRestrictions:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

