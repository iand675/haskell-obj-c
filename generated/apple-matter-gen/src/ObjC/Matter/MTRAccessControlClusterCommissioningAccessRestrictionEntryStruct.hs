{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterCommissioningAccessRestrictionEntryStruct@.
module ObjC.Matter.MTRAccessControlClusterCommissioningAccessRestrictionEntryStruct
  ( MTRAccessControlClusterCommissioningAccessRestrictionEntryStruct
  , IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct(..)
  , endpoint
  , setEndpoint
  , cluster
  , setCluster
  , restrictions
  , setRestrictions
  , clusterSelector
  , endpointSelector
  , restrictionsSelector
  , setClusterSelector
  , setEndpointSelector
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
endpoint :: IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> IO (Id NSNumber)
endpoint mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct =
  sendMessage mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct, IsNSNumber value) => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> value -> IO ()
setEndpoint mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct value =
  sendMessage mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct setEndpointSelector (toNSNumber value)

-- | @- cluster@
cluster :: IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> IO (Id NSNumber)
cluster mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct =
  sendMessage mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct clusterSelector

-- | @- setCluster:@
setCluster :: (IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct, IsNSNumber value) => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> value -> IO ()
setCluster mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct value =
  sendMessage mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct setClusterSelector (toNSNumber value)

-- | @- restrictions@
restrictions :: IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> IO (Id NSArray)
restrictions mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct =
  sendMessage mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct restrictionsSelector

-- | @- setRestrictions:@
setRestrictions :: (IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct, IsNSArray value) => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> value -> IO ()
setRestrictions mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct value =
  sendMessage mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct setRestrictionsSelector (toNSArray value)

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

