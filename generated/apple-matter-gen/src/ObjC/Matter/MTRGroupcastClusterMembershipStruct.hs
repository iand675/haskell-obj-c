{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupcastClusterMembershipStruct@.
module ObjC.Matter.MTRGroupcastClusterMembershipStruct
  ( MTRGroupcastClusterMembershipStruct
  , IsMTRGroupcastClusterMembershipStruct(..)
  , groupID
  , setGroupID
  , endpoints
  , setEndpoints
  , keyID
  , setKeyID
  , hasAuxiliaryACL
  , setHasAuxiliaryACL
  , expiringKeyID
  , setExpiringKeyID
  , fabricIndex
  , setFabricIndex
  , endpointsSelector
  , expiringKeyIDSelector
  , fabricIndexSelector
  , groupIDSelector
  , hasAuxiliaryACLSelector
  , keyIDSelector
  , setEndpointsSelector
  , setExpiringKeyIDSelector
  , setFabricIndexSelector
  , setGroupIDSelector
  , setHasAuxiliaryACLSelector
  , setKeyIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- groupID@
groupID :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSNumber)
groupID mtrGroupcastClusterMembershipStruct =
  sendMessage mtrGroupcastClusterMembershipStruct groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSNumber value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setGroupID mtrGroupcastClusterMembershipStruct value =
  sendMessage mtrGroupcastClusterMembershipStruct setGroupIDSelector (toNSNumber value)

-- | @- endpoints@
endpoints :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSArray)
endpoints mtrGroupcastClusterMembershipStruct =
  sendMessage mtrGroupcastClusterMembershipStruct endpointsSelector

-- | @- setEndpoints:@
setEndpoints :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSArray value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setEndpoints mtrGroupcastClusterMembershipStruct value =
  sendMessage mtrGroupcastClusterMembershipStruct setEndpointsSelector (toNSArray value)

-- | @- keyID@
keyID :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSNumber)
keyID mtrGroupcastClusterMembershipStruct =
  sendMessage mtrGroupcastClusterMembershipStruct keyIDSelector

-- | @- setKeyID:@
setKeyID :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSNumber value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setKeyID mtrGroupcastClusterMembershipStruct value =
  sendMessage mtrGroupcastClusterMembershipStruct setKeyIDSelector (toNSNumber value)

-- | @- hasAuxiliaryACL@
hasAuxiliaryACL :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSNumber)
hasAuxiliaryACL mtrGroupcastClusterMembershipStruct =
  sendMessage mtrGroupcastClusterMembershipStruct hasAuxiliaryACLSelector

-- | @- setHasAuxiliaryACL:@
setHasAuxiliaryACL :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSNumber value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setHasAuxiliaryACL mtrGroupcastClusterMembershipStruct value =
  sendMessage mtrGroupcastClusterMembershipStruct setHasAuxiliaryACLSelector (toNSNumber value)

-- | @- expiringKeyID@
expiringKeyID :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSNumber)
expiringKeyID mtrGroupcastClusterMembershipStruct =
  sendMessage mtrGroupcastClusterMembershipStruct expiringKeyIDSelector

-- | @- setExpiringKeyID:@
setExpiringKeyID :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSNumber value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setExpiringKeyID mtrGroupcastClusterMembershipStruct value =
  sendMessage mtrGroupcastClusterMembershipStruct setExpiringKeyIDSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct => mtrGroupcastClusterMembershipStruct -> IO (Id NSNumber)
fabricIndex mtrGroupcastClusterMembershipStruct =
  sendMessage mtrGroupcastClusterMembershipStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRGroupcastClusterMembershipStruct mtrGroupcastClusterMembershipStruct, IsNSNumber value) => mtrGroupcastClusterMembershipStruct -> value -> IO ()
setFabricIndex mtrGroupcastClusterMembershipStruct value =
  sendMessage mtrGroupcastClusterMembershipStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector '[] (Id NSArray)
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector '[Id NSArray] ()
setEndpointsSelector = mkSelector "setEndpoints:"

-- | @Selector@ for @keyID@
keyIDSelector :: Selector '[] (Id NSNumber)
keyIDSelector = mkSelector "keyID"

-- | @Selector@ for @setKeyID:@
setKeyIDSelector :: Selector '[Id NSNumber] ()
setKeyIDSelector = mkSelector "setKeyID:"

-- | @Selector@ for @hasAuxiliaryACL@
hasAuxiliaryACLSelector :: Selector '[] (Id NSNumber)
hasAuxiliaryACLSelector = mkSelector "hasAuxiliaryACL"

-- | @Selector@ for @setHasAuxiliaryACL:@
setHasAuxiliaryACLSelector :: Selector '[Id NSNumber] ()
setHasAuxiliaryACLSelector = mkSelector "setHasAuxiliaryACL:"

-- | @Selector@ for @expiringKeyID@
expiringKeyIDSelector :: Selector '[] (Id NSNumber)
expiringKeyIDSelector = mkSelector "expiringKeyID"

-- | @Selector@ for @setExpiringKeyID:@
setExpiringKeyIDSelector :: Selector '[Id NSNumber] ()
setExpiringKeyIDSelector = mkSelector "setExpiringKeyID:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

