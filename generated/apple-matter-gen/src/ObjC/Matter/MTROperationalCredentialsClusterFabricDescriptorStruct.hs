{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterFabricDescriptorStruct@.
module ObjC.Matter.MTROperationalCredentialsClusterFabricDescriptorStruct
  ( MTROperationalCredentialsClusterFabricDescriptorStruct
  , IsMTROperationalCredentialsClusterFabricDescriptorStruct(..)
  , rootPublicKey
  , setRootPublicKey
  , vendorID
  , setVendorID
  , vendorId
  , setVendorId
  , fabricID
  , setFabricID
  , fabricId
  , setFabricId
  , nodeID
  , setNodeID
  , nodeId
  , setNodeId
  , label
  , setLabel
  , vidVerificationStatement
  , setVidVerificationStatement
  , fabricIndex
  , setFabricIndex
  , fabricIDSelector
  , fabricIdSelector
  , fabricIndexSelector
  , labelSelector
  , nodeIDSelector
  , nodeIdSelector
  , rootPublicKeySelector
  , setFabricIDSelector
  , setFabricIdSelector
  , setFabricIndexSelector
  , setLabelSelector
  , setNodeIDSelector
  , setNodeIdSelector
  , setRootPublicKeySelector
  , setVendorIDSelector
  , setVendorIdSelector
  , setVidVerificationStatementSelector
  , vendorIDSelector
  , vendorIdSelector
  , vidVerificationStatementSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rootPublicKey@
rootPublicKey :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSData)
rootPublicKey mtrOperationalCredentialsClusterFabricDescriptorStruct =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct rootPublicKeySelector

-- | @- setRootPublicKey:@
setRootPublicKey :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSData value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setRootPublicKey mtrOperationalCredentialsClusterFabricDescriptorStruct value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct setRootPublicKeySelector (toNSData value)

-- | @- vendorID@
vendorID :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
vendorID mtrOperationalCredentialsClusterFabricDescriptorStruct =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct vendorIDSelector

-- | @- setVendorID:@
setVendorID :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setVendorID mtrOperationalCredentialsClusterFabricDescriptorStruct value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct setVendorIDSelector (toNSNumber value)

-- | @- vendorId@
vendorId :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
vendorId mtrOperationalCredentialsClusterFabricDescriptorStruct =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct vendorIdSelector

-- | @- setVendorId:@
setVendorId :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setVendorId mtrOperationalCredentialsClusterFabricDescriptorStruct value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct setVendorIdSelector (toNSNumber value)

-- | @- fabricID@
fabricID :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
fabricID mtrOperationalCredentialsClusterFabricDescriptorStruct =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct fabricIDSelector

-- | @- setFabricID:@
setFabricID :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setFabricID mtrOperationalCredentialsClusterFabricDescriptorStruct value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct setFabricIDSelector (toNSNumber value)

-- | @- fabricId@
fabricId :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
fabricId mtrOperationalCredentialsClusterFabricDescriptorStruct =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct fabricIdSelector

-- | @- setFabricId:@
setFabricId :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setFabricId mtrOperationalCredentialsClusterFabricDescriptorStruct value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct setFabricIdSelector (toNSNumber value)

-- | @- nodeID@
nodeID :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
nodeID mtrOperationalCredentialsClusterFabricDescriptorStruct =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setNodeID mtrOperationalCredentialsClusterFabricDescriptorStruct value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct setNodeIDSelector (toNSNumber value)

-- | @- nodeId@
nodeId :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
nodeId mtrOperationalCredentialsClusterFabricDescriptorStruct =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct nodeIdSelector

-- | @- setNodeId:@
setNodeId :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setNodeId mtrOperationalCredentialsClusterFabricDescriptorStruct value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct setNodeIdSelector (toNSNumber value)

-- | @- label@
label :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSString)
label mtrOperationalCredentialsClusterFabricDescriptorStruct =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSString value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setLabel mtrOperationalCredentialsClusterFabricDescriptorStruct value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct setLabelSelector (toNSString value)

-- | @- vidVerificationStatement@
vidVerificationStatement :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSData)
vidVerificationStatement mtrOperationalCredentialsClusterFabricDescriptorStruct =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct vidVerificationStatementSelector

-- | @- setVidVerificationStatement:@
setVidVerificationStatement :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSData value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setVidVerificationStatement mtrOperationalCredentialsClusterFabricDescriptorStruct value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct setVidVerificationStatementSelector (toNSData value)

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct => mtrOperationalCredentialsClusterFabricDescriptorStruct -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterFabricDescriptorStruct =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterFabricDescriptorStruct mtrOperationalCredentialsClusterFabricDescriptorStruct, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptorStruct -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterFabricDescriptorStruct value =
  sendMessage mtrOperationalCredentialsClusterFabricDescriptorStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rootPublicKey@
rootPublicKeySelector :: Selector '[] (Id NSData)
rootPublicKeySelector = mkSelector "rootPublicKey"

-- | @Selector@ for @setRootPublicKey:@
setRootPublicKeySelector :: Selector '[Id NSData] ()
setRootPublicKeySelector = mkSelector "setRootPublicKey:"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector '[Id NSNumber] ()
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @vendorId@
vendorIdSelector :: Selector '[] (Id NSNumber)
vendorIdSelector = mkSelector "vendorId"

-- | @Selector@ for @setVendorId:@
setVendorIdSelector :: Selector '[Id NSNumber] ()
setVendorIdSelector = mkSelector "setVendorId:"

-- | @Selector@ for @fabricID@
fabricIDSelector :: Selector '[] (Id NSNumber)
fabricIDSelector = mkSelector "fabricID"

-- | @Selector@ for @setFabricID:@
setFabricIDSelector :: Selector '[Id NSNumber] ()
setFabricIDSelector = mkSelector "setFabricID:"

-- | @Selector@ for @fabricId@
fabricIdSelector :: Selector '[] (Id NSNumber)
fabricIdSelector = mkSelector "fabricId"

-- | @Selector@ for @setFabricId:@
setFabricIdSelector :: Selector '[Id NSNumber] ()
setFabricIdSelector = mkSelector "setFabricId:"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @nodeId@
nodeIdSelector :: Selector '[] (Id NSNumber)
nodeIdSelector = mkSelector "nodeId"

-- | @Selector@ for @setNodeId:@
setNodeIdSelector :: Selector '[Id NSNumber] ()
setNodeIdSelector = mkSelector "setNodeId:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @vidVerificationStatement@
vidVerificationStatementSelector :: Selector '[] (Id NSData)
vidVerificationStatementSelector = mkSelector "vidVerificationStatement"

-- | @Selector@ for @setVidVerificationStatement:@
setVidVerificationStatementSelector :: Selector '[Id NSData] ()
setVidVerificationStatementSelector = mkSelector "setVidVerificationStatement:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

