{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterNetworkInfoStruct@.
module ObjC.Matter.MTRNetworkCommissioningClusterNetworkInfoStruct
  ( MTRNetworkCommissioningClusterNetworkInfoStruct
  , IsMTRNetworkCommissioningClusterNetworkInfoStruct(..)
  , networkID
  , setNetworkID
  , connected
  , setConnected
  , networkIdentifier
  , setNetworkIdentifier
  , clientIdentifier
  , setClientIdentifier
  , clientIdentifierSelector
  , connectedSelector
  , networkIDSelector
  , networkIdentifierSelector
  , setClientIdentifierSelector
  , setConnectedSelector
  , setNetworkIDSelector
  , setNetworkIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- networkID@
networkID :: IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct => mtrNetworkCommissioningClusterNetworkInfoStruct -> IO (Id NSData)
networkID mtrNetworkCommissioningClusterNetworkInfoStruct =
  sendMessage mtrNetworkCommissioningClusterNetworkInfoStruct networkIDSelector

-- | @- setNetworkID:@
setNetworkID :: (IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct, IsNSData value) => mtrNetworkCommissioningClusterNetworkInfoStruct -> value -> IO ()
setNetworkID mtrNetworkCommissioningClusterNetworkInfoStruct value =
  sendMessage mtrNetworkCommissioningClusterNetworkInfoStruct setNetworkIDSelector (toNSData value)

-- | @- connected@
connected :: IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct => mtrNetworkCommissioningClusterNetworkInfoStruct -> IO (Id NSNumber)
connected mtrNetworkCommissioningClusterNetworkInfoStruct =
  sendMessage mtrNetworkCommissioningClusterNetworkInfoStruct connectedSelector

-- | @- setConnected:@
setConnected :: (IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct, IsNSNumber value) => mtrNetworkCommissioningClusterNetworkInfoStruct -> value -> IO ()
setConnected mtrNetworkCommissioningClusterNetworkInfoStruct value =
  sendMessage mtrNetworkCommissioningClusterNetworkInfoStruct setConnectedSelector (toNSNumber value)

-- | @- networkIdentifier@
networkIdentifier :: IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct => mtrNetworkCommissioningClusterNetworkInfoStruct -> IO (Id NSData)
networkIdentifier mtrNetworkCommissioningClusterNetworkInfoStruct =
  sendMessage mtrNetworkCommissioningClusterNetworkInfoStruct networkIdentifierSelector

-- | @- setNetworkIdentifier:@
setNetworkIdentifier :: (IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct, IsNSData value) => mtrNetworkCommissioningClusterNetworkInfoStruct -> value -> IO ()
setNetworkIdentifier mtrNetworkCommissioningClusterNetworkInfoStruct value =
  sendMessage mtrNetworkCommissioningClusterNetworkInfoStruct setNetworkIdentifierSelector (toNSData value)

-- | @- clientIdentifier@
clientIdentifier :: IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct => mtrNetworkCommissioningClusterNetworkInfoStruct -> IO (Id NSData)
clientIdentifier mtrNetworkCommissioningClusterNetworkInfoStruct =
  sendMessage mtrNetworkCommissioningClusterNetworkInfoStruct clientIdentifierSelector

-- | @- setClientIdentifier:@
setClientIdentifier :: (IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct, IsNSData value) => mtrNetworkCommissioningClusterNetworkInfoStruct -> value -> IO ()
setClientIdentifier mtrNetworkCommissioningClusterNetworkInfoStruct value =
  sendMessage mtrNetworkCommissioningClusterNetworkInfoStruct setClientIdentifierSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkID@
networkIDSelector :: Selector '[] (Id NSData)
networkIDSelector = mkSelector "networkID"

-- | @Selector@ for @setNetworkID:@
setNetworkIDSelector :: Selector '[Id NSData] ()
setNetworkIDSelector = mkSelector "setNetworkID:"

-- | @Selector@ for @connected@
connectedSelector :: Selector '[] (Id NSNumber)
connectedSelector = mkSelector "connected"

-- | @Selector@ for @setConnected:@
setConnectedSelector :: Selector '[Id NSNumber] ()
setConnectedSelector = mkSelector "setConnected:"

-- | @Selector@ for @networkIdentifier@
networkIdentifierSelector :: Selector '[] (Id NSData)
networkIdentifierSelector = mkSelector "networkIdentifier"

-- | @Selector@ for @setNetworkIdentifier:@
setNetworkIdentifierSelector :: Selector '[Id NSData] ()
setNetworkIdentifierSelector = mkSelector "setNetworkIdentifier:"

-- | @Selector@ for @clientIdentifier@
clientIdentifierSelector :: Selector '[] (Id NSData)
clientIdentifierSelector = mkSelector "clientIdentifier"

-- | @Selector@ for @setClientIdentifier:@
setClientIdentifierSelector :: Selector '[Id NSData] ()
setClientIdentifierSelector = mkSelector "setClientIdentifier:"

