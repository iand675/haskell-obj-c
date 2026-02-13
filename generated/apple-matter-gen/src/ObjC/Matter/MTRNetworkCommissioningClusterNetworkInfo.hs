{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterNetworkInfo@.
module ObjC.Matter.MTRNetworkCommissioningClusterNetworkInfo
  ( MTRNetworkCommissioningClusterNetworkInfo
  , IsMTRNetworkCommissioningClusterNetworkInfo(..)
  , networkID
  , setNetworkID
  , connected
  , setConnected
  , connectedSelector
  , networkIDSelector
  , setConnectedSelector
  , setNetworkIDSelector


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
networkID :: IsMTRNetworkCommissioningClusterNetworkInfo mtrNetworkCommissioningClusterNetworkInfo => mtrNetworkCommissioningClusterNetworkInfo -> IO (Id NSData)
networkID mtrNetworkCommissioningClusterNetworkInfo =
  sendMessage mtrNetworkCommissioningClusterNetworkInfo networkIDSelector

-- | @- setNetworkID:@
setNetworkID :: (IsMTRNetworkCommissioningClusterNetworkInfo mtrNetworkCommissioningClusterNetworkInfo, IsNSData value) => mtrNetworkCommissioningClusterNetworkInfo -> value -> IO ()
setNetworkID mtrNetworkCommissioningClusterNetworkInfo value =
  sendMessage mtrNetworkCommissioningClusterNetworkInfo setNetworkIDSelector (toNSData value)

-- | @- connected@
connected :: IsMTRNetworkCommissioningClusterNetworkInfo mtrNetworkCommissioningClusterNetworkInfo => mtrNetworkCommissioningClusterNetworkInfo -> IO (Id NSNumber)
connected mtrNetworkCommissioningClusterNetworkInfo =
  sendMessage mtrNetworkCommissioningClusterNetworkInfo connectedSelector

-- | @- setConnected:@
setConnected :: (IsMTRNetworkCommissioningClusterNetworkInfo mtrNetworkCommissioningClusterNetworkInfo, IsNSNumber value) => mtrNetworkCommissioningClusterNetworkInfo -> value -> IO ()
setConnected mtrNetworkCommissioningClusterNetworkInfo value =
  sendMessage mtrNetworkCommissioningClusterNetworkInfo setConnectedSelector (toNSNumber value)

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

