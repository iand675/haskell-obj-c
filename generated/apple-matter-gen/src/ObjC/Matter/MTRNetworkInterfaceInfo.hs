{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information about a network interface on a Matter node.
--
-- Generated bindings for @MTRNetworkInterfaceInfo@.
module ObjC.Matter.MTRNetworkInterfaceInfo
  ( MTRNetworkInterfaceInfo
  , IsMTRNetworkInterfaceInfo(..)
  , init_
  , new
  , endpointID
  , featureMap
  , type_
  , endpointIDSelector
  , featureMapSelector
  , initSelector
  , newSelector
  , typeSelector

  -- * Enum types
  , MTRNetworkCommissioningFeature(MTRNetworkCommissioningFeature)
  , pattern MTRNetworkCommissioningFeatureWiFiNetworkInterface
  , pattern MTRNetworkCommissioningFeatureThreadNetworkInterface
  , pattern MTRNetworkCommissioningFeatureEthernetNetworkInterface
  , pattern MTRNetworkCommissioningFeaturePerDeviceCredentials

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRNetworkInterfaceInfo mtrNetworkInterfaceInfo => mtrNetworkInterfaceInfo -> IO (Id MTRNetworkInterfaceInfo)
init_ mtrNetworkInterfaceInfo =
  sendOwnedMessage mtrNetworkInterfaceInfo initSelector

-- | @+ new@
new :: IO (Id MTRNetworkInterfaceInfo)
new  =
  do
    cls' <- getRequiredClass "MTRNetworkInterfaceInfo"
    sendOwnedClassMessage cls' newSelector

-- | The endpoint this network interface is exposed on (i.e. the endpoint its corresponding Network Commissioning server cluster instance is on).
--
-- ObjC selector: @- endpointID@
endpointID :: IsMTRNetworkInterfaceInfo mtrNetworkInterfaceInfo => mtrNetworkInterfaceInfo -> IO (Id NSNumber)
endpointID mtrNetworkInterfaceInfo =
  sendMessage mtrNetworkInterfaceInfo endpointIDSelector

-- | The value of the FeatureMap attribute of the corresponding Network Commissioning cluster instance.
--
-- ObjC selector: @- featureMap@
featureMap :: IsMTRNetworkInterfaceInfo mtrNetworkInterfaceInfo => mtrNetworkInterfaceInfo -> IO (Id NSNumber)
featureMap mtrNetworkInterfaceInfo =
  sendMessage mtrNetworkInterfaceInfo featureMapSelector

-- | The network technology used by the interface.  This will be one of the MTRNetworkCommissioningFeature*NetworkInterface values (exactly one bit).
--
-- ObjC selector: @- type@
type_ :: IsMTRNetworkInterfaceInfo mtrNetworkInterfaceInfo => mtrNetworkInterfaceInfo -> IO MTRNetworkCommissioningFeature
type_ mtrNetworkInterfaceInfo =
  sendMessage mtrNetworkInterfaceInfo typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRNetworkInterfaceInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRNetworkInterfaceInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @featureMap@
featureMapSelector :: Selector '[] (Id NSNumber)
featureMapSelector = mkSelector "featureMap"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MTRNetworkCommissioningFeature
typeSelector = mkSelector "type"

