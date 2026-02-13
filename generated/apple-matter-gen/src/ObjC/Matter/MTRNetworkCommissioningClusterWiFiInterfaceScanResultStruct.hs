{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct@.
module ObjC.Matter.MTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct
  ( MTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct
  , IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct(..)
  , security
  , setSecurity
  , ssid
  , setSsid
  , bssid
  , setBssid
  , channel
  , setChannel
  , wiFiBand
  , setWiFiBand
  , rssi
  , setRssi
  , bssidSelector
  , channelSelector
  , rssiSelector
  , securitySelector
  , setBssidSelector
  , setChannelSelector
  , setRssiSelector
  , setSecuritySelector
  , setSsidSelector
  , setWiFiBandSelector
  , ssidSelector
  , wiFiBandSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- security@
security :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSNumber)
security mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct securitySelector

-- | @- setSecurity:@
setSecurity :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setSecurity mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct setSecuritySelector (toNSNumber value)

-- | @- ssid@
ssid :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSData)
ssid mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct ssidSelector

-- | @- setSsid:@
setSsid :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSData value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setSsid mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct setSsidSelector (toNSData value)

-- | @- bssid@
bssid :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSData)
bssid mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct bssidSelector

-- | @- setBssid:@
setBssid :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSData value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setBssid mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct setBssidSelector (toNSData value)

-- | @- channel@
channel :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSNumber)
channel mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct channelSelector

-- | @- setChannel:@
setChannel :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setChannel mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct setChannelSelector (toNSNumber value)

-- | @- wiFiBand@
wiFiBand :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSNumber)
wiFiBand mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct wiFiBandSelector

-- | @- setWiFiBand:@
setWiFiBand :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setWiFiBand mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct setWiFiBandSelector (toNSNumber value)

-- | @- rssi@
rssi :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> IO (Id NSNumber)
rssi mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct rssiSelector

-- | @- setRssi:@
setRssi :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResultStruct mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct -> value -> IO ()
setRssi mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResultStruct setRssiSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @security@
securitySelector :: Selector '[] (Id NSNumber)
securitySelector = mkSelector "security"

-- | @Selector@ for @setSecurity:@
setSecuritySelector :: Selector '[Id NSNumber] ()
setSecuritySelector = mkSelector "setSecurity:"

-- | @Selector@ for @ssid@
ssidSelector :: Selector '[] (Id NSData)
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @setSsid:@
setSsidSelector :: Selector '[Id NSData] ()
setSsidSelector = mkSelector "setSsid:"

-- | @Selector@ for @bssid@
bssidSelector :: Selector '[] (Id NSData)
bssidSelector = mkSelector "bssid"

-- | @Selector@ for @setBssid:@
setBssidSelector :: Selector '[Id NSData] ()
setBssidSelector = mkSelector "setBssid:"

-- | @Selector@ for @channel@
channelSelector :: Selector '[] (Id NSNumber)
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector '[Id NSNumber] ()
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @wiFiBand@
wiFiBandSelector :: Selector '[] (Id NSNumber)
wiFiBandSelector = mkSelector "wiFiBand"

-- | @Selector@ for @setWiFiBand:@
setWiFiBandSelector :: Selector '[Id NSNumber] ()
setWiFiBandSelector = mkSelector "setWiFiBand:"

-- | @Selector@ for @rssi@
rssiSelector :: Selector '[] (Id NSNumber)
rssiSelector = mkSelector "rssi"

-- | @Selector@ for @setRssi:@
setRssiSelector :: Selector '[Id NSNumber] ()
setRssiSelector = mkSelector "setRssi:"

