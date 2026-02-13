{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterWiFiInterfaceScanResult@.
module ObjC.Matter.MTRNetworkCommissioningClusterWiFiInterfaceScanResult
  ( MTRNetworkCommissioningClusterWiFiInterfaceScanResult
  , IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult(..)
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
security :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSNumber)
security mtrNetworkCommissioningClusterWiFiInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult securitySelector

-- | @- setSecurity:@
setSecurity :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setSecurity mtrNetworkCommissioningClusterWiFiInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult setSecuritySelector (toNSNumber value)

-- | @- ssid@
ssid :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSData)
ssid mtrNetworkCommissioningClusterWiFiInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult ssidSelector

-- | @- setSsid:@
setSsid :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSData value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setSsid mtrNetworkCommissioningClusterWiFiInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult setSsidSelector (toNSData value)

-- | @- bssid@
bssid :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSData)
bssid mtrNetworkCommissioningClusterWiFiInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult bssidSelector

-- | @- setBssid:@
setBssid :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSData value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setBssid mtrNetworkCommissioningClusterWiFiInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult setBssidSelector (toNSData value)

-- | @- channel@
channel :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSNumber)
channel mtrNetworkCommissioningClusterWiFiInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult channelSelector

-- | @- setChannel:@
setChannel :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setChannel mtrNetworkCommissioningClusterWiFiInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult setChannelSelector (toNSNumber value)

-- | @- wiFiBand@
wiFiBand :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSNumber)
wiFiBand mtrNetworkCommissioningClusterWiFiInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult wiFiBandSelector

-- | @- setWiFiBand:@
setWiFiBand :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setWiFiBand mtrNetworkCommissioningClusterWiFiInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult setWiFiBandSelector (toNSNumber value)

-- | @- rssi@
rssi :: IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> IO (Id NSNumber)
rssi mtrNetworkCommissioningClusterWiFiInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult rssiSelector

-- | @- setRssi:@
setRssi :: (IsMTRNetworkCommissioningClusterWiFiInterfaceScanResult mtrNetworkCommissioningClusterWiFiInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterWiFiInterfaceScanResult -> value -> IO ()
setRssi mtrNetworkCommissioningClusterWiFiInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterWiFiInterfaceScanResult setRssiSelector (toNSNumber value)

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

