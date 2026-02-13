{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterThreadInterfaceScanResult@.
module ObjC.Matter.MTRNetworkCommissioningClusterThreadInterfaceScanResult
  ( MTRNetworkCommissioningClusterThreadInterfaceScanResult
  , IsMTRNetworkCommissioningClusterThreadInterfaceScanResult(..)
  , panId
  , setPanId
  , extendedPanId
  , setExtendedPanId
  , networkName
  , setNetworkName
  , channel
  , setChannel
  , version
  , setVersion
  , extendedAddress
  , setExtendedAddress
  , rssi
  , setRssi
  , lqi
  , setLqi
  , channelSelector
  , extendedAddressSelector
  , extendedPanIdSelector
  , lqiSelector
  , networkNameSelector
  , panIdSelector
  , rssiSelector
  , setChannelSelector
  , setExtendedAddressSelector
  , setExtendedPanIdSelector
  , setLqiSelector
  , setNetworkNameSelector
  , setPanIdSelector
  , setRssiSelector
  , setVersionSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- panId@
panId :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
panId mtrNetworkCommissioningClusterThreadInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult panIdSelector

-- | @- setPanId:@
setPanId :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setPanId mtrNetworkCommissioningClusterThreadInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult setPanIdSelector (toNSNumber value)

-- | @- extendedPanId@
extendedPanId :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
extendedPanId mtrNetworkCommissioningClusterThreadInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult extendedPanIdSelector

-- | @- setExtendedPanId:@
setExtendedPanId :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setExtendedPanId mtrNetworkCommissioningClusterThreadInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult setExtendedPanIdSelector (toNSNumber value)

-- | @- networkName@
networkName :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSString)
networkName mtrNetworkCommissioningClusterThreadInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult networkNameSelector

-- | @- setNetworkName:@
setNetworkName :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSString value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setNetworkName mtrNetworkCommissioningClusterThreadInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult setNetworkNameSelector (toNSString value)

-- | @- channel@
channel :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
channel mtrNetworkCommissioningClusterThreadInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult channelSelector

-- | @- setChannel:@
setChannel :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setChannel mtrNetworkCommissioningClusterThreadInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult setChannelSelector (toNSNumber value)

-- | @- version@
version :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
version mtrNetworkCommissioningClusterThreadInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult versionSelector

-- | @- setVersion:@
setVersion :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setVersion mtrNetworkCommissioningClusterThreadInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult setVersionSelector (toNSNumber value)

-- | @- extendedAddress@
extendedAddress :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSData)
extendedAddress mtrNetworkCommissioningClusterThreadInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult extendedAddressSelector

-- | @- setExtendedAddress:@
setExtendedAddress :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSData value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setExtendedAddress mtrNetworkCommissioningClusterThreadInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult setExtendedAddressSelector (toNSData value)

-- | @- rssi@
rssi :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
rssi mtrNetworkCommissioningClusterThreadInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult rssiSelector

-- | @- setRssi:@
setRssi :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setRssi mtrNetworkCommissioningClusterThreadInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult setRssiSelector (toNSNumber value)

-- | @- lqi@
lqi :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
lqi mtrNetworkCommissioningClusterThreadInterfaceScanResult =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult lqiSelector

-- | @- setLqi:@
setLqi :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setLqi mtrNetworkCommissioningClusterThreadInterfaceScanResult value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResult setLqiSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @panId@
panIdSelector :: Selector '[] (Id NSNumber)
panIdSelector = mkSelector "panId"

-- | @Selector@ for @setPanId:@
setPanIdSelector :: Selector '[Id NSNumber] ()
setPanIdSelector = mkSelector "setPanId:"

-- | @Selector@ for @extendedPanId@
extendedPanIdSelector :: Selector '[] (Id NSNumber)
extendedPanIdSelector = mkSelector "extendedPanId"

-- | @Selector@ for @setExtendedPanId:@
setExtendedPanIdSelector :: Selector '[Id NSNumber] ()
setExtendedPanIdSelector = mkSelector "setExtendedPanId:"

-- | @Selector@ for @networkName@
networkNameSelector :: Selector '[] (Id NSString)
networkNameSelector = mkSelector "networkName"

-- | @Selector@ for @setNetworkName:@
setNetworkNameSelector :: Selector '[Id NSString] ()
setNetworkNameSelector = mkSelector "setNetworkName:"

-- | @Selector@ for @channel@
channelSelector :: Selector '[] (Id NSNumber)
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector '[Id NSNumber] ()
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSNumber)
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector '[Id NSNumber] ()
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @extendedAddress@
extendedAddressSelector :: Selector '[] (Id NSData)
extendedAddressSelector = mkSelector "extendedAddress"

-- | @Selector@ for @setExtendedAddress:@
setExtendedAddressSelector :: Selector '[Id NSData] ()
setExtendedAddressSelector = mkSelector "setExtendedAddress:"

-- | @Selector@ for @rssi@
rssiSelector :: Selector '[] (Id NSNumber)
rssiSelector = mkSelector "rssi"

-- | @Selector@ for @setRssi:@
setRssiSelector :: Selector '[Id NSNumber] ()
setRssiSelector = mkSelector "setRssi:"

-- | @Selector@ for @lqi@
lqiSelector :: Selector '[] (Id NSNumber)
lqiSelector = mkSelector "lqi"

-- | @Selector@ for @setLqi:@
setLqiSelector :: Selector '[Id NSNumber] ()
setLqiSelector = mkSelector "setLqi:"

