{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterThreadInterfaceScanResultStruct@.
module ObjC.Matter.MTRNetworkCommissioningClusterThreadInterfaceScanResultStruct
  ( MTRNetworkCommissioningClusterThreadInterfaceScanResultStruct
  , IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct(..)
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
panId :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
panId mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct panIdSelector

-- | @- setPanId:@
setPanId :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setPanId mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct setPanIdSelector (toNSNumber value)

-- | @- extendedPanId@
extendedPanId :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
extendedPanId mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct extendedPanIdSelector

-- | @- setExtendedPanId:@
setExtendedPanId :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setExtendedPanId mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct setExtendedPanIdSelector (toNSNumber value)

-- | @- networkName@
networkName :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSString)
networkName mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct networkNameSelector

-- | @- setNetworkName:@
setNetworkName :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSString value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setNetworkName mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct setNetworkNameSelector (toNSString value)

-- | @- channel@
channel :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
channel mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct channelSelector

-- | @- setChannel:@
setChannel :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setChannel mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct setChannelSelector (toNSNumber value)

-- | @- version@
version :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
version mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct versionSelector

-- | @- setVersion:@
setVersion :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setVersion mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct setVersionSelector (toNSNumber value)

-- | @- extendedAddress@
extendedAddress :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSData)
extendedAddress mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct extendedAddressSelector

-- | @- setExtendedAddress:@
setExtendedAddress :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSData value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setExtendedAddress mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct setExtendedAddressSelector (toNSData value)

-- | @- rssi@
rssi :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
rssi mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct rssiSelector

-- | @- setRssi:@
setRssi :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setRssi mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct setRssiSelector (toNSNumber value)

-- | @- lqi@
lqi :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
lqi mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct lqiSelector

-- | @- setLqi:@
setLqi :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setLqi mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct value =
  sendMessage mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct setLqiSelector (toNSNumber value)

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

