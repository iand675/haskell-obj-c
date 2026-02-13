{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterNetworkInterface@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterNetworkInterface
  ( MTRGeneralDiagnosticsClusterNetworkInterface
  , IsMTRGeneralDiagnosticsClusterNetworkInterface(..)
  , name
  , setName
  , isOperational
  , setIsOperational
  , offPremiseServicesReachableIPv4
  , setOffPremiseServicesReachableIPv4
  , offPremiseServicesReachableIPv6
  , setOffPremiseServicesReachableIPv6
  , hardwareAddress
  , setHardwareAddress
  , iPv4Addresses
  , setIPv4Addresses
  , iPv6Addresses
  , setIPv6Addresses
  , type_
  , setType
  , hardwareAddressSelector
  , iPv4AddressesSelector
  , iPv6AddressesSelector
  , isOperationalSelector
  , nameSelector
  , offPremiseServicesReachableIPv4Selector
  , offPremiseServicesReachableIPv6Selector
  , setHardwareAddressSelector
  , setIPv4AddressesSelector
  , setIPv6AddressesSelector
  , setIsOperationalSelector
  , setNameSelector
  , setOffPremiseServicesReachableIPv4Selector
  , setOffPremiseServicesReachableIPv6Selector
  , setTypeSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSString)
name mtrGeneralDiagnosticsClusterNetworkInterface =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface nameSelector

-- | @- setName:@
setName :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSString value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setName mtrGeneralDiagnosticsClusterNetworkInterface value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface setNameSelector (toNSString value)

-- | @- isOperational@
isOperational :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSNumber)
isOperational mtrGeneralDiagnosticsClusterNetworkInterface =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface isOperationalSelector

-- | @- setIsOperational:@
setIsOperational :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setIsOperational mtrGeneralDiagnosticsClusterNetworkInterface value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface setIsOperationalSelector (toNSNumber value)

-- | @- offPremiseServicesReachableIPv4@
offPremiseServicesReachableIPv4 :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSNumber)
offPremiseServicesReachableIPv4 mtrGeneralDiagnosticsClusterNetworkInterface =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface offPremiseServicesReachableIPv4Selector

-- | @- setOffPremiseServicesReachableIPv4:@
setOffPremiseServicesReachableIPv4 :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setOffPremiseServicesReachableIPv4 mtrGeneralDiagnosticsClusterNetworkInterface value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface setOffPremiseServicesReachableIPv4Selector (toNSNumber value)

-- | @- offPremiseServicesReachableIPv6@
offPremiseServicesReachableIPv6 :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSNumber)
offPremiseServicesReachableIPv6 mtrGeneralDiagnosticsClusterNetworkInterface =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface offPremiseServicesReachableIPv6Selector

-- | @- setOffPremiseServicesReachableIPv6:@
setOffPremiseServicesReachableIPv6 :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setOffPremiseServicesReachableIPv6 mtrGeneralDiagnosticsClusterNetworkInterface value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface setOffPremiseServicesReachableIPv6Selector (toNSNumber value)

-- | @- hardwareAddress@
hardwareAddress :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSData)
hardwareAddress mtrGeneralDiagnosticsClusterNetworkInterface =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface hardwareAddressSelector

-- | @- setHardwareAddress:@
setHardwareAddress :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSData value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setHardwareAddress mtrGeneralDiagnosticsClusterNetworkInterface value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface setHardwareAddressSelector (toNSData value)

-- | @- iPv4Addresses@
iPv4Addresses :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSArray)
iPv4Addresses mtrGeneralDiagnosticsClusterNetworkInterface =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface iPv4AddressesSelector

-- | @- setIPv4Addresses:@
setIPv4Addresses :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSArray value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setIPv4Addresses mtrGeneralDiagnosticsClusterNetworkInterface value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface setIPv4AddressesSelector (toNSArray value)

-- | @- iPv6Addresses@
iPv6Addresses :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSArray)
iPv6Addresses mtrGeneralDiagnosticsClusterNetworkInterface =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface iPv6AddressesSelector

-- | @- setIPv6Addresses:@
setIPv6Addresses :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSArray value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setIPv6Addresses mtrGeneralDiagnosticsClusterNetworkInterface value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface setIPv6AddressesSelector (toNSArray value)

-- | @- type@
type_ :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSNumber)
type_ mtrGeneralDiagnosticsClusterNetworkInterface =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface typeSelector

-- | @- setType:@
setType :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setType mtrGeneralDiagnosticsClusterNetworkInterface value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterface setTypeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @isOperational@
isOperationalSelector :: Selector '[] (Id NSNumber)
isOperationalSelector = mkSelector "isOperational"

-- | @Selector@ for @setIsOperational:@
setIsOperationalSelector :: Selector '[Id NSNumber] ()
setIsOperationalSelector = mkSelector "setIsOperational:"

-- | @Selector@ for @offPremiseServicesReachableIPv4@
offPremiseServicesReachableIPv4Selector :: Selector '[] (Id NSNumber)
offPremiseServicesReachableIPv4Selector = mkSelector "offPremiseServicesReachableIPv4"

-- | @Selector@ for @setOffPremiseServicesReachableIPv4:@
setOffPremiseServicesReachableIPv4Selector :: Selector '[Id NSNumber] ()
setOffPremiseServicesReachableIPv4Selector = mkSelector "setOffPremiseServicesReachableIPv4:"

-- | @Selector@ for @offPremiseServicesReachableIPv6@
offPremiseServicesReachableIPv6Selector :: Selector '[] (Id NSNumber)
offPremiseServicesReachableIPv6Selector = mkSelector "offPremiseServicesReachableIPv6"

-- | @Selector@ for @setOffPremiseServicesReachableIPv6:@
setOffPremiseServicesReachableIPv6Selector :: Selector '[Id NSNumber] ()
setOffPremiseServicesReachableIPv6Selector = mkSelector "setOffPremiseServicesReachableIPv6:"

-- | @Selector@ for @hardwareAddress@
hardwareAddressSelector :: Selector '[] (Id NSData)
hardwareAddressSelector = mkSelector "hardwareAddress"

-- | @Selector@ for @setHardwareAddress:@
setHardwareAddressSelector :: Selector '[Id NSData] ()
setHardwareAddressSelector = mkSelector "setHardwareAddress:"

-- | @Selector@ for @iPv4Addresses@
iPv4AddressesSelector :: Selector '[] (Id NSArray)
iPv4AddressesSelector = mkSelector "iPv4Addresses"

-- | @Selector@ for @setIPv4Addresses:@
setIPv4AddressesSelector :: Selector '[Id NSArray] ()
setIPv4AddressesSelector = mkSelector "setIPv4Addresses:"

-- | @Selector@ for @iPv6Addresses@
iPv6AddressesSelector :: Selector '[] (Id NSArray)
iPv6AddressesSelector = mkSelector "iPv6Addresses"

-- | @Selector@ for @setIPv6Addresses:@
setIPv6AddressesSelector :: Selector '[Id NSArray] ()
setIPv6AddressesSelector = mkSelector "setIPv6Addresses:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSNumber)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSNumber] ()
setTypeSelector = mkSelector "setType:"

