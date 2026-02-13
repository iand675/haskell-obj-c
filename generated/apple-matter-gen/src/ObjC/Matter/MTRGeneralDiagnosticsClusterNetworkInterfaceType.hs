{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterNetworkInterfaceType@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterNetworkInterfaceType
  ( MTRGeneralDiagnosticsClusterNetworkInterfaceType
  , IsMTRGeneralDiagnosticsClusterNetworkInterfaceType(..)
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
name :: IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> IO (Id NSString)
name mtrGeneralDiagnosticsClusterNetworkInterfaceType =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType nameSelector

-- | @- setName:@
setName :: (IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType, IsNSString value) => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> value -> IO ()
setName mtrGeneralDiagnosticsClusterNetworkInterfaceType value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType setNameSelector (toNSString value)

-- | @- isOperational@
isOperational :: IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> IO (Id NSNumber)
isOperational mtrGeneralDiagnosticsClusterNetworkInterfaceType =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType isOperationalSelector

-- | @- setIsOperational:@
setIsOperational :: (IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> value -> IO ()
setIsOperational mtrGeneralDiagnosticsClusterNetworkInterfaceType value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType setIsOperationalSelector (toNSNumber value)

-- | @- offPremiseServicesReachableIPv4@
offPremiseServicesReachableIPv4 :: IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> IO (Id NSNumber)
offPremiseServicesReachableIPv4 mtrGeneralDiagnosticsClusterNetworkInterfaceType =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType offPremiseServicesReachableIPv4Selector

-- | @- setOffPremiseServicesReachableIPv4:@
setOffPremiseServicesReachableIPv4 :: (IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> value -> IO ()
setOffPremiseServicesReachableIPv4 mtrGeneralDiagnosticsClusterNetworkInterfaceType value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType setOffPremiseServicesReachableIPv4Selector (toNSNumber value)

-- | @- offPremiseServicesReachableIPv6@
offPremiseServicesReachableIPv6 :: IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> IO (Id NSNumber)
offPremiseServicesReachableIPv6 mtrGeneralDiagnosticsClusterNetworkInterfaceType =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType offPremiseServicesReachableIPv6Selector

-- | @- setOffPremiseServicesReachableIPv6:@
setOffPremiseServicesReachableIPv6 :: (IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> value -> IO ()
setOffPremiseServicesReachableIPv6 mtrGeneralDiagnosticsClusterNetworkInterfaceType value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType setOffPremiseServicesReachableIPv6Selector (toNSNumber value)

-- | @- hardwareAddress@
hardwareAddress :: IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> IO (Id NSData)
hardwareAddress mtrGeneralDiagnosticsClusterNetworkInterfaceType =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType hardwareAddressSelector

-- | @- setHardwareAddress:@
setHardwareAddress :: (IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType, IsNSData value) => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> value -> IO ()
setHardwareAddress mtrGeneralDiagnosticsClusterNetworkInterfaceType value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType setHardwareAddressSelector (toNSData value)

-- | @- iPv4Addresses@
iPv4Addresses :: IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> IO (Id NSArray)
iPv4Addresses mtrGeneralDiagnosticsClusterNetworkInterfaceType =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType iPv4AddressesSelector

-- | @- setIPv4Addresses:@
setIPv4Addresses :: (IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType, IsNSArray value) => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> value -> IO ()
setIPv4Addresses mtrGeneralDiagnosticsClusterNetworkInterfaceType value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType setIPv4AddressesSelector (toNSArray value)

-- | @- iPv6Addresses@
iPv6Addresses :: IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> IO (Id NSArray)
iPv6Addresses mtrGeneralDiagnosticsClusterNetworkInterfaceType =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType iPv6AddressesSelector

-- | @- setIPv6Addresses:@
setIPv6Addresses :: (IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType, IsNSArray value) => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> value -> IO ()
setIPv6Addresses mtrGeneralDiagnosticsClusterNetworkInterfaceType value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType setIPv6AddressesSelector (toNSArray value)

-- | @- type@
type_ :: IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> IO (Id NSNumber)
type_ mtrGeneralDiagnosticsClusterNetworkInterfaceType =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType typeSelector

-- | @- setType:@
setType :: (IsMTRGeneralDiagnosticsClusterNetworkInterfaceType mtrGeneralDiagnosticsClusterNetworkInterfaceType, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterfaceType -> value -> IO ()
setType mtrGeneralDiagnosticsClusterNetworkInterfaceType value =
  sendMessage mtrGeneralDiagnosticsClusterNetworkInterfaceType setTypeSelector (toNSNumber value)

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

