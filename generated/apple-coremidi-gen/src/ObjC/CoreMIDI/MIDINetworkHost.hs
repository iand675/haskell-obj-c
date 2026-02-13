{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDINetworkHost@.
module ObjC.CoreMIDI.MIDINetworkHost
  ( MIDINetworkHost
  , IsMIDINetworkHost(..)
  , hostWithName_address_port
  , hostWithName_netService
  , hostWithName_netServiceName_netServiceDomain
  , hasSameAddressAs
  , name
  , address
  , port
  , netServiceName
  , netServiceDomain
  , addressSelector
  , hasSameAddressAsSelector
  , hostWithName_address_portSelector
  , hostWithName_netServiceName_netServiceDomainSelector
  , hostWithName_netServiceSelector
  , nameSelector
  , netServiceDomainSelector
  , netServiceNameSelector
  , portSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ hostWithName:address:port:@
hostWithName_address_port :: (IsNSString name, IsNSString address) => name -> address -> CULong -> IO (Id MIDINetworkHost)
hostWithName_address_port name address port =
  do
    cls' <- getRequiredClass "MIDINetworkHost"
    sendClassMessage cls' hostWithName_address_portSelector (toNSString name) (toNSString address) port

-- | @+ hostWithName:netService:@
hostWithName_netService :: (IsNSString name, IsNSNetService netService) => name -> netService -> IO (Id MIDINetworkHost)
hostWithName_netService name netService =
  do
    cls' <- getRequiredClass "MIDINetworkHost"
    sendClassMessage cls' hostWithName_netServiceSelector (toNSString name) (toNSNetService netService)

-- | @+ hostWithName:netServiceName:netServiceDomain:@
hostWithName_netServiceName_netServiceDomain :: (IsNSString name, IsNSString netServiceName, IsNSString netServiceDomain) => name -> netServiceName -> netServiceDomain -> IO (Id MIDINetworkHost)
hostWithName_netServiceName_netServiceDomain name netServiceName netServiceDomain =
  do
    cls' <- getRequiredClass "MIDINetworkHost"
    sendClassMessage cls' hostWithName_netServiceName_netServiceDomainSelector (toNSString name) (toNSString netServiceName) (toNSString netServiceDomain)

-- | @- hasSameAddressAs:@
hasSameAddressAs :: (IsMIDINetworkHost midiNetworkHost, IsMIDINetworkHost other) => midiNetworkHost -> other -> IO Bool
hasSameAddressAs midiNetworkHost other =
  sendMessage midiNetworkHost hasSameAddressAsSelector (toMIDINetworkHost other)

-- | @- name@
name :: IsMIDINetworkHost midiNetworkHost => midiNetworkHost -> IO (Id NSString)
name midiNetworkHost =
  sendMessage midiNetworkHost nameSelector

-- | @- address@
address :: IsMIDINetworkHost midiNetworkHost => midiNetworkHost -> IO (Id NSString)
address midiNetworkHost =
  sendMessage midiNetworkHost addressSelector

-- | @- port@
port :: IsMIDINetworkHost midiNetworkHost => midiNetworkHost -> IO CULong
port midiNetworkHost =
  sendMessage midiNetworkHost portSelector

-- | @- netServiceName@
netServiceName :: IsMIDINetworkHost midiNetworkHost => midiNetworkHost -> IO (Id NSString)
netServiceName midiNetworkHost =
  sendMessage midiNetworkHost netServiceNameSelector

-- | @- netServiceDomain@
netServiceDomain :: IsMIDINetworkHost midiNetworkHost => midiNetworkHost -> IO (Id NSString)
netServiceDomain midiNetworkHost =
  sendMessage midiNetworkHost netServiceDomainSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hostWithName:address:port:@
hostWithName_address_portSelector :: Selector '[Id NSString, Id NSString, CULong] (Id MIDINetworkHost)
hostWithName_address_portSelector = mkSelector "hostWithName:address:port:"

-- | @Selector@ for @hostWithName:netService:@
hostWithName_netServiceSelector :: Selector '[Id NSString, Id NSNetService] (Id MIDINetworkHost)
hostWithName_netServiceSelector = mkSelector "hostWithName:netService:"

-- | @Selector@ for @hostWithName:netServiceName:netServiceDomain:@
hostWithName_netServiceName_netServiceDomainSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id MIDINetworkHost)
hostWithName_netServiceName_netServiceDomainSelector = mkSelector "hostWithName:netServiceName:netServiceDomain:"

-- | @Selector@ for @hasSameAddressAs:@
hasSameAddressAsSelector :: Selector '[Id MIDINetworkHost] Bool
hasSameAddressAsSelector = mkSelector "hasSameAddressAs:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @address@
addressSelector :: Selector '[] (Id NSString)
addressSelector = mkSelector "address"

-- | @Selector@ for @port@
portSelector :: Selector '[] CULong
portSelector = mkSelector "port"

-- | @Selector@ for @netServiceName@
netServiceNameSelector :: Selector '[] (Id NSString)
netServiceNameSelector = mkSelector "netServiceName"

-- | @Selector@ for @netServiceDomain@
netServiceDomainSelector :: Selector '[] (Id NSString)
netServiceDomainSelector = mkSelector "netServiceDomain"

