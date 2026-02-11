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
  , hostWithName_address_portSelector
  , hostWithName_netServiceSelector
  , hostWithName_netServiceName_netServiceDomainSelector
  , hasSameAddressAsSelector
  , nameSelector
  , addressSelector
  , portSelector
  , netServiceNameSelector
  , netServiceDomainSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ hostWithName:address:port:@
hostWithName_address_port :: (IsNSString name, IsNSString address) => name -> address -> CULong -> IO (Id MIDINetworkHost)
hostWithName_address_port name address port =
  do
    cls' <- getRequiredClass "MIDINetworkHost"
    withObjCPtr name $ \raw_name ->
      withObjCPtr address $ \raw_address ->
        sendClassMsg cls' (mkSelector "hostWithName:address:port:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_address :: Ptr ()), argCULong (fromIntegral port)] >>= retainedObject . castPtr

-- | @+ hostWithName:netService:@
hostWithName_netService :: (IsNSString name, IsNSNetService netService) => name -> netService -> IO (Id MIDINetworkHost)
hostWithName_netService name netService =
  do
    cls' <- getRequiredClass "MIDINetworkHost"
    withObjCPtr name $ \raw_name ->
      withObjCPtr netService $ \raw_netService ->
        sendClassMsg cls' (mkSelector "hostWithName:netService:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_netService :: Ptr ())] >>= retainedObject . castPtr

-- | @+ hostWithName:netServiceName:netServiceDomain:@
hostWithName_netServiceName_netServiceDomain :: (IsNSString name, IsNSString netServiceName, IsNSString netServiceDomain) => name -> netServiceName -> netServiceDomain -> IO (Id MIDINetworkHost)
hostWithName_netServiceName_netServiceDomain name netServiceName netServiceDomain =
  do
    cls' <- getRequiredClass "MIDINetworkHost"
    withObjCPtr name $ \raw_name ->
      withObjCPtr netServiceName $ \raw_netServiceName ->
        withObjCPtr netServiceDomain $ \raw_netServiceDomain ->
          sendClassMsg cls' (mkSelector "hostWithName:netServiceName:netServiceDomain:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_netServiceName :: Ptr ()), argPtr (castPtr raw_netServiceDomain :: Ptr ())] >>= retainedObject . castPtr

-- | @- hasSameAddressAs:@
hasSameAddressAs :: (IsMIDINetworkHost midiNetworkHost, IsMIDINetworkHost other) => midiNetworkHost -> other -> IO Bool
hasSameAddressAs midiNetworkHost  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiNetworkHost (mkSelector "hasSameAddressAs:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- name@
name :: IsMIDINetworkHost midiNetworkHost => midiNetworkHost -> IO (Id NSString)
name midiNetworkHost  =
  sendMsg midiNetworkHost (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- address@
address :: IsMIDINetworkHost midiNetworkHost => midiNetworkHost -> IO (Id NSString)
address midiNetworkHost  =
  sendMsg midiNetworkHost (mkSelector "address") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- port@
port :: IsMIDINetworkHost midiNetworkHost => midiNetworkHost -> IO CULong
port midiNetworkHost  =
  sendMsg midiNetworkHost (mkSelector "port") retCULong []

-- | @- netServiceName@
netServiceName :: IsMIDINetworkHost midiNetworkHost => midiNetworkHost -> IO (Id NSString)
netServiceName midiNetworkHost  =
  sendMsg midiNetworkHost (mkSelector "netServiceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- netServiceDomain@
netServiceDomain :: IsMIDINetworkHost midiNetworkHost => midiNetworkHost -> IO (Id NSString)
netServiceDomain midiNetworkHost  =
  sendMsg midiNetworkHost (mkSelector "netServiceDomain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hostWithName:address:port:@
hostWithName_address_portSelector :: Selector
hostWithName_address_portSelector = mkSelector "hostWithName:address:port:"

-- | @Selector@ for @hostWithName:netService:@
hostWithName_netServiceSelector :: Selector
hostWithName_netServiceSelector = mkSelector "hostWithName:netService:"

-- | @Selector@ for @hostWithName:netServiceName:netServiceDomain:@
hostWithName_netServiceName_netServiceDomainSelector :: Selector
hostWithName_netServiceName_netServiceDomainSelector = mkSelector "hostWithName:netServiceName:netServiceDomain:"

-- | @Selector@ for @hasSameAddressAs:@
hasSameAddressAsSelector :: Selector
hasSameAddressAsSelector = mkSelector "hasSameAddressAs:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @address@
addressSelector :: Selector
addressSelector = mkSelector "address"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

-- | @Selector@ for @netServiceName@
netServiceNameSelector :: Selector
netServiceNameSelector = mkSelector "netServiceName"

-- | @Selector@ for @netServiceDomain@
netServiceDomainSelector :: Selector
netServiceDomainSelector = mkSelector "netServiceDomain"

