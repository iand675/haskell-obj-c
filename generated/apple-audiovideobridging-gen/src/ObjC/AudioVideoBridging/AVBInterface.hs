{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVBInterface
--
-- AVBInterface is an abstract class providing a central access point for the AVB functionality of an interface.
--
-- AVBInterface is an abstract class providing a central access point for the AVB functionality of an interface.				AVBInterface objects should not be directly created as they cannot provide full functionality, instead a concrete				subclass should be instantiated.
--
-- Generated bindings for @AVBInterface@.
module ObjC.AudioVideoBridging.AVBInterface
  ( AVBInterface
  , IsAVBInterface(..)
  , macAddressForInterfaceNamed
  , supportedInterfaces
  , isAVBEnabledOnInterfaceNamed
  , isAVBCapableInterfaceNamed
  , init_
  , initWithInterfaceName
  , myEntityID
  , interfaceName
  , entityDiscovery
  , aecp
  , acmp
  , acmpSelector
  , aecpSelector
  , entityDiscoverySelector
  , initSelector
  , initWithInterfaceNameSelector
  , interfaceNameSelector
  , isAVBCapableInterfaceNamedSelector
  , isAVBEnabledOnInterfaceNamedSelector
  , macAddressForInterfaceNamedSelector
  , myEntityIDSelector
  , supportedInterfacesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | macAddressForInterfaceNamed:
--
-- This method looks up the MAC address for an interface with a given BSD name.
--
-- @anInterfaceName@ — The BSD name of the interface to get the address for.
--
-- Returns: An instance of AVBMACAddress if the lookup was successful, nil otherwise.
--
-- ObjC selector: @+ macAddressForInterfaceNamed:@
macAddressForInterfaceNamed :: IsNSString anInterfaceName => anInterfaceName -> IO (Id AVBMACAddress)
macAddressForInterfaceNamed anInterfaceName =
  do
    cls' <- getRequiredClass "AVBInterface"
    sendClassMessage cls' macAddressForInterfaceNamedSelector (toNSString anInterfaceName)

-- | supportedInterfaces
--
-- This method returns an array of BSD interface names of interfaces supporting AVB. An interface is included in this list if it claims it supports AVB.
--
-- Returns: An NSArray of NSStrings, with each string being the BSD name of an interface. This may return nil.
--
-- ObjC selector: @+ supportedInterfaces@
supportedInterfaces :: IO (Id NSArray)
supportedInterfaces  =
  do
    cls' <- getRequiredClass "AVBInterface"
    sendClassMessage cls' supportedInterfacesSelector

-- | isAVBEnabledOnInterfaceNamed:
--
-- This method determines if AVB has been enabled on an interface.
--
-- @anInterfaceName@ — The BSD name of the interface.
--
-- Returns: YES if AVB is enabled, NO otherwise.
--
-- ObjC selector: @+ isAVBEnabledOnInterfaceNamed:@
isAVBEnabledOnInterfaceNamed :: IsNSString anInterfaceName => anInterfaceName -> IO Bool
isAVBEnabledOnInterfaceNamed anInterfaceName =
  do
    cls' <- getRequiredClass "AVBInterface"
    sendClassMessage cls' isAVBEnabledOnInterfaceNamedSelector (toNSString anInterfaceName)

-- | isAVBCapableInterfaceNamed:
--
-- This method determines if AVB is supported on an interface.
--
-- @anInterfaceName@ — The BSD name of the interface.
--
-- Returns: YES if AVB is supported, NO otherwise.
--
-- ObjC selector: @+ isAVBCapableInterfaceNamed:@
isAVBCapableInterfaceNamed :: IsNSString anInterfaceName => anInterfaceName -> IO Bool
isAVBCapableInterfaceNamed anInterfaceName =
  do
    cls' <- getRequiredClass "AVBInterface"
    sendClassMessage cls' isAVBCapableInterfaceNamedSelector (toNSString anInterfaceName)

-- | @- init@
init_ :: IsAVBInterface avbInterface => avbInterface -> IO (Id AVBInterface)
init_ avbInterface =
  sendOwnedMessage avbInterface initSelector

-- | initWithInterfaceName:
--
-- This method initializes the receiver to work on the specified interface.
--
-- @anInterfaceName@ — The BSD name of the interface.
--
-- Returns: The initialized receiver.
--
-- ObjC selector: @- initWithInterfaceName:@
initWithInterfaceName :: (IsAVBInterface avbInterface, IsNSString anInterfaceName) => avbInterface -> anInterfaceName -> IO (Id AVBInterface)
initWithInterfaceName avbInterface anInterfaceName =
  sendOwnedMessage avbInterface initWithInterfaceNameSelector (toNSString anInterfaceName)

-- | myEntityID
--
-- This method returns the EntityID which is used by the built-in controller functionality of Mac OS X. This is either the FireWire GUID or an EUI64 based on the first found ethernet type interface (may be an ethernet port, USB ethernet adapter, PCI Express adapter or the AirPort card).
--
-- Returns: The EntityID which is used by the OS.
--
-- ObjC selector: @+ myEntityID@
myEntityID :: IO CULong
myEntityID  =
  do
    cls' <- getRequiredClass "AVBInterface"
    sendClassMessage cls' myEntityIDSelector

-- | interfaceName
--
-- The BSD interface name.
--
-- ObjC selector: @- interfaceName@
interfaceName :: IsAVBInterface avbInterface => avbInterface -> IO (Id NSString)
interfaceName avbInterface =
  sendMessage avbInterface interfaceNameSelector

-- | entityDiscovery
--
-- The IEEE Std 1722.1™-2013 entity discovery for the interface.
--
-- ObjC selector: @- entityDiscovery@
entityDiscovery :: IsAVBInterface avbInterface => avbInterface -> IO (Id AVB17221EntityDiscovery)
entityDiscovery avbInterface =
  sendMessage avbInterface entityDiscoverySelector

-- | aecp
--
-- The IEEE Std 1722.1™-2013 AECP interface for the interface.
--
-- ObjC selector: @- aecp@
aecp :: IsAVBInterface avbInterface => avbInterface -> IO (Id AVB17221AECPInterface)
aecp avbInterface =
  sendMessage avbInterface aecpSelector

-- | acmp
--
-- The IEEE Std 1722.1™-2013 ACMP interface for the interface.
--
-- ObjC selector: @- acmp@
acmp :: IsAVBInterface avbInterface => avbInterface -> IO (Id AVB17221ACMPInterface)
acmp avbInterface =
  sendMessage avbInterface acmpSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @macAddressForInterfaceNamed:@
macAddressForInterfaceNamedSelector :: Selector '[Id NSString] (Id AVBMACAddress)
macAddressForInterfaceNamedSelector = mkSelector "macAddressForInterfaceNamed:"

-- | @Selector@ for @supportedInterfaces@
supportedInterfacesSelector :: Selector '[] (Id NSArray)
supportedInterfacesSelector = mkSelector "supportedInterfaces"

-- | @Selector@ for @isAVBEnabledOnInterfaceNamed:@
isAVBEnabledOnInterfaceNamedSelector :: Selector '[Id NSString] Bool
isAVBEnabledOnInterfaceNamedSelector = mkSelector "isAVBEnabledOnInterfaceNamed:"

-- | @Selector@ for @isAVBCapableInterfaceNamed:@
isAVBCapableInterfaceNamedSelector :: Selector '[Id NSString] Bool
isAVBCapableInterfaceNamedSelector = mkSelector "isAVBCapableInterfaceNamed:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVBInterface)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterfaceName:@
initWithInterfaceNameSelector :: Selector '[Id NSString] (Id AVBInterface)
initWithInterfaceNameSelector = mkSelector "initWithInterfaceName:"

-- | @Selector@ for @myEntityID@
myEntityIDSelector :: Selector '[] CULong
myEntityIDSelector = mkSelector "myEntityID"

-- | @Selector@ for @interfaceName@
interfaceNameSelector :: Selector '[] (Id NSString)
interfaceNameSelector = mkSelector "interfaceName"

-- | @Selector@ for @entityDiscovery@
entityDiscoverySelector :: Selector '[] (Id AVB17221EntityDiscovery)
entityDiscoverySelector = mkSelector "entityDiscovery"

-- | @Selector@ for @aecp@
aecpSelector :: Selector '[] (Id AVB17221AECPInterface)
aecpSelector = mkSelector "aecp"

-- | @Selector@ for @acmp@
acmpSelector :: Selector '[] (Id AVB17221ACMPInterface)
acmpSelector = mkSelector "acmp"

