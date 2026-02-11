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
  , macAddressForInterfaceNamedSelector
  , supportedInterfacesSelector
  , isAVBEnabledOnInterfaceNamedSelector
  , isAVBCapableInterfaceNamedSelector
  , initSelector
  , initWithInterfaceNameSelector
  , myEntityIDSelector
  , interfaceNameSelector
  , entityDiscoverySelector
  , aecpSelector
  , acmpSelector


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
    withObjCPtr anInterfaceName $ \raw_anInterfaceName ->
      sendClassMsg cls' (mkSelector "macAddressForInterfaceNamed:") (retPtr retVoid) [argPtr (castPtr raw_anInterfaceName :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "supportedInterfaces") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    withObjCPtr anInterfaceName $ \raw_anInterfaceName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isAVBEnabledOnInterfaceNamed:") retCULong [argPtr (castPtr raw_anInterfaceName :: Ptr ())]

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
    withObjCPtr anInterfaceName $ \raw_anInterfaceName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isAVBCapableInterfaceNamed:") retCULong [argPtr (castPtr raw_anInterfaceName :: Ptr ())]

-- | @- init@
init_ :: IsAVBInterface avbInterface => avbInterface -> IO (Id AVBInterface)
init_ avbInterface  =
  sendMsg avbInterface (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithInterfaceName avbInterface  anInterfaceName =
withObjCPtr anInterfaceName $ \raw_anInterfaceName ->
    sendMsg avbInterface (mkSelector "initWithInterfaceName:") (retPtr retVoid) [argPtr (castPtr raw_anInterfaceName :: Ptr ())] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "myEntityID") retCULong []

-- | interfaceName
--
-- The BSD interface name.
--
-- ObjC selector: @- interfaceName@
interfaceName :: IsAVBInterface avbInterface => avbInterface -> IO (Id NSString)
interfaceName avbInterface  =
  sendMsg avbInterface (mkSelector "interfaceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | entityDiscovery
--
-- The IEEE Std 1722.1™-2013 entity discovery for the interface.
--
-- ObjC selector: @- entityDiscovery@
entityDiscovery :: IsAVBInterface avbInterface => avbInterface -> IO (Id AVB17221EntityDiscovery)
entityDiscovery avbInterface  =
  sendMsg avbInterface (mkSelector "entityDiscovery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | aecp
--
-- The IEEE Std 1722.1™-2013 AECP interface for the interface.
--
-- ObjC selector: @- aecp@
aecp :: IsAVBInterface avbInterface => avbInterface -> IO (Id AVB17221AECPInterface)
aecp avbInterface  =
  sendMsg avbInterface (mkSelector "aecp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | acmp
--
-- The IEEE Std 1722.1™-2013 ACMP interface for the interface.
--
-- ObjC selector: @- acmp@
acmp :: IsAVBInterface avbInterface => avbInterface -> IO (Id AVB17221ACMPInterface)
acmp avbInterface  =
  sendMsg avbInterface (mkSelector "acmp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @macAddressForInterfaceNamed:@
macAddressForInterfaceNamedSelector :: Selector
macAddressForInterfaceNamedSelector = mkSelector "macAddressForInterfaceNamed:"

-- | @Selector@ for @supportedInterfaces@
supportedInterfacesSelector :: Selector
supportedInterfacesSelector = mkSelector "supportedInterfaces"

-- | @Selector@ for @isAVBEnabledOnInterfaceNamed:@
isAVBEnabledOnInterfaceNamedSelector :: Selector
isAVBEnabledOnInterfaceNamedSelector = mkSelector "isAVBEnabledOnInterfaceNamed:"

-- | @Selector@ for @isAVBCapableInterfaceNamed:@
isAVBCapableInterfaceNamedSelector :: Selector
isAVBCapableInterfaceNamedSelector = mkSelector "isAVBCapableInterfaceNamed:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterfaceName:@
initWithInterfaceNameSelector :: Selector
initWithInterfaceNameSelector = mkSelector "initWithInterfaceName:"

-- | @Selector@ for @myEntityID@
myEntityIDSelector :: Selector
myEntityIDSelector = mkSelector "myEntityID"

-- | @Selector@ for @interfaceName@
interfaceNameSelector :: Selector
interfaceNameSelector = mkSelector "interfaceName"

-- | @Selector@ for @entityDiscovery@
entityDiscoverySelector :: Selector
entityDiscoverySelector = mkSelector "entityDiscovery"

-- | @Selector@ for @aecp@
aecpSelector :: Selector
aecpSelector = mkSelector "aecp"

-- | @Selector@ for @acmp@
acmpSelector :: Selector
acmpSelector = mkSelector "acmp"

