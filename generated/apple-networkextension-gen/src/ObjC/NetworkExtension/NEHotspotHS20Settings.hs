{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEHotspotHS20Settings
--
-- NEHotspotHS20Settings class provides a set of properties that are required   to discover and negotiate Hotspot 2.0 Wi-Fi networks.
--
-- Generated bindings for @NEHotspotHS20Settings@.
module ObjC.NetworkExtension.NEHotspotHS20Settings
  ( NEHotspotHS20Settings
  , IsNEHotspotHS20Settings(..)
  , initWithDomainName_roamingEnabled
  , domainName
  , roamingEnabled
  , setRoamingEnabled
  , roamingConsortiumOIs
  , setRoamingConsortiumOIs
  , naiRealmNames
  , setNaiRealmNames
  , mccAndMNCs
  , setMCCAndMNCs
  , initWithDomainName_roamingEnabledSelector
  , domainNameSelector
  , roamingEnabledSelector
  , setRoamingEnabledSelector
  , roamingConsortiumOIsSelector
  , setRoamingConsortiumOIsSelector
  , naiRealmNamesSelector
  , setNaiRealmNamesSelector
  , mccAndMNCsSelector
  , setMCCAndMNCsSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithDomainName:roamingEnabled
--
-- A designated initializer to instantiate a new NEHotspotHSSettings object.   This initializer is used to configure Legacy Hotspot or HS2.0 Wi-Fi Networks.
--
-- @domainName@ — The domain name of HS2.0 Wi-Fi Network
--
-- @roamingEnabled@ — If YES, allows connections to networks of roaming service providers.
--
-- ObjC selector: @- initWithDomainName:roamingEnabled:@
initWithDomainName_roamingEnabled :: (IsNEHotspotHS20Settings neHotspotHS20Settings, IsNSString domainName) => neHotspotHS20Settings -> domainName -> Bool -> IO (Id NEHotspotHS20Settings)
initWithDomainName_roamingEnabled neHotspotHS20Settings  domainName roamingEnabled =
  withObjCPtr domainName $ \raw_domainName ->
      sendMsg neHotspotHS20Settings (mkSelector "initWithDomainName:roamingEnabled:") (retPtr retVoid) [argPtr (castPtr raw_domainName :: Ptr ()), argCULong (if roamingEnabled then 1 else 0)] >>= ownedObject . castPtr

-- | domainName
--
-- Domain Name of Legacy Hotspot or Hotspot 2.0 Wi-Fi Network.   This Domain Name is used for Wi-Fi Hotspot 2.0 negotiation.
--
-- ObjC selector: @- domainName@
domainName :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO (Id NSString)
domainName neHotspotHS20Settings  =
    sendMsg neHotspotHS20Settings (mkSelector "domainName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | roamingEnabled
--
-- If set to YES, allows connection to networks of roaming service   providers. Defaults to NO.
--
-- ObjC selector: @- roamingEnabled@
roamingEnabled :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO Bool
roamingEnabled neHotspotHS20Settings  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg neHotspotHS20Settings (mkSelector "roamingEnabled") retCULong []

-- | roamingEnabled
--
-- If set to YES, allows connection to networks of roaming service   providers. Defaults to NO.
--
-- ObjC selector: @- setRoamingEnabled:@
setRoamingEnabled :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> Bool -> IO ()
setRoamingEnabled neHotspotHS20Settings  value =
    sendMsg neHotspotHS20Settings (mkSelector "setRoamingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | roamingConsortiumOIs
--
-- Array of Roaming Consortium Organization Identifiers used   for Wi-Fi Hotspot 2.0 negotiation.
--
-- ObjC selector: @- roamingConsortiumOIs@
roamingConsortiumOIs :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO (Id NSArray)
roamingConsortiumOIs neHotspotHS20Settings  =
    sendMsg neHotspotHS20Settings (mkSelector "roamingConsortiumOIs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | roamingConsortiumOIs
--
-- Array of Roaming Consortium Organization Identifiers used   for Wi-Fi Hotspot 2.0 negotiation.
--
-- ObjC selector: @- setRoamingConsortiumOIs:@
setRoamingConsortiumOIs :: (IsNEHotspotHS20Settings neHotspotHS20Settings, IsNSArray value) => neHotspotHS20Settings -> value -> IO ()
setRoamingConsortiumOIs neHotspotHS20Settings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neHotspotHS20Settings (mkSelector "setRoamingConsortiumOIs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | naiRealmNames
--
-- Array of Network Access Identifier Realm names used for   Wi-Fi Hotspot 2.0 negotiation.
--
-- ObjC selector: @- naiRealmNames@
naiRealmNames :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO (Id NSArray)
naiRealmNames neHotspotHS20Settings  =
    sendMsg neHotspotHS20Settings (mkSelector "naiRealmNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | naiRealmNames
--
-- Array of Network Access Identifier Realm names used for   Wi-Fi Hotspot 2.0 negotiation.
--
-- ObjC selector: @- setNaiRealmNames:@
setNaiRealmNames :: (IsNEHotspotHS20Settings neHotspotHS20Settings, IsNSArray value) => neHotspotHS20Settings -> value -> IO ()
setNaiRealmNames neHotspotHS20Settings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neHotspotHS20Settings (mkSelector "setNaiRealmNames:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | MCCAndMNCs
--
-- Array of Mobile Country Code (MCC)/Mobile Network Code (MNC)   pairs used for Wi-Fi Hotspot 2.0 negotiation. Each string must contain   exactly six digits.
--
-- ObjC selector: @- MCCAndMNCs@
mccAndMNCs :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO (Id NSArray)
mccAndMNCs neHotspotHS20Settings  =
    sendMsg neHotspotHS20Settings (mkSelector "MCCAndMNCs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | MCCAndMNCs
--
-- Array of Mobile Country Code (MCC)/Mobile Network Code (MNC)   pairs used for Wi-Fi Hotspot 2.0 negotiation. Each string must contain   exactly six digits.
--
-- ObjC selector: @- setMCCAndMNCs:@
setMCCAndMNCs :: (IsNEHotspotHS20Settings neHotspotHS20Settings, IsNSArray value) => neHotspotHS20Settings -> value -> IO ()
setMCCAndMNCs neHotspotHS20Settings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neHotspotHS20Settings (mkSelector "setMCCAndMNCs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDomainName:roamingEnabled:@
initWithDomainName_roamingEnabledSelector :: Selector
initWithDomainName_roamingEnabledSelector = mkSelector "initWithDomainName:roamingEnabled:"

-- | @Selector@ for @domainName@
domainNameSelector :: Selector
domainNameSelector = mkSelector "domainName"

-- | @Selector@ for @roamingEnabled@
roamingEnabledSelector :: Selector
roamingEnabledSelector = mkSelector "roamingEnabled"

-- | @Selector@ for @setRoamingEnabled:@
setRoamingEnabledSelector :: Selector
setRoamingEnabledSelector = mkSelector "setRoamingEnabled:"

-- | @Selector@ for @roamingConsortiumOIs@
roamingConsortiumOIsSelector :: Selector
roamingConsortiumOIsSelector = mkSelector "roamingConsortiumOIs"

-- | @Selector@ for @setRoamingConsortiumOIs:@
setRoamingConsortiumOIsSelector :: Selector
setRoamingConsortiumOIsSelector = mkSelector "setRoamingConsortiumOIs:"

-- | @Selector@ for @naiRealmNames@
naiRealmNamesSelector :: Selector
naiRealmNamesSelector = mkSelector "naiRealmNames"

-- | @Selector@ for @setNaiRealmNames:@
setNaiRealmNamesSelector :: Selector
setNaiRealmNamesSelector = mkSelector "setNaiRealmNames:"

-- | @Selector@ for @MCCAndMNCs@
mccAndMNCsSelector :: Selector
mccAndMNCsSelector = mkSelector "MCCAndMNCs"

-- | @Selector@ for @setMCCAndMNCs:@
setMCCAndMNCsSelector :: Selector
setMCCAndMNCsSelector = mkSelector "setMCCAndMNCs:"

