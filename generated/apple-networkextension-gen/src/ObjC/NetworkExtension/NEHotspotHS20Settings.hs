{-# LANGUAGE DataKinds #-}
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
  , domainNameSelector
  , initWithDomainName_roamingEnabledSelector
  , mccAndMNCsSelector
  , naiRealmNamesSelector
  , roamingConsortiumOIsSelector
  , roamingEnabledSelector
  , setMCCAndMNCsSelector
  , setNaiRealmNamesSelector
  , setRoamingConsortiumOIsSelector
  , setRoamingEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithDomainName_roamingEnabled neHotspotHS20Settings domainName roamingEnabled =
  sendOwnedMessage neHotspotHS20Settings initWithDomainName_roamingEnabledSelector (toNSString domainName) roamingEnabled

-- | domainName
--
-- Domain Name of Legacy Hotspot or Hotspot 2.0 Wi-Fi Network.   This Domain Name is used for Wi-Fi Hotspot 2.0 negotiation.
--
-- ObjC selector: @- domainName@
domainName :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO (Id NSString)
domainName neHotspotHS20Settings =
  sendMessage neHotspotHS20Settings domainNameSelector

-- | roamingEnabled
--
-- If set to YES, allows connection to networks of roaming service   providers. Defaults to NO.
--
-- ObjC selector: @- roamingEnabled@
roamingEnabled :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO Bool
roamingEnabled neHotspotHS20Settings =
  sendMessage neHotspotHS20Settings roamingEnabledSelector

-- | roamingEnabled
--
-- If set to YES, allows connection to networks of roaming service   providers. Defaults to NO.
--
-- ObjC selector: @- setRoamingEnabled:@
setRoamingEnabled :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> Bool -> IO ()
setRoamingEnabled neHotspotHS20Settings value =
  sendMessage neHotspotHS20Settings setRoamingEnabledSelector value

-- | roamingConsortiumOIs
--
-- Array of Roaming Consortium Organization Identifiers used   for Wi-Fi Hotspot 2.0 negotiation.
--
-- ObjC selector: @- roamingConsortiumOIs@
roamingConsortiumOIs :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO (Id NSArray)
roamingConsortiumOIs neHotspotHS20Settings =
  sendMessage neHotspotHS20Settings roamingConsortiumOIsSelector

-- | roamingConsortiumOIs
--
-- Array of Roaming Consortium Organization Identifiers used   for Wi-Fi Hotspot 2.0 negotiation.
--
-- ObjC selector: @- setRoamingConsortiumOIs:@
setRoamingConsortiumOIs :: (IsNEHotspotHS20Settings neHotspotHS20Settings, IsNSArray value) => neHotspotHS20Settings -> value -> IO ()
setRoamingConsortiumOIs neHotspotHS20Settings value =
  sendMessage neHotspotHS20Settings setRoamingConsortiumOIsSelector (toNSArray value)

-- | naiRealmNames
--
-- Array of Network Access Identifier Realm names used for   Wi-Fi Hotspot 2.0 negotiation.
--
-- ObjC selector: @- naiRealmNames@
naiRealmNames :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO (Id NSArray)
naiRealmNames neHotspotHS20Settings =
  sendMessage neHotspotHS20Settings naiRealmNamesSelector

-- | naiRealmNames
--
-- Array of Network Access Identifier Realm names used for   Wi-Fi Hotspot 2.0 negotiation.
--
-- ObjC selector: @- setNaiRealmNames:@
setNaiRealmNames :: (IsNEHotspotHS20Settings neHotspotHS20Settings, IsNSArray value) => neHotspotHS20Settings -> value -> IO ()
setNaiRealmNames neHotspotHS20Settings value =
  sendMessage neHotspotHS20Settings setNaiRealmNamesSelector (toNSArray value)

-- | MCCAndMNCs
--
-- Array of Mobile Country Code (MCC)/Mobile Network Code (MNC)   pairs used for Wi-Fi Hotspot 2.0 negotiation. Each string must contain   exactly six digits.
--
-- ObjC selector: @- MCCAndMNCs@
mccAndMNCs :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO (Id NSArray)
mccAndMNCs neHotspotHS20Settings =
  sendMessage neHotspotHS20Settings mccAndMNCsSelector

-- | MCCAndMNCs
--
-- Array of Mobile Country Code (MCC)/Mobile Network Code (MNC)   pairs used for Wi-Fi Hotspot 2.0 negotiation. Each string must contain   exactly six digits.
--
-- ObjC selector: @- setMCCAndMNCs:@
setMCCAndMNCs :: (IsNEHotspotHS20Settings neHotspotHS20Settings, IsNSArray value) => neHotspotHS20Settings -> value -> IO ()
setMCCAndMNCs neHotspotHS20Settings value =
  sendMessage neHotspotHS20Settings setMCCAndMNCsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDomainName:roamingEnabled:@
initWithDomainName_roamingEnabledSelector :: Selector '[Id NSString, Bool] (Id NEHotspotHS20Settings)
initWithDomainName_roamingEnabledSelector = mkSelector "initWithDomainName:roamingEnabled:"

-- | @Selector@ for @domainName@
domainNameSelector :: Selector '[] (Id NSString)
domainNameSelector = mkSelector "domainName"

-- | @Selector@ for @roamingEnabled@
roamingEnabledSelector :: Selector '[] Bool
roamingEnabledSelector = mkSelector "roamingEnabled"

-- | @Selector@ for @setRoamingEnabled:@
setRoamingEnabledSelector :: Selector '[Bool] ()
setRoamingEnabledSelector = mkSelector "setRoamingEnabled:"

-- | @Selector@ for @roamingConsortiumOIs@
roamingConsortiumOIsSelector :: Selector '[] (Id NSArray)
roamingConsortiumOIsSelector = mkSelector "roamingConsortiumOIs"

-- | @Selector@ for @setRoamingConsortiumOIs:@
setRoamingConsortiumOIsSelector :: Selector '[Id NSArray] ()
setRoamingConsortiumOIsSelector = mkSelector "setRoamingConsortiumOIs:"

-- | @Selector@ for @naiRealmNames@
naiRealmNamesSelector :: Selector '[] (Id NSArray)
naiRealmNamesSelector = mkSelector "naiRealmNames"

-- | @Selector@ for @setNaiRealmNames:@
setNaiRealmNamesSelector :: Selector '[Id NSArray] ()
setNaiRealmNamesSelector = mkSelector "setNaiRealmNames:"

-- | @Selector@ for @MCCAndMNCs@
mccAndMNCsSelector :: Selector '[] (Id NSArray)
mccAndMNCsSelector = mkSelector "MCCAndMNCs"

-- | @Selector@ for @setMCCAndMNCs:@
setMCCAndMNCsSelector :: Selector '[Id NSArray] ()
setMCCAndMNCsSelector = mkSelector "setMCCAndMNCs:"

