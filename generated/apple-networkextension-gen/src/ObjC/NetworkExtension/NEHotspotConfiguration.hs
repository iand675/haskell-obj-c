{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEHotspotConfiguration
--
-- The NEHotspotConfiguration class represents set of properties that are required   to configure a Wi-Fi Network.
--
-- Generated bindings for @NEHotspotConfiguration@.
module ObjC.NetworkExtension.NEHotspotConfiguration
  ( NEHotspotConfiguration
  , IsNEHotspotConfiguration(..)
  , initWithSSID
  , initWithSSID_passphrase_isWEP
  , initWithSSID_eapSettings
  , initWithHS20Settings_eapSettings
  , initWithSSIDPrefix
  , initWithSSIDPrefix_passphrase_isWEP
  , ssid
  , ssidPrefix
  , joinOnce
  , setJoinOnce
  , lifeTimeInDays
  , setLifeTimeInDays
  , hidden
  , setHidden
  , hiddenSelector
  , initWithHS20Settings_eapSettingsSelector
  , initWithSSIDPrefixSelector
  , initWithSSIDPrefix_passphrase_isWEPSelector
  , initWithSSIDSelector
  , initWithSSID_eapSettingsSelector
  , initWithSSID_passphrase_isWEPSelector
  , joinOnceSelector
  , lifeTimeInDaysSelector
  , setHiddenSelector
  , setJoinOnceSelector
  , setLifeTimeInDaysSelector
  , ssidPrefixSelector
  , ssidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithSSID:
--
-- A designated initializer to instantiate a new NEHotspotConfiguration object.   This initializer is used to configure open Wi-Fi Networks.
--
-- @SSID@ — The SSID of the open Wi-Fi Network.   Length of SSID must be between 1 and 32 characters.
--
-- ObjC selector: @- initWithSSID:@
initWithSSID :: (IsNEHotspotConfiguration neHotspotConfiguration, IsNSString ssid) => neHotspotConfiguration -> ssid -> IO (Id NEHotspotConfiguration)
initWithSSID neHotspotConfiguration ssid =
  sendOwnedMessage neHotspotConfiguration initWithSSIDSelector (toNSString ssid)

-- | initWithSSID:passphrase:isWEP
--
-- A designated initializer to instantiate a new NEHotspotConfiguration object.   This initializer is used configure either WEP or WPA/WPA2 Personal Wi-Fi Networks.
--
-- @SSID@ — The SSID of the WEP or WPA/WPA2 Personal Wi-Fi Network
--
-- @passphrase@ — The passphrase credential.   For WPA/WPA2 Personal networks: between 8 and 63 characters.   For Static WEP(64bit)  : 10 Hex Digits   For Static WEP(128bit) : 26 Hex Digits
--
-- @isWEP@ — YES specifies WEP Wi-Fi Network else WPA/WPA2 Personal Wi-Fi Network
--
-- ObjC selector: @- initWithSSID:passphrase:isWEP:@
initWithSSID_passphrase_isWEP :: (IsNEHotspotConfiguration neHotspotConfiguration, IsNSString ssid, IsNSString passphrase) => neHotspotConfiguration -> ssid -> passphrase -> Bool -> IO (Id NEHotspotConfiguration)
initWithSSID_passphrase_isWEP neHotspotConfiguration ssid passphrase isWEP =
  sendOwnedMessage neHotspotConfiguration initWithSSID_passphrase_isWEPSelector (toNSString ssid) (toNSString passphrase) isWEP

-- | initWithSSID:eapSettings
--
-- A designated initializer to instantiate a new NEHotspotConfiguration object.   This initializer is used configure WPA/WPA2 Enterprise Wi-Fi Networks.
--
-- @SSID@ — The SSID of WPA/WPA2 Enterprise Wi-Fi Network
--
-- @eapSettings@ — EAP configuration
--
-- ObjC selector: @- initWithSSID:eapSettings:@
initWithSSID_eapSettings :: (IsNEHotspotConfiguration neHotspotConfiguration, IsNSString ssid, IsNEHotspotEAPSettings eapSettings) => neHotspotConfiguration -> ssid -> eapSettings -> IO (Id NEHotspotConfiguration)
initWithSSID_eapSettings neHotspotConfiguration ssid eapSettings =
  sendOwnedMessage neHotspotConfiguration initWithSSID_eapSettingsSelector (toNSString ssid) (toNEHotspotEAPSettings eapSettings)

-- | initWithHS20Settings:eapSettings
--
-- A designated initializer to instantiate a new NEHotspotConfiguration object.   This initializer is used configure HS2.0 Wi-Fi Networks.
--
-- @hs20Settings@ — Hotspot 2.0 configuration
--
-- @eapSettings@ — EAP configuration
--
-- ObjC selector: @- initWithHS20Settings:eapSettings:@
initWithHS20Settings_eapSettings :: (IsNEHotspotConfiguration neHotspotConfiguration, IsNEHotspotHS20Settings hs20Settings, IsNEHotspotEAPSettings eapSettings) => neHotspotConfiguration -> hs20Settings -> eapSettings -> IO (Id NEHotspotConfiguration)
initWithHS20Settings_eapSettings neHotspotConfiguration hs20Settings eapSettings =
  sendOwnedMessage neHotspotConfiguration initWithHS20Settings_eapSettingsSelector (toNEHotspotHS20Settings hs20Settings) (toNEHotspotEAPSettings eapSettings)

-- | initWithSSIDPrefix:
--
-- A designated initializer to instantiate a new NEHotspotConfiguration object.   This initializer is used to configure open Wi-Fi Networks.
--
-- @SSIDPrefix@ — The prefix string of SSID of the open Wi-Fi Network.   Length of SSIDPrefix must be between 3 and 32 characters.
--
-- ObjC selector: @- initWithSSIDPrefix:@
initWithSSIDPrefix :: (IsNEHotspotConfiguration neHotspotConfiguration, IsNSString ssidPrefix) => neHotspotConfiguration -> ssidPrefix -> IO (Id NEHotspotConfiguration)
initWithSSIDPrefix neHotspotConfiguration ssidPrefix =
  sendOwnedMessage neHotspotConfiguration initWithSSIDPrefixSelector (toNSString ssidPrefix)

-- | initWithSSIDPrefix:passphrase:isWEP
--
-- A designated initializer to instantiate a new NEHotspotConfiguration object.   This initializer is used configure either WEP or WPA/WPA2 Personal Wi-Fi Networks.
--
-- @SSIDPrefix@ — The prefix string of SSID of the WEP or WPA/WPA2 Personal Wi-Fi Network. 	Length of SSIDPrefix must be between 3 and 32 characters.
--
-- @passphrase@ — The passphrase credential.   For WPA/WPA2 Personal networks: between 8 and 63 characters.   For Static WEP(64bit)  : 10 Hex Digits   For Static WEP(128bit) : 26 Hex Digits
--
-- @isWEP@ — YES specifies WEP Wi-Fi Network else WPA/WPA2 Personal Wi-Fi Network
--
-- ObjC selector: @- initWithSSIDPrefix:passphrase:isWEP:@
initWithSSIDPrefix_passphrase_isWEP :: (IsNEHotspotConfiguration neHotspotConfiguration, IsNSString ssidPrefix, IsNSString passphrase) => neHotspotConfiguration -> ssidPrefix -> passphrase -> Bool -> IO (Id NEHotspotConfiguration)
initWithSSIDPrefix_passphrase_isWEP neHotspotConfiguration ssidPrefix passphrase isWEP =
  sendOwnedMessage neHotspotConfiguration initWithSSIDPrefix_passphrase_isWEPSelector (toNSString ssidPrefix) (toNSString passphrase) isWEP

-- | SSID
--
-- SSID of the Wi-Fi Network.
--
-- ObjC selector: @- SSID@
ssid :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> IO (Id NSString)
ssid neHotspotConfiguration =
  sendMessage neHotspotConfiguration ssidSelector

-- | SSIDPrefix
--
-- Prefix string of SSID of the Wi-Fi Network.
--
-- ObjC selector: @- SSIDPrefix@
ssidPrefix :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> IO (Id NSString)
ssidPrefix neHotspotConfiguration =
  sendMessage neHotspotConfiguration ssidPrefixSelector

-- | joinOnce
--
-- if set to YES the configuration will not be persisted. Default is NO.
--
-- ObjC selector: @- joinOnce@
joinOnce :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> IO Bool
joinOnce neHotspotConfiguration =
  sendMessage neHotspotConfiguration joinOnceSelector

-- | joinOnce
--
-- if set to YES the configuration will not be persisted. Default is NO.
--
-- ObjC selector: @- setJoinOnce:@
setJoinOnce :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> Bool -> IO ()
setJoinOnce neHotspotConfiguration value =
  sendMessage neHotspotConfiguration setJoinOnceSelector value

-- | lifeTimeInDays
--
-- The lifetime of the configuration in days. The configuration is stored for the   number of days specified by this property. The minimum value is 1 day and maximum value is 365 days.   A configuration does not get deleted automatically if this property is not set or set to an invalid value.   This property does not apply to Enterprise and HS2.0 networks.
--
-- ObjC selector: @- lifeTimeInDays@
lifeTimeInDays :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> IO (Id NSNumber)
lifeTimeInDays neHotspotConfiguration =
  sendMessage neHotspotConfiguration lifeTimeInDaysSelector

-- | lifeTimeInDays
--
-- The lifetime of the configuration in days. The configuration is stored for the   number of days specified by this property. The minimum value is 1 day and maximum value is 365 days.   A configuration does not get deleted automatically if this property is not set or set to an invalid value.   This property does not apply to Enterprise and HS2.0 networks.
--
-- ObjC selector: @- setLifeTimeInDays:@
setLifeTimeInDays :: (IsNEHotspotConfiguration neHotspotConfiguration, IsNSNumber value) => neHotspotConfiguration -> value -> IO ()
setLifeTimeInDays neHotspotConfiguration value =
  sendMessage neHotspotConfiguration setLifeTimeInDaysSelector (toNSNumber value)

-- | hidden
--
-- if set to YES the system will perform active scan of the SSID. Default is NO.
--
-- ObjC selector: @- hidden@
hidden :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> IO Bool
hidden neHotspotConfiguration =
  sendMessage neHotspotConfiguration hiddenSelector

-- | hidden
--
-- if set to YES the system will perform active scan of the SSID. Default is NO.
--
-- ObjC selector: @- setHidden:@
setHidden :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> Bool -> IO ()
setHidden neHotspotConfiguration value =
  sendMessage neHotspotConfiguration setHiddenSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSSID:@
initWithSSIDSelector :: Selector '[Id NSString] (Id NEHotspotConfiguration)
initWithSSIDSelector = mkSelector "initWithSSID:"

-- | @Selector@ for @initWithSSID:passphrase:isWEP:@
initWithSSID_passphrase_isWEPSelector :: Selector '[Id NSString, Id NSString, Bool] (Id NEHotspotConfiguration)
initWithSSID_passphrase_isWEPSelector = mkSelector "initWithSSID:passphrase:isWEP:"

-- | @Selector@ for @initWithSSID:eapSettings:@
initWithSSID_eapSettingsSelector :: Selector '[Id NSString, Id NEHotspotEAPSettings] (Id NEHotspotConfiguration)
initWithSSID_eapSettingsSelector = mkSelector "initWithSSID:eapSettings:"

-- | @Selector@ for @initWithHS20Settings:eapSettings:@
initWithHS20Settings_eapSettingsSelector :: Selector '[Id NEHotspotHS20Settings, Id NEHotspotEAPSettings] (Id NEHotspotConfiguration)
initWithHS20Settings_eapSettingsSelector = mkSelector "initWithHS20Settings:eapSettings:"

-- | @Selector@ for @initWithSSIDPrefix:@
initWithSSIDPrefixSelector :: Selector '[Id NSString] (Id NEHotspotConfiguration)
initWithSSIDPrefixSelector = mkSelector "initWithSSIDPrefix:"

-- | @Selector@ for @initWithSSIDPrefix:passphrase:isWEP:@
initWithSSIDPrefix_passphrase_isWEPSelector :: Selector '[Id NSString, Id NSString, Bool] (Id NEHotspotConfiguration)
initWithSSIDPrefix_passphrase_isWEPSelector = mkSelector "initWithSSIDPrefix:passphrase:isWEP:"

-- | @Selector@ for @SSID@
ssidSelector :: Selector '[] (Id NSString)
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @SSIDPrefix@
ssidPrefixSelector :: Selector '[] (Id NSString)
ssidPrefixSelector = mkSelector "SSIDPrefix"

-- | @Selector@ for @joinOnce@
joinOnceSelector :: Selector '[] Bool
joinOnceSelector = mkSelector "joinOnce"

-- | @Selector@ for @setJoinOnce:@
setJoinOnceSelector :: Selector '[Bool] ()
setJoinOnceSelector = mkSelector "setJoinOnce:"

-- | @Selector@ for @lifeTimeInDays@
lifeTimeInDaysSelector :: Selector '[] (Id NSNumber)
lifeTimeInDaysSelector = mkSelector "lifeTimeInDays"

-- | @Selector@ for @setLifeTimeInDays:@
setLifeTimeInDaysSelector :: Selector '[Id NSNumber] ()
setLifeTimeInDaysSelector = mkSelector "setLifeTimeInDays:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

