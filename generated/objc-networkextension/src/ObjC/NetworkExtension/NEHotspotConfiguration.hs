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
  , initWithSSIDSelector
  , initWithSSID_passphrase_isWEPSelector
  , initWithSSID_eapSettingsSelector
  , initWithHS20Settings_eapSettingsSelector
  , initWithSSIDPrefixSelector
  , initWithSSIDPrefix_passphrase_isWEPSelector
  , ssidSelector
  , ssidPrefixSelector
  , joinOnceSelector
  , setJoinOnceSelector
  , lifeTimeInDaysSelector
  , setLifeTimeInDaysSelector
  , hiddenSelector
  , setHiddenSelector


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

-- | initWithSSID:
--
-- A designated initializer to instantiate a new NEHotspotConfiguration object.   This initializer is used to configure open Wi-Fi Networks.
--
-- @SSID@ — The SSID of the open Wi-Fi Network.   Length of SSID must be between 1 and 32 characters.
--
-- ObjC selector: @- initWithSSID:@
initWithSSID :: (IsNEHotspotConfiguration neHotspotConfiguration, IsNSString ssid) => neHotspotConfiguration -> ssid -> IO (Id NEHotspotConfiguration)
initWithSSID neHotspotConfiguration  ssid =
withObjCPtr ssid $ \raw_ssid ->
    sendMsg neHotspotConfiguration (mkSelector "initWithSSID:") (retPtr retVoid) [argPtr (castPtr raw_ssid :: Ptr ())] >>= ownedObject . castPtr

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
initWithSSID_passphrase_isWEP neHotspotConfiguration  ssid passphrase isWEP =
withObjCPtr ssid $ \raw_ssid ->
  withObjCPtr passphrase $ \raw_passphrase ->
      sendMsg neHotspotConfiguration (mkSelector "initWithSSID:passphrase:isWEP:") (retPtr retVoid) [argPtr (castPtr raw_ssid :: Ptr ()), argPtr (castPtr raw_passphrase :: Ptr ()), argCULong (if isWEP then 1 else 0)] >>= ownedObject . castPtr

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
initWithSSID_eapSettings neHotspotConfiguration  ssid eapSettings =
withObjCPtr ssid $ \raw_ssid ->
  withObjCPtr eapSettings $ \raw_eapSettings ->
      sendMsg neHotspotConfiguration (mkSelector "initWithSSID:eapSettings:") (retPtr retVoid) [argPtr (castPtr raw_ssid :: Ptr ()), argPtr (castPtr raw_eapSettings :: Ptr ())] >>= ownedObject . castPtr

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
initWithHS20Settings_eapSettings neHotspotConfiguration  hs20Settings eapSettings =
withObjCPtr hs20Settings $ \raw_hs20Settings ->
  withObjCPtr eapSettings $ \raw_eapSettings ->
      sendMsg neHotspotConfiguration (mkSelector "initWithHS20Settings:eapSettings:") (retPtr retVoid) [argPtr (castPtr raw_hs20Settings :: Ptr ()), argPtr (castPtr raw_eapSettings :: Ptr ())] >>= ownedObject . castPtr

-- | initWithSSIDPrefix:
--
-- A designated initializer to instantiate a new NEHotspotConfiguration object.   This initializer is used to configure open Wi-Fi Networks.
--
-- @SSIDPrefix@ — The prefix string of SSID of the open Wi-Fi Network.   Length of SSIDPrefix must be between 3 and 32 characters.
--
-- ObjC selector: @- initWithSSIDPrefix:@
initWithSSIDPrefix :: (IsNEHotspotConfiguration neHotspotConfiguration, IsNSString ssidPrefix) => neHotspotConfiguration -> ssidPrefix -> IO (Id NEHotspotConfiguration)
initWithSSIDPrefix neHotspotConfiguration  ssidPrefix =
withObjCPtr ssidPrefix $ \raw_ssidPrefix ->
    sendMsg neHotspotConfiguration (mkSelector "initWithSSIDPrefix:") (retPtr retVoid) [argPtr (castPtr raw_ssidPrefix :: Ptr ())] >>= ownedObject . castPtr

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
initWithSSIDPrefix_passphrase_isWEP neHotspotConfiguration  ssidPrefix passphrase isWEP =
withObjCPtr ssidPrefix $ \raw_ssidPrefix ->
  withObjCPtr passphrase $ \raw_passphrase ->
      sendMsg neHotspotConfiguration (mkSelector "initWithSSIDPrefix:passphrase:isWEP:") (retPtr retVoid) [argPtr (castPtr raw_ssidPrefix :: Ptr ()), argPtr (castPtr raw_passphrase :: Ptr ()), argCULong (if isWEP then 1 else 0)] >>= ownedObject . castPtr

-- | SSID
--
-- SSID of the Wi-Fi Network.
--
-- ObjC selector: @- SSID@
ssid :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> IO (Id NSString)
ssid neHotspotConfiguration  =
  sendMsg neHotspotConfiguration (mkSelector "SSID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | SSIDPrefix
--
-- Prefix string of SSID of the Wi-Fi Network.
--
-- ObjC selector: @- SSIDPrefix@
ssidPrefix :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> IO (Id NSString)
ssidPrefix neHotspotConfiguration  =
  sendMsg neHotspotConfiguration (mkSelector "SSIDPrefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | joinOnce
--
-- if set to YES the configuration will not be persisted. Default is NO.
--
-- ObjC selector: @- joinOnce@
joinOnce :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> IO Bool
joinOnce neHotspotConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neHotspotConfiguration (mkSelector "joinOnce") retCULong []

-- | joinOnce
--
-- if set to YES the configuration will not be persisted. Default is NO.
--
-- ObjC selector: @- setJoinOnce:@
setJoinOnce :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> Bool -> IO ()
setJoinOnce neHotspotConfiguration  value =
  sendMsg neHotspotConfiguration (mkSelector "setJoinOnce:") retVoid [argCULong (if value then 1 else 0)]

-- | lifeTimeInDays
--
-- The lifetime of the configuration in days. The configuration is stored for the   number of days specified by this property. The minimum value is 1 day and maximum value is 365 days.   A configuration does not get deleted automatically if this property is not set or set to an invalid value.   This property does not apply to Enterprise and HS2.0 networks.
--
-- ObjC selector: @- lifeTimeInDays@
lifeTimeInDays :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> IO (Id NSNumber)
lifeTimeInDays neHotspotConfiguration  =
  sendMsg neHotspotConfiguration (mkSelector "lifeTimeInDays") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | lifeTimeInDays
--
-- The lifetime of the configuration in days. The configuration is stored for the   number of days specified by this property. The minimum value is 1 day and maximum value is 365 days.   A configuration does not get deleted automatically if this property is not set or set to an invalid value.   This property does not apply to Enterprise and HS2.0 networks.
--
-- ObjC selector: @- setLifeTimeInDays:@
setLifeTimeInDays :: (IsNEHotspotConfiguration neHotspotConfiguration, IsNSNumber value) => neHotspotConfiguration -> value -> IO ()
setLifeTimeInDays neHotspotConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg neHotspotConfiguration (mkSelector "setLifeTimeInDays:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | hidden
--
-- if set to YES the system will perform active scan of the SSID. Default is NO.
--
-- ObjC selector: @- hidden@
hidden :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> IO Bool
hidden neHotspotConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neHotspotConfiguration (mkSelector "hidden") retCULong []

-- | hidden
--
-- if set to YES the system will perform active scan of the SSID. Default is NO.
--
-- ObjC selector: @- setHidden:@
setHidden :: IsNEHotspotConfiguration neHotspotConfiguration => neHotspotConfiguration -> Bool -> IO ()
setHidden neHotspotConfiguration  value =
  sendMsg neHotspotConfiguration (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSSID:@
initWithSSIDSelector :: Selector
initWithSSIDSelector = mkSelector "initWithSSID:"

-- | @Selector@ for @initWithSSID:passphrase:isWEP:@
initWithSSID_passphrase_isWEPSelector :: Selector
initWithSSID_passphrase_isWEPSelector = mkSelector "initWithSSID:passphrase:isWEP:"

-- | @Selector@ for @initWithSSID:eapSettings:@
initWithSSID_eapSettingsSelector :: Selector
initWithSSID_eapSettingsSelector = mkSelector "initWithSSID:eapSettings:"

-- | @Selector@ for @initWithHS20Settings:eapSettings:@
initWithHS20Settings_eapSettingsSelector :: Selector
initWithHS20Settings_eapSettingsSelector = mkSelector "initWithHS20Settings:eapSettings:"

-- | @Selector@ for @initWithSSIDPrefix:@
initWithSSIDPrefixSelector :: Selector
initWithSSIDPrefixSelector = mkSelector "initWithSSIDPrefix:"

-- | @Selector@ for @initWithSSIDPrefix:passphrase:isWEP:@
initWithSSIDPrefix_passphrase_isWEPSelector :: Selector
initWithSSIDPrefix_passphrase_isWEPSelector = mkSelector "initWithSSIDPrefix:passphrase:isWEP:"

-- | @Selector@ for @SSID@
ssidSelector :: Selector
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @SSIDPrefix@
ssidPrefixSelector :: Selector
ssidPrefixSelector = mkSelector "SSIDPrefix"

-- | @Selector@ for @joinOnce@
joinOnceSelector :: Selector
joinOnceSelector = mkSelector "joinOnce"

-- | @Selector@ for @setJoinOnce:@
setJoinOnceSelector :: Selector
setJoinOnceSelector = mkSelector "setJoinOnce:"

-- | @Selector@ for @lifeTimeInDays@
lifeTimeInDaysSelector :: Selector
lifeTimeInDaysSelector = mkSelector "lifeTimeInDays"

-- | @Selector@ for @setLifeTimeInDays:@
setLifeTimeInDaysSelector :: Selector
setLifeTimeInDaysSelector = mkSelector "setLifeTimeInDays:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

