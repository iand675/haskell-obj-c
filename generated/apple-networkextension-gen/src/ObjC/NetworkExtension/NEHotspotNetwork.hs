{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEHotspotNetwork
--
-- The NEHotspotNetwork class provides a class method to get the SSID and BSSID of   the current Wi-Fi network.
--
-- NEHotspotNetwork is part of NetworkExtension.framework
--
-- Generated bindings for @NEHotspotNetwork@.
module ObjC.NetworkExtension.NEHotspotNetwork
  ( NEHotspotNetwork
  , IsNEHotspotNetwork(..)
  , fetchCurrentWithCompletionHandler
  , setConfidence
  , setPassword
  , ssid
  , bssid
  , securityType
  , signalStrength
  , secure
  , autoJoined
  , justJoined
  , chosenHelper
  , autoJoinedSelector
  , bssidSelector
  , chosenHelperSelector
  , fetchCurrentWithCompletionHandlerSelector
  , justJoinedSelector
  , secureSelector
  , securityTypeSelector
  , setConfidenceSelector
  , setPasswordSelector
  , signalStrengthSelector
  , ssidSelector

  -- * Enum types
  , NEHotspotHelperConfidence(NEHotspotHelperConfidence)
  , pattern KNEHotspotHelperConfidenceNone
  , pattern KNEHotspotHelperConfidenceLow
  , pattern KNEHotspotHelperConfidenceHigh
  , NEHotspotNetworkSecurityType(NEHotspotNetworkSecurityType)
  , pattern NEHotspotNetworkSecurityTypeOpen
  , pattern NEHotspotNetworkSecurityTypeWEP
  , pattern NEHotspotNetworkSecurityTypePersonal
  , pattern NEHotspotNetworkSecurityTypeEnterprise
  , pattern NEHotspotNetworkSecurityTypeUnknown

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | fetchCurrentWithCompletionHandler:completionHandler:
--
-- This method returns SSID, BSSID and security type of the current Wi-Fi network when the   requesting application meets one of following 4 requirements -.   1. application is using CoreLocation API and has user's authorization to access precise location.   2. application has used NEHotspotConfiguration API to configure the current Wi-Fi network.   3. application has active VPN configurations installed.   4. application has active NEDNSSettingsManager configuration installed.   An application will receive nil if it fails to meet any of the above 4 requirements.   An application will receive nil if does not have the "com.apple.developer.networking.wifi-info" entitlement.
--
-- @completionHandler@ — A block that will be executed when current Wi-Fi network details are   obtained from the system. The NEHotspotNetwork object passed to this block will be nil if the requesting   application fails to meet above requirements, non-nil otherwise. NEHotspotNetwork object contains only valid   SSID, BSSID and security type values, when the block is passed non-nil object.This block is executed on application's   main queue.
--
-- ObjC selector: @+ fetchCurrentWithCompletionHandler:@
fetchCurrentWithCompletionHandler :: Ptr () -> IO ()
fetchCurrentWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "NEHotspotNetwork"
    sendClassMessage cls' fetchCurrentWithCompletionHandlerSelector completionHandler

-- | setConfidence
--
-- Indicate the confidence in being able to handle the network.
--
-- Use this method to indicate the confidence in being able to   successfully authenticate to the given network. Used in the response   to the kNEHotspotHelperCommandTypeEvaluate and   kNEHotspotHelperCommandTypeFilterScanList commands.
--
-- ObjC selector: @- setConfidence:@
setConfidence :: IsNEHotspotNetwork neHotspotNetwork => neHotspotNetwork -> NEHotspotHelperConfidence -> IO ()
setConfidence neHotspotNetwork confidence =
  sendMessage neHotspotNetwork setConfidenceSelector confidence

-- | setPassword
--
-- Provide the password for a secure network
--
-- The HotspotHelper may set a password for a secure network. The format   password string must adhere to IEEE 802.11 guidelines appropriate for   the particular security scheme.
--
-- Used only in the response to the kNEHotspotHelperCommandTypeFilterScanList   command.
--
-- ObjC selector: @- setPassword:@
setPassword :: (IsNEHotspotNetwork neHotspotNetwork, IsNSString password) => neHotspotNetwork -> password -> IO ()
setPassword neHotspotNetwork password =
  sendMessage neHotspotNetwork setPasswordSelector (toNSString password)

-- | SSID
--
-- The SSID of the Wi-Fi network.
--
-- ObjC selector: @- SSID@
ssid :: IsNEHotspotNetwork neHotspotNetwork => neHotspotNetwork -> IO (Id NSString)
ssid neHotspotNetwork =
  sendMessage neHotspotNetwork ssidSelector

-- | BSSID
--
-- The BSSID of the Wi-Fi network.
--
-- ObjC selector: @- BSSID@
bssid :: IsNEHotspotNetwork neHotspotNetwork => neHotspotNetwork -> IO (Id NSString)
bssid neHotspotNetwork =
  sendMessage neHotspotNetwork bssidSelector

-- | securityType
--
-- The security type of the Wi-Fi network.
--
-- ObjC selector: @- securityType@
securityType :: IsNEHotspotNetwork neHotspotNetwork => neHotspotNetwork -> IO NEHotspotNetworkSecurityType
securityType neHotspotNetwork =
  sendMessage neHotspotNetwork securityTypeSelector

-- | signalStrength
--
-- The signal strength for the Wi-Fi network. The value lies within   the range 0.0 (weak/no signal) to 1.0 (strong signal).
--
-- ObjC selector: @- signalStrength@
signalStrength :: IsNEHotspotNetwork neHotspotNetwork => neHotspotNetwork -> IO CDouble
signalStrength neHotspotNetwork =
  sendMessage neHotspotNetwork signalStrengthSelector

-- | secure
--
-- Indicates whether the network is secure
--
-- ObjC selector: @- secure@
secure :: IsNEHotspotNetwork neHotspotNetwork => neHotspotNetwork -> IO Bool
secure neHotspotNetwork =
  sendMessage neHotspotNetwork secureSelector

-- | autoJoined
--
-- Indicates whether the network was joined automatically   (YES) or joined by the user (NO).
--
-- ObjC selector: @- autoJoined@
autoJoined :: IsNEHotspotNetwork neHotspotNetwork => neHotspotNetwork -> IO Bool
autoJoined neHotspotNetwork =
  sendMessage neHotspotNetwork autoJoinedSelector

-- | justJoined
--
-- Indicates whether the network was just joined. Useful in the   Maintaining state to differentiate whether the Maintain command   is for the initial join, or the subsequent periodic callback.
--
-- ObjC selector: @- justJoined@
justJoined :: IsNEHotspotNetwork neHotspotNetwork => neHotspotNetwork -> IO Bool
justJoined neHotspotNetwork =
  sendMessage neHotspotNetwork justJoinedSelector

-- | chosenHelper
--
-- Indicates whether the HotspotHelper is the chosen helper for   the network. The NEHotspotNetwork must have been instantiated via a   call to the +[NEHotspotHelper supportedNetworkInterfaces] method. This   is useful to restore state after the HotspotHelper application is quit   and restarted.
--
-- ObjC selector: @- chosenHelper@
chosenHelper :: IsNEHotspotNetwork neHotspotNetwork => neHotspotNetwork -> IO Bool
chosenHelper neHotspotNetwork =
  sendMessage neHotspotNetwork chosenHelperSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchCurrentWithCompletionHandler:@
fetchCurrentWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
fetchCurrentWithCompletionHandlerSelector = mkSelector "fetchCurrentWithCompletionHandler:"

-- | @Selector@ for @setConfidence:@
setConfidenceSelector :: Selector '[NEHotspotHelperConfidence] ()
setConfidenceSelector = mkSelector "setConfidence:"

-- | @Selector@ for @setPassword:@
setPasswordSelector :: Selector '[Id NSString] ()
setPasswordSelector = mkSelector "setPassword:"

-- | @Selector@ for @SSID@
ssidSelector :: Selector '[] (Id NSString)
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @BSSID@
bssidSelector :: Selector '[] (Id NSString)
bssidSelector = mkSelector "BSSID"

-- | @Selector@ for @securityType@
securityTypeSelector :: Selector '[] NEHotspotNetworkSecurityType
securityTypeSelector = mkSelector "securityType"

-- | @Selector@ for @signalStrength@
signalStrengthSelector :: Selector '[] CDouble
signalStrengthSelector = mkSelector "signalStrength"

-- | @Selector@ for @secure@
secureSelector :: Selector '[] Bool
secureSelector = mkSelector "secure"

-- | @Selector@ for @autoJoined@
autoJoinedSelector :: Selector '[] Bool
autoJoinedSelector = mkSelector "autoJoined"

-- | @Selector@ for @justJoined@
justJoinedSelector :: Selector '[] Bool
justJoinedSelector = mkSelector "justJoined"

-- | @Selector@ for @chosenHelper@
chosenHelperSelector :: Selector '[] Bool
chosenHelperSelector = mkSelector "chosenHelper"

