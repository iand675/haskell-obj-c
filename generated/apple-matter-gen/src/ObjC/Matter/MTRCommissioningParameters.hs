{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information that can be provided to commissionWithNodeID to commision devices.
--
-- Generated bindings for @MTRCommissioningParameters@.
module ObjC.Matter.MTRCommissioningParameters
  ( MTRCommissioningParameters
  , IsMTRCommissioningParameters(..)
  , csrNonce
  , setCsrNonce
  , attestationNonce
  , setAttestationNonce
  , wifiSSID
  , setWifiSSID
  , wifiCredentials
  , setWifiCredentials
  , threadOperationalDataset
  , setThreadOperationalDataset
  , deviceAttestationDelegate
  , setDeviceAttestationDelegate
  , failSafeTimeout
  , setFailSafeTimeout
  , skipCommissioningComplete
  , setSkipCommissioningComplete
  , countryCode
  , setCountryCode
  , readEndpointInformation
  , setReadEndpointInformation
  , acceptedTermsAndConditions
  , setAcceptedTermsAndConditions
  , acceptedTermsAndConditionsVersion
  , setAcceptedTermsAndConditionsVersion
  , extraAttributesToRead
  , setExtraAttributesToRead
  , forceWiFiScan
  , setForceWiFiScan
  , forceThreadScan
  , setForceThreadScan
  , setCSRNonce
  , failSafeExpiryTimeoutSecs
  , setFailSafeExpiryTimeoutSecs
  , acceptedTermsAndConditionsSelector
  , acceptedTermsAndConditionsVersionSelector
  , attestationNonceSelector
  , countryCodeSelector
  , csrNonceSelector
  , deviceAttestationDelegateSelector
  , extraAttributesToReadSelector
  , failSafeExpiryTimeoutSecsSelector
  , failSafeTimeoutSelector
  , forceThreadScanSelector
  , forceWiFiScanSelector
  , readEndpointInformationSelector
  , setAcceptedTermsAndConditionsSelector
  , setAcceptedTermsAndConditionsVersionSelector
  , setAttestationNonceSelector
  , setCSRNonceSelector
  , setCountryCodeSelector
  , setCsrNonceSelector
  , setDeviceAttestationDelegateSelector
  , setExtraAttributesToReadSelector
  , setFailSafeExpiryTimeoutSecsSelector
  , setFailSafeTimeoutSelector
  , setForceThreadScanSelector
  , setForceWiFiScanSelector
  , setReadEndpointInformationSelector
  , setSkipCommissioningCompleteSelector
  , setThreadOperationalDatasetSelector
  , setWifiCredentialsSelector
  , setWifiSSIDSelector
  , skipCommissioningCompleteSelector
  , threadOperationalDatasetSelector
  , wifiCredentialsSelector
  , wifiSSIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The nonce to use when requesting a CSR for the node's operational certificate.
--
-- If nil, a random nonce will be generated automatically.
--
-- If not nil, must be 32 bytes of data.
--
-- ObjC selector: @- csrNonce@
csrNonce :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSData)
csrNonce mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters csrNonceSelector

-- | The nonce to use when requesting a CSR for the node's operational certificate.
--
-- If nil, a random nonce will be generated automatically.
--
-- If not nil, must be 32 bytes of data.
--
-- ObjC selector: @- setCsrNonce:@
setCsrNonce :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setCsrNonce mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setCsrNonceSelector (toNSData value)

-- | The nonce to use when requesting attestation information from the device.
--
-- If nil, a random nonce will be generated automatically.
--
-- If not nil, must be 32 bytes of data.
--
-- ObjC selector: @- attestationNonce@
attestationNonce :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSData)
attestationNonce mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters attestationNonceSelector

-- | The nonce to use when requesting attestation information from the device.
--
-- If nil, a random nonce will be generated automatically.
--
-- If not nil, must be 32 bytes of data.
--
-- ObjC selector: @- setAttestationNonce:@
setAttestationNonce :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setAttestationNonce mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setAttestationNonceSelector (toNSData value)

-- | The Wi-Fi SSID, if available.
--
-- ObjC selector: @- wifiSSID@
wifiSSID :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSData)
wifiSSID mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters wifiSSIDSelector

-- | The Wi-Fi SSID, if available.
--
-- ObjC selector: @- setWifiSSID:@
setWifiSSID :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setWifiSSID mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setWifiSSIDSelector (toNSData value)

-- | The Wi-Fi Credentials.  Allowed to be nil or 0-length data for an open network, as long as wifiSSID is not nil.
--
-- ObjC selector: @- wifiCredentials@
wifiCredentials :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSData)
wifiCredentials mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters wifiCredentialsSelector

-- | The Wi-Fi Credentials.  Allowed to be nil or 0-length data for an open network, as long as wifiSSID is not nil.
--
-- ObjC selector: @- setWifiCredentials:@
setWifiCredentials :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setWifiCredentials mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setWifiCredentialsSelector (toNSData value)

-- | The Thread operational dataset, if available.
--
-- ObjC selector: @- threadOperationalDataset@
threadOperationalDataset :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSData)
threadOperationalDataset mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters threadOperationalDatasetSelector

-- | The Thread operational dataset, if available.
--
-- ObjC selector: @- setThreadOperationalDataset:@
setThreadOperationalDataset :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setThreadOperationalDataset mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setThreadOperationalDatasetSelector (toNSData value)

-- | An optional delegate that can be notified upon completion of device attestation.  See documentation for MTRDeviceAttestationDelegate for details.
--
-- The delegate methods will be invoked on an arbitrary thread.
--
-- ObjC selector: @- deviceAttestationDelegate@
deviceAttestationDelegate :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO RawId
deviceAttestationDelegate mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters deviceAttestationDelegateSelector

-- | An optional delegate that can be notified upon completion of device attestation.  See documentation for MTRDeviceAttestationDelegate for details.
--
-- The delegate methods will be invoked on an arbitrary thread.
--
-- ObjC selector: @- setDeviceAttestationDelegate:@
setDeviceAttestationDelegate :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> RawId -> IO ()
setDeviceAttestationDelegate mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setDeviceAttestationDelegateSelector value

-- | The timeout, in seconds, to set for the fail-safe when calling into the deviceAttestationDelegate and waiting for it to respond.
--
-- If nil, the fail-safe will not be extended before calling into the deviceAttestationDelegate.
--
-- ObjC selector: @- failSafeTimeout@
failSafeTimeout :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSNumber)
failSafeTimeout mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters failSafeTimeoutSelector

-- | The timeout, in seconds, to set for the fail-safe when calling into the deviceAttestationDelegate and waiting for it to respond.
--
-- If nil, the fail-safe will not be extended before calling into the deviceAttestationDelegate.
--
-- ObjC selector: @- setFailSafeTimeout:@
setFailSafeTimeout :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSNumber value) => mtrCommissioningParameters -> value -> IO ()
setFailSafeTimeout mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setFailSafeTimeoutSelector (toNSNumber value)

-- | Only perform the PASE steps of commissioning. If set to YES, commissioning will be completed by another admin on the network.
--
-- Defaults to NO.
--
-- ObjC selector: @- skipCommissioningComplete@
skipCommissioningComplete :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO Bool
skipCommissioningComplete mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters skipCommissioningCompleteSelector

-- | Only perform the PASE steps of commissioning. If set to YES, commissioning will be completed by another admin on the network.
--
-- Defaults to NO.
--
-- ObjC selector: @- setSkipCommissioningComplete:@
setSkipCommissioningComplete :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> Bool -> IO ()
setSkipCommissioningComplete mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setSkipCommissioningCompleteSelector value

-- | The country code to provide to the device during commissioning.
--
-- If not nil, this must be a 2-character ISO 3166-1 country code, which the device can use to decide on things like radio communications bands.
--
-- ObjC selector: @- countryCode@
countryCode :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSString)
countryCode mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters countryCodeSelector

-- | The country code to provide to the device during commissioning.
--
-- If not nil, this must be a 2-character ISO 3166-1 country code, which the device can use to decide on things like radio communications bands.
--
-- ObjC selector: @- setCountryCode:@
setCountryCode :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSString value) => mtrCommissioningParameters -> value -> IO ()
setCountryCode mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setCountryCodeSelector (toNSString value)

-- | Read device type information from all endpoints during commissioning. Defaults to NO.
--
-- ObjC selector: @- readEndpointInformation@
readEndpointInformation :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO Bool
readEndpointInformation mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters readEndpointInformationSelector

-- | Read device type information from all endpoints during commissioning. Defaults to NO.
--
-- ObjC selector: @- setReadEndpointInformation:@
setReadEndpointInformation :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> Bool -> IO ()
setReadEndpointInformation mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setReadEndpointInformationSelector value

-- | A bitmask of the user’s responses to the presented terms and conditions. Each bit corresponds to a term’s acceptance (1) or non-acceptance (0) at the matching index.
--
-- ObjC selector: @- acceptedTermsAndConditions@
acceptedTermsAndConditions :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSNumber)
acceptedTermsAndConditions mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters acceptedTermsAndConditionsSelector

-- | A bitmask of the user’s responses to the presented terms and conditions. Each bit corresponds to a term’s acceptance (1) or non-acceptance (0) at the matching index.
--
-- ObjC selector: @- setAcceptedTermsAndConditions:@
setAcceptedTermsAndConditions :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSNumber value) => mtrCommissioningParameters -> value -> IO ()
setAcceptedTermsAndConditions mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setAcceptedTermsAndConditionsSelector (toNSNumber value)

-- | The version of the terms and conditions that the user has accepted.
--
-- ObjC selector: @- acceptedTermsAndConditionsVersion@
acceptedTermsAndConditionsVersion :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSNumber)
acceptedTermsAndConditionsVersion mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters acceptedTermsAndConditionsVersionSelector

-- | The version of the terms and conditions that the user has accepted.
--
-- ObjC selector: @- setAcceptedTermsAndConditionsVersion:@
setAcceptedTermsAndConditionsVersion :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSNumber value) => mtrCommissioningParameters -> value -> IO ()
setAcceptedTermsAndConditionsVersion mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setAcceptedTermsAndConditionsVersionSelector (toNSNumber value)

-- | List of attribute paths to read from the commissionee (in addition to whatever attributes are already read to handle readEndpointInformation being YES, or to handle other commissioning tasks).
--
-- The FeatureMap attribute of all Network Commissioning clusters on the commissionee will always be read and does not need to be included in this list.
--
-- ObjC selector: @- extraAttributesToRead@
extraAttributesToRead :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSArray)
extraAttributesToRead mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters extraAttributesToReadSelector

-- | List of attribute paths to read from the commissionee (in addition to whatever attributes are already read to handle readEndpointInformation being YES, or to handle other commissioning tasks).
--
-- The FeatureMap attribute of all Network Commissioning clusters on the commissionee will always be read and does not need to be included in this list.
--
-- ObjC selector: @- setExtraAttributesToRead:@
setExtraAttributesToRead :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSArray value) => mtrCommissioningParameters -> value -> IO ()
setExtraAttributesToRead mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setExtraAttributesToReadSelector (toNSArray value)

-- | Whether to force a network scan before requesting Wi-Fi credentials. The default is NO.
--
-- Even if this value is NO a scan may still be performed.
--
-- This value will be ignored if Wi-Fi credentials are provided or not needed.
--
-- NOTE: Not all APIs that take MTRCommissioningParameters pay attention to this flag.
--
-- ObjC selector: @- forceWiFiScan@
forceWiFiScan :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO Bool
forceWiFiScan mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters forceWiFiScanSelector

-- | Whether to force a network scan before requesting Wi-Fi credentials. The default is NO.
--
-- Even if this value is NO a scan may still be performed.
--
-- This value will be ignored if Wi-Fi credentials are provided or not needed.
--
-- NOTE: Not all APIs that take MTRCommissioningParameters pay attention to this flag.
--
-- ObjC selector: @- setForceWiFiScan:@
setForceWiFiScan :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> Bool -> IO ()
setForceWiFiScan mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setForceWiFiScanSelector value

-- | Whether to force a network scan before requesting Thread credentials. The default is NO.
--
-- Even if this value is NO a scan may still be performed.
--
-- This value will be ignored if a Thread operational dataset is provided or not needed.
--
-- NOTE: Not all APIs that take MTRCommissioningParameters pay attention to this flag.
--
-- ObjC selector: @- forceThreadScan@
forceThreadScan :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO Bool
forceThreadScan mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters forceThreadScanSelector

-- | Whether to force a network scan before requesting Thread credentials. The default is NO.
--
-- Even if this value is NO a scan may still be performed.
--
-- This value will be ignored if a Thread operational dataset is provided or not needed.
--
-- NOTE: Not all APIs that take MTRCommissioningParameters pay attention to this flag.
--
-- ObjC selector: @- setForceThreadScan:@
setForceThreadScan :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> Bool -> IO ()
setForceThreadScan mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setForceThreadScanSelector value

-- | @- setCSRNonce:@
setCSRNonce :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setCSRNonce mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setCSRNonceSelector (toNSData value)

-- | @- failSafeExpiryTimeoutSecs@
failSafeExpiryTimeoutSecs :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSNumber)
failSafeExpiryTimeoutSecs mtrCommissioningParameters =
  sendMessage mtrCommissioningParameters failSafeExpiryTimeoutSecsSelector

-- | @- setFailSafeExpiryTimeoutSecs:@
setFailSafeExpiryTimeoutSecs :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSNumber value) => mtrCommissioningParameters -> value -> IO ()
setFailSafeExpiryTimeoutSecs mtrCommissioningParameters value =
  sendMessage mtrCommissioningParameters setFailSafeExpiryTimeoutSecsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @csrNonce@
csrNonceSelector :: Selector '[] (Id NSData)
csrNonceSelector = mkSelector "csrNonce"

-- | @Selector@ for @setCsrNonce:@
setCsrNonceSelector :: Selector '[Id NSData] ()
setCsrNonceSelector = mkSelector "setCsrNonce:"

-- | @Selector@ for @attestationNonce@
attestationNonceSelector :: Selector '[] (Id NSData)
attestationNonceSelector = mkSelector "attestationNonce"

-- | @Selector@ for @setAttestationNonce:@
setAttestationNonceSelector :: Selector '[Id NSData] ()
setAttestationNonceSelector = mkSelector "setAttestationNonce:"

-- | @Selector@ for @wifiSSID@
wifiSSIDSelector :: Selector '[] (Id NSData)
wifiSSIDSelector = mkSelector "wifiSSID"

-- | @Selector@ for @setWifiSSID:@
setWifiSSIDSelector :: Selector '[Id NSData] ()
setWifiSSIDSelector = mkSelector "setWifiSSID:"

-- | @Selector@ for @wifiCredentials@
wifiCredentialsSelector :: Selector '[] (Id NSData)
wifiCredentialsSelector = mkSelector "wifiCredentials"

-- | @Selector@ for @setWifiCredentials:@
setWifiCredentialsSelector :: Selector '[Id NSData] ()
setWifiCredentialsSelector = mkSelector "setWifiCredentials:"

-- | @Selector@ for @threadOperationalDataset@
threadOperationalDatasetSelector :: Selector '[] (Id NSData)
threadOperationalDatasetSelector = mkSelector "threadOperationalDataset"

-- | @Selector@ for @setThreadOperationalDataset:@
setThreadOperationalDatasetSelector :: Selector '[Id NSData] ()
setThreadOperationalDatasetSelector = mkSelector "setThreadOperationalDataset:"

-- | @Selector@ for @deviceAttestationDelegate@
deviceAttestationDelegateSelector :: Selector '[] RawId
deviceAttestationDelegateSelector = mkSelector "deviceAttestationDelegate"

-- | @Selector@ for @setDeviceAttestationDelegate:@
setDeviceAttestationDelegateSelector :: Selector '[RawId] ()
setDeviceAttestationDelegateSelector = mkSelector "setDeviceAttestationDelegate:"

-- | @Selector@ for @failSafeTimeout@
failSafeTimeoutSelector :: Selector '[] (Id NSNumber)
failSafeTimeoutSelector = mkSelector "failSafeTimeout"

-- | @Selector@ for @setFailSafeTimeout:@
setFailSafeTimeoutSelector :: Selector '[Id NSNumber] ()
setFailSafeTimeoutSelector = mkSelector "setFailSafeTimeout:"

-- | @Selector@ for @skipCommissioningComplete@
skipCommissioningCompleteSelector :: Selector '[] Bool
skipCommissioningCompleteSelector = mkSelector "skipCommissioningComplete"

-- | @Selector@ for @setSkipCommissioningComplete:@
setSkipCommissioningCompleteSelector :: Selector '[Bool] ()
setSkipCommissioningCompleteSelector = mkSelector "setSkipCommissioningComplete:"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector '[] (Id NSString)
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @setCountryCode:@
setCountryCodeSelector :: Selector '[Id NSString] ()
setCountryCodeSelector = mkSelector "setCountryCode:"

-- | @Selector@ for @readEndpointInformation@
readEndpointInformationSelector :: Selector '[] Bool
readEndpointInformationSelector = mkSelector "readEndpointInformation"

-- | @Selector@ for @setReadEndpointInformation:@
setReadEndpointInformationSelector :: Selector '[Bool] ()
setReadEndpointInformationSelector = mkSelector "setReadEndpointInformation:"

-- | @Selector@ for @acceptedTermsAndConditions@
acceptedTermsAndConditionsSelector :: Selector '[] (Id NSNumber)
acceptedTermsAndConditionsSelector = mkSelector "acceptedTermsAndConditions"

-- | @Selector@ for @setAcceptedTermsAndConditions:@
setAcceptedTermsAndConditionsSelector :: Selector '[Id NSNumber] ()
setAcceptedTermsAndConditionsSelector = mkSelector "setAcceptedTermsAndConditions:"

-- | @Selector@ for @acceptedTermsAndConditionsVersion@
acceptedTermsAndConditionsVersionSelector :: Selector '[] (Id NSNumber)
acceptedTermsAndConditionsVersionSelector = mkSelector "acceptedTermsAndConditionsVersion"

-- | @Selector@ for @setAcceptedTermsAndConditionsVersion:@
setAcceptedTermsAndConditionsVersionSelector :: Selector '[Id NSNumber] ()
setAcceptedTermsAndConditionsVersionSelector = mkSelector "setAcceptedTermsAndConditionsVersion:"

-- | @Selector@ for @extraAttributesToRead@
extraAttributesToReadSelector :: Selector '[] (Id NSArray)
extraAttributesToReadSelector = mkSelector "extraAttributesToRead"

-- | @Selector@ for @setExtraAttributesToRead:@
setExtraAttributesToReadSelector :: Selector '[Id NSArray] ()
setExtraAttributesToReadSelector = mkSelector "setExtraAttributesToRead:"

-- | @Selector@ for @forceWiFiScan@
forceWiFiScanSelector :: Selector '[] Bool
forceWiFiScanSelector = mkSelector "forceWiFiScan"

-- | @Selector@ for @setForceWiFiScan:@
setForceWiFiScanSelector :: Selector '[Bool] ()
setForceWiFiScanSelector = mkSelector "setForceWiFiScan:"

-- | @Selector@ for @forceThreadScan@
forceThreadScanSelector :: Selector '[] Bool
forceThreadScanSelector = mkSelector "forceThreadScan"

-- | @Selector@ for @setForceThreadScan:@
setForceThreadScanSelector :: Selector '[Bool] ()
setForceThreadScanSelector = mkSelector "setForceThreadScan:"

-- | @Selector@ for @setCSRNonce:@
setCSRNonceSelector :: Selector '[Id NSData] ()
setCSRNonceSelector = mkSelector "setCSRNonce:"

-- | @Selector@ for @failSafeExpiryTimeoutSecs@
failSafeExpiryTimeoutSecsSelector :: Selector '[] (Id NSNumber)
failSafeExpiryTimeoutSecsSelector = mkSelector "failSafeExpiryTimeoutSecs"

-- | @Selector@ for @setFailSafeExpiryTimeoutSecs:@
setFailSafeExpiryTimeoutSecsSelector :: Selector '[Id NSNumber] ()
setFailSafeExpiryTimeoutSecsSelector = mkSelector "setFailSafeExpiryTimeoutSecs:"

