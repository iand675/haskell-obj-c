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
  , csrNonceSelector
  , setCsrNonceSelector
  , attestationNonceSelector
  , setAttestationNonceSelector
  , wifiSSIDSelector
  , setWifiSSIDSelector
  , wifiCredentialsSelector
  , setWifiCredentialsSelector
  , threadOperationalDatasetSelector
  , setThreadOperationalDatasetSelector
  , deviceAttestationDelegateSelector
  , setDeviceAttestationDelegateSelector
  , failSafeTimeoutSelector
  , setFailSafeTimeoutSelector
  , skipCommissioningCompleteSelector
  , setSkipCommissioningCompleteSelector
  , countryCodeSelector
  , setCountryCodeSelector
  , readEndpointInformationSelector
  , setReadEndpointInformationSelector
  , acceptedTermsAndConditionsSelector
  , setAcceptedTermsAndConditionsSelector
  , acceptedTermsAndConditionsVersionSelector
  , setAcceptedTermsAndConditionsVersionSelector
  , extraAttributesToReadSelector
  , setExtraAttributesToReadSelector
  , forceWiFiScanSelector
  , setForceWiFiScanSelector
  , forceThreadScanSelector
  , setForceThreadScanSelector
  , setCSRNonceSelector
  , failSafeExpiryTimeoutSecsSelector
  , setFailSafeExpiryTimeoutSecsSelector


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
csrNonce mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "csrNonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The nonce to use when requesting a CSR for the node's operational certificate.
--
-- If nil, a random nonce will be generated automatically.
--
-- If not nil, must be 32 bytes of data.
--
-- ObjC selector: @- setCsrNonce:@
setCsrNonce :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setCsrNonce mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setCsrNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The nonce to use when requesting attestation information from the device.
--
-- If nil, a random nonce will be generated automatically.
--
-- If not nil, must be 32 bytes of data.
--
-- ObjC selector: @- attestationNonce@
attestationNonce :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSData)
attestationNonce mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "attestationNonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The nonce to use when requesting attestation information from the device.
--
-- If nil, a random nonce will be generated automatically.
--
-- If not nil, must be 32 bytes of data.
--
-- ObjC selector: @- setAttestationNonce:@
setAttestationNonce :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setAttestationNonce mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setAttestationNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The Wi-Fi SSID, if available.
--
-- ObjC selector: @- wifiSSID@
wifiSSID :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSData)
wifiSSID mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "wifiSSID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Wi-Fi SSID, if available.
--
-- ObjC selector: @- setWifiSSID:@
setWifiSSID :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setWifiSSID mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setWifiSSID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The Wi-Fi Credentials.  Allowed to be nil or 0-length data for an open network, as long as wifiSSID is not nil.
--
-- ObjC selector: @- wifiCredentials@
wifiCredentials :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSData)
wifiCredentials mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "wifiCredentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Wi-Fi Credentials.  Allowed to be nil or 0-length data for an open network, as long as wifiSSID is not nil.
--
-- ObjC selector: @- setWifiCredentials:@
setWifiCredentials :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setWifiCredentials mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setWifiCredentials:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The Thread operational dataset, if available.
--
-- ObjC selector: @- threadOperationalDataset@
threadOperationalDataset :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSData)
threadOperationalDataset mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "threadOperationalDataset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Thread operational dataset, if available.
--
-- ObjC selector: @- setThreadOperationalDataset:@
setThreadOperationalDataset :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setThreadOperationalDataset mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setThreadOperationalDataset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An optional delegate that can be notified upon completion of device attestation.  See documentation for MTRDeviceAttestationDelegate for details.
--
-- The delegate methods will be invoked on an arbitrary thread.
--
-- ObjC selector: @- deviceAttestationDelegate@
deviceAttestationDelegate :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO RawId
deviceAttestationDelegate mtrCommissioningParameters  =
    fmap (RawId . castPtr) $ sendMsg mtrCommissioningParameters (mkSelector "deviceAttestationDelegate") (retPtr retVoid) []

-- | An optional delegate that can be notified upon completion of device attestation.  See documentation for MTRDeviceAttestationDelegate for details.
--
-- The delegate methods will be invoked on an arbitrary thread.
--
-- ObjC selector: @- setDeviceAttestationDelegate:@
setDeviceAttestationDelegate :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> RawId -> IO ()
setDeviceAttestationDelegate mtrCommissioningParameters  value =
    sendMsg mtrCommissioningParameters (mkSelector "setDeviceAttestationDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The timeout, in seconds, to set for the fail-safe when calling into the deviceAttestationDelegate and waiting for it to respond.
--
-- If nil, the fail-safe will not be extended before calling into the deviceAttestationDelegate.
--
-- ObjC selector: @- failSafeTimeout@
failSafeTimeout :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSNumber)
failSafeTimeout mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "failSafeTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The timeout, in seconds, to set for the fail-safe when calling into the deviceAttestationDelegate and waiting for it to respond.
--
-- If nil, the fail-safe will not be extended before calling into the deviceAttestationDelegate.
--
-- ObjC selector: @- setFailSafeTimeout:@
setFailSafeTimeout :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSNumber value) => mtrCommissioningParameters -> value -> IO ()
setFailSafeTimeout mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setFailSafeTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Only perform the PASE steps of commissioning. If set to YES, commissioning will be completed by another admin on the network.
--
-- Defaults to NO.
--
-- ObjC selector: @- skipCommissioningComplete@
skipCommissioningComplete :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO Bool
skipCommissioningComplete mtrCommissioningParameters  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrCommissioningParameters (mkSelector "skipCommissioningComplete") retCULong []

-- | Only perform the PASE steps of commissioning. If set to YES, commissioning will be completed by another admin on the network.
--
-- Defaults to NO.
--
-- ObjC selector: @- setSkipCommissioningComplete:@
setSkipCommissioningComplete :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> Bool -> IO ()
setSkipCommissioningComplete mtrCommissioningParameters  value =
    sendMsg mtrCommissioningParameters (mkSelector "setSkipCommissioningComplete:") retVoid [argCULong (if value then 1 else 0)]

-- | The country code to provide to the device during commissioning.
--
-- If not nil, this must be a 2-character ISO 3166-1 country code, which the device can use to decide on things like radio communications bands.
--
-- ObjC selector: @- countryCode@
countryCode :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSString)
countryCode mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "countryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The country code to provide to the device during commissioning.
--
-- If not nil, this must be a 2-character ISO 3166-1 country code, which the device can use to decide on things like radio communications bands.
--
-- ObjC selector: @- setCountryCode:@
setCountryCode :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSString value) => mtrCommissioningParameters -> value -> IO ()
setCountryCode mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setCountryCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Read device type information from all endpoints during commissioning. Defaults to NO.
--
-- ObjC selector: @- readEndpointInformation@
readEndpointInformation :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO Bool
readEndpointInformation mtrCommissioningParameters  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrCommissioningParameters (mkSelector "readEndpointInformation") retCULong []

-- | Read device type information from all endpoints during commissioning. Defaults to NO.
--
-- ObjC selector: @- setReadEndpointInformation:@
setReadEndpointInformation :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> Bool -> IO ()
setReadEndpointInformation mtrCommissioningParameters  value =
    sendMsg mtrCommissioningParameters (mkSelector "setReadEndpointInformation:") retVoid [argCULong (if value then 1 else 0)]

-- | A bitmask of the user’s responses to the presented terms and conditions. Each bit corresponds to a term’s acceptance (1) or non-acceptance (0) at the matching index.
--
-- ObjC selector: @- acceptedTermsAndConditions@
acceptedTermsAndConditions :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSNumber)
acceptedTermsAndConditions mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "acceptedTermsAndConditions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A bitmask of the user’s responses to the presented terms and conditions. Each bit corresponds to a term’s acceptance (1) or non-acceptance (0) at the matching index.
--
-- ObjC selector: @- setAcceptedTermsAndConditions:@
setAcceptedTermsAndConditions :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSNumber value) => mtrCommissioningParameters -> value -> IO ()
setAcceptedTermsAndConditions mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setAcceptedTermsAndConditions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The version of the terms and conditions that the user has accepted.
--
-- ObjC selector: @- acceptedTermsAndConditionsVersion@
acceptedTermsAndConditionsVersion :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSNumber)
acceptedTermsAndConditionsVersion mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "acceptedTermsAndConditionsVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The version of the terms and conditions that the user has accepted.
--
-- ObjC selector: @- setAcceptedTermsAndConditionsVersion:@
setAcceptedTermsAndConditionsVersion :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSNumber value) => mtrCommissioningParameters -> value -> IO ()
setAcceptedTermsAndConditionsVersion mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setAcceptedTermsAndConditionsVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | List of attribute paths to read from the commissionee (in addition to whatever attributes are already read to handle readEndpointInformation being YES, or to handle other commissioning tasks).
--
-- The FeatureMap attribute of all Network Commissioning clusters on the commissionee will always be read and does not need to be included in this list.
--
-- ObjC selector: @- extraAttributesToRead@
extraAttributesToRead :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSArray)
extraAttributesToRead mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "extraAttributesToRead") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | List of attribute paths to read from the commissionee (in addition to whatever attributes are already read to handle readEndpointInformation being YES, or to handle other commissioning tasks).
--
-- The FeatureMap attribute of all Network Commissioning clusters on the commissionee will always be read and does not need to be included in this list.
--
-- ObjC selector: @- setExtraAttributesToRead:@
setExtraAttributesToRead :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSArray value) => mtrCommissioningParameters -> value -> IO ()
setExtraAttributesToRead mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setExtraAttributesToRead:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
forceWiFiScan mtrCommissioningParameters  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrCommissioningParameters (mkSelector "forceWiFiScan") retCULong []

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
setForceWiFiScan mtrCommissioningParameters  value =
    sendMsg mtrCommissioningParameters (mkSelector "setForceWiFiScan:") retVoid [argCULong (if value then 1 else 0)]

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
forceThreadScan mtrCommissioningParameters  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrCommissioningParameters (mkSelector "forceThreadScan") retCULong []

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
setForceThreadScan mtrCommissioningParameters  value =
    sendMsg mtrCommissioningParameters (mkSelector "setForceThreadScan:") retVoid [argCULong (if value then 1 else 0)]

-- | @- setCSRNonce:@
setCSRNonce :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSData value) => mtrCommissioningParameters -> value -> IO ()
setCSRNonce mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setCSRNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- failSafeExpiryTimeoutSecs@
failSafeExpiryTimeoutSecs :: IsMTRCommissioningParameters mtrCommissioningParameters => mtrCommissioningParameters -> IO (Id NSNumber)
failSafeExpiryTimeoutSecs mtrCommissioningParameters  =
    sendMsg mtrCommissioningParameters (mkSelector "failSafeExpiryTimeoutSecs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFailSafeExpiryTimeoutSecs:@
setFailSafeExpiryTimeoutSecs :: (IsMTRCommissioningParameters mtrCommissioningParameters, IsNSNumber value) => mtrCommissioningParameters -> value -> IO ()
setFailSafeExpiryTimeoutSecs mtrCommissioningParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissioningParameters (mkSelector "setFailSafeExpiryTimeoutSecs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @csrNonce@
csrNonceSelector :: Selector
csrNonceSelector = mkSelector "csrNonce"

-- | @Selector@ for @setCsrNonce:@
setCsrNonceSelector :: Selector
setCsrNonceSelector = mkSelector "setCsrNonce:"

-- | @Selector@ for @attestationNonce@
attestationNonceSelector :: Selector
attestationNonceSelector = mkSelector "attestationNonce"

-- | @Selector@ for @setAttestationNonce:@
setAttestationNonceSelector :: Selector
setAttestationNonceSelector = mkSelector "setAttestationNonce:"

-- | @Selector@ for @wifiSSID@
wifiSSIDSelector :: Selector
wifiSSIDSelector = mkSelector "wifiSSID"

-- | @Selector@ for @setWifiSSID:@
setWifiSSIDSelector :: Selector
setWifiSSIDSelector = mkSelector "setWifiSSID:"

-- | @Selector@ for @wifiCredentials@
wifiCredentialsSelector :: Selector
wifiCredentialsSelector = mkSelector "wifiCredentials"

-- | @Selector@ for @setWifiCredentials:@
setWifiCredentialsSelector :: Selector
setWifiCredentialsSelector = mkSelector "setWifiCredentials:"

-- | @Selector@ for @threadOperationalDataset@
threadOperationalDatasetSelector :: Selector
threadOperationalDatasetSelector = mkSelector "threadOperationalDataset"

-- | @Selector@ for @setThreadOperationalDataset:@
setThreadOperationalDatasetSelector :: Selector
setThreadOperationalDatasetSelector = mkSelector "setThreadOperationalDataset:"

-- | @Selector@ for @deviceAttestationDelegate@
deviceAttestationDelegateSelector :: Selector
deviceAttestationDelegateSelector = mkSelector "deviceAttestationDelegate"

-- | @Selector@ for @setDeviceAttestationDelegate:@
setDeviceAttestationDelegateSelector :: Selector
setDeviceAttestationDelegateSelector = mkSelector "setDeviceAttestationDelegate:"

-- | @Selector@ for @failSafeTimeout@
failSafeTimeoutSelector :: Selector
failSafeTimeoutSelector = mkSelector "failSafeTimeout"

-- | @Selector@ for @setFailSafeTimeout:@
setFailSafeTimeoutSelector :: Selector
setFailSafeTimeoutSelector = mkSelector "setFailSafeTimeout:"

-- | @Selector@ for @skipCommissioningComplete@
skipCommissioningCompleteSelector :: Selector
skipCommissioningCompleteSelector = mkSelector "skipCommissioningComplete"

-- | @Selector@ for @setSkipCommissioningComplete:@
setSkipCommissioningCompleteSelector :: Selector
setSkipCommissioningCompleteSelector = mkSelector "setSkipCommissioningComplete:"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @setCountryCode:@
setCountryCodeSelector :: Selector
setCountryCodeSelector = mkSelector "setCountryCode:"

-- | @Selector@ for @readEndpointInformation@
readEndpointInformationSelector :: Selector
readEndpointInformationSelector = mkSelector "readEndpointInformation"

-- | @Selector@ for @setReadEndpointInformation:@
setReadEndpointInformationSelector :: Selector
setReadEndpointInformationSelector = mkSelector "setReadEndpointInformation:"

-- | @Selector@ for @acceptedTermsAndConditions@
acceptedTermsAndConditionsSelector :: Selector
acceptedTermsAndConditionsSelector = mkSelector "acceptedTermsAndConditions"

-- | @Selector@ for @setAcceptedTermsAndConditions:@
setAcceptedTermsAndConditionsSelector :: Selector
setAcceptedTermsAndConditionsSelector = mkSelector "setAcceptedTermsAndConditions:"

-- | @Selector@ for @acceptedTermsAndConditionsVersion@
acceptedTermsAndConditionsVersionSelector :: Selector
acceptedTermsAndConditionsVersionSelector = mkSelector "acceptedTermsAndConditionsVersion"

-- | @Selector@ for @setAcceptedTermsAndConditionsVersion:@
setAcceptedTermsAndConditionsVersionSelector :: Selector
setAcceptedTermsAndConditionsVersionSelector = mkSelector "setAcceptedTermsAndConditionsVersion:"

-- | @Selector@ for @extraAttributesToRead@
extraAttributesToReadSelector :: Selector
extraAttributesToReadSelector = mkSelector "extraAttributesToRead"

-- | @Selector@ for @setExtraAttributesToRead:@
setExtraAttributesToReadSelector :: Selector
setExtraAttributesToReadSelector = mkSelector "setExtraAttributesToRead:"

-- | @Selector@ for @forceWiFiScan@
forceWiFiScanSelector :: Selector
forceWiFiScanSelector = mkSelector "forceWiFiScan"

-- | @Selector@ for @setForceWiFiScan:@
setForceWiFiScanSelector :: Selector
setForceWiFiScanSelector = mkSelector "setForceWiFiScan:"

-- | @Selector@ for @forceThreadScan@
forceThreadScanSelector :: Selector
forceThreadScanSelector = mkSelector "forceThreadScan"

-- | @Selector@ for @setForceThreadScan:@
setForceThreadScanSelector :: Selector
setForceThreadScanSelector = mkSelector "setForceThreadScan:"

-- | @Selector@ for @setCSRNonce:@
setCSRNonceSelector :: Selector
setCSRNonceSelector = mkSelector "setCSRNonce:"

-- | @Selector@ for @failSafeExpiryTimeoutSecs@
failSafeExpiryTimeoutSecsSelector :: Selector
failSafeExpiryTimeoutSecsSelector = mkSelector "failSafeExpiryTimeoutSecs"

-- | @Selector@ for @setFailSafeExpiryTimeoutSecs:@
setFailSafeExpiryTimeoutSecsSelector :: Selector
setFailSafeExpiryTimeoutSecsSelector = mkSelector "setFailSafeExpiryTimeoutSecs:"

