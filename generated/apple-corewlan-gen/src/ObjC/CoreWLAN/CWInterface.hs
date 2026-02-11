{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Control and query a Wi-Fi interface on OS X.
--
-- All actions performed by a CWInterface object are executed on the Wi-Fi device with the corresponding interface name.
--
-- Generated bindings for @CWInterface@.
module ObjC.CoreWLAN.CWInterface
  ( CWInterface
  , IsCWInterface(..)
  , powerOn
  , supportedWLANChannels
  , wlanChannel
  , activePHYMode
  , ssid
  , ssidData
  , bssid
  , rssiValue
  , noiseMeasurement
  , security
  , transmitRate
  , countryCode
  , interfaceMode
  , transmitPower
  , hardwareAddress
  , serviceActive
  , cachedScanResults
  , configuration
  , interfaceNames
  , interface
  , interfaceWithName
  , initWithInterfaceName
  , setPower_error
  , setWLANChannel_error
  , setPairwiseMasterKey_error
  , setWEPKey_flags_index_error
  , scanForNetworksWithSSID_error
  , scanForNetworksWithSSID_includeHidden_error
  , scanForNetworksWithName_error
  , scanForNetworksWithName_includeHidden_error
  , associateToNetwork_password_error
  , disassociate
  , associateToEnterpriseNetwork_identity_username_password_error
  , startIBSSModeWithSSID_security_channel_password_error
  , commitConfiguration_authorization_error
  , interfaceName
  , powerOnSelector
  , supportedWLANChannelsSelector
  , wlanChannelSelector
  , activePHYModeSelector
  , ssidSelector
  , ssidDataSelector
  , bssidSelector
  , rssiValueSelector
  , noiseMeasurementSelector
  , securitySelector
  , transmitRateSelector
  , countryCodeSelector
  , interfaceModeSelector
  , transmitPowerSelector
  , hardwareAddressSelector
  , serviceActiveSelector
  , cachedScanResultsSelector
  , configurationSelector
  , interfaceNamesSelector
  , interfaceSelector
  , interfaceWithNameSelector
  , initWithInterfaceNameSelector
  , setPower_errorSelector
  , setWLANChannel_errorSelector
  , setPairwiseMasterKey_errorSelector
  , setWEPKey_flags_index_errorSelector
  , scanForNetworksWithSSID_errorSelector
  , scanForNetworksWithSSID_includeHidden_errorSelector
  , scanForNetworksWithName_errorSelector
  , scanForNetworksWithName_includeHidden_errorSelector
  , associateToNetwork_password_errorSelector
  , disassociateSelector
  , associateToEnterpriseNetwork_identity_username_password_errorSelector
  , startIBSSModeWithSSID_security_channel_password_errorSelector
  , commitConfiguration_authorization_errorSelector
  , interfaceNameSelector

  -- * Enum types
  , CWCipherKeyFlags(CWCipherKeyFlags)
  , pattern KCWCipherKeyFlagsNone
  , pattern KCWCipherKeyFlagsUnicast
  , pattern KCWCipherKeyFlagsMulticast
  , pattern KCWCipherKeyFlagsTx
  , pattern KCWCipherKeyFlagsRx
  , CWIBSSModeSecurity(CWIBSSModeSecurity)
  , pattern KCWIBSSModeSecurityNone
  , pattern KCWIBSSModeSecurityWEP40
  , pattern KCWIBSSModeSecurityWEP104
  , CWInterfaceMode(CWInterfaceMode)
  , pattern KCWInterfaceModeNone
  , pattern KCWInterfaceModeStation
  , pattern KCWInterfaceModeIBSS
  , pattern KCWInterfaceModeHostAP
  , CWPHYMode(CWPHYMode)
  , pattern KCWPHYModeNone
  , pattern KCWPHYMode11a
  , pattern KCWPHYMode11b
  , pattern KCWPHYMode11g
  , pattern KCWPHYMode11n
  , pattern KCWPHYMode11ac
  , pattern KCWPHYMode11ax
  , CWSecurity(CWSecurity)
  , pattern KCWSecurityNone
  , pattern KCWSecurityWEP
  , pattern KCWSecurityWPAPersonal
  , pattern KCWSecurityWPAPersonalMixed
  , pattern KCWSecurityWPA2Personal
  , pattern KCWSecurityPersonal
  , pattern KCWSecurityDynamicWEP
  , pattern KCWSecurityWPAEnterprise
  , pattern KCWSecurityWPAEnterpriseMixed
  , pattern KCWSecurityWPA2Enterprise
  , pattern KCWSecurityEnterprise
  , pattern KCWSecurityWPA3Personal
  , pattern KCWSecurityWPA3Enterprise
  , pattern KCWSecurityWPA3Transition
  , pattern KCWSecurityOWE
  , pattern KCWSecurityOWETransition
  , pattern KCWSecurityUnknown

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

import ObjC.CoreWLAN.Internal.Classes
import ObjC.CoreWLAN.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.SecurityFoundation.Internal.Classes

-- | Returns: YES if the Wi-Fi interface is on, NO otherwise.
--
-- Indicates the Wi-Fi interface power state.
--
-- Returns NO if an error occurs.
--
-- ObjC selector: @- powerOn@
powerOn :: IsCWInterface cwInterface => cwInterface -> IO Bool
powerOn cwInterface  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwInterface (mkSelector "powerOn") retCULong []

-- | Returns: An NSSet of CWChannel objects.
--
-- Returns the set of channels supported by the Wi-Fi interface for the currently adopted country code.
--
-- Returns nil if an error occurs.
--
-- ObjC selector: @- supportedWLANChannels@
supportedWLANChannels :: IsCWInterface cwInterface => cwInterface -> IO (Id NSSet)
supportedWLANChannels cwInterface  =
    sendMsg cwInterface (mkSelector "supportedWLANChannels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the current channel of the Wi-Fi interface.
--
-- Returns nil if an error occurs.
--
-- ObjC selector: @- wlanChannel@
wlanChannel :: IsCWInterface cwInterface => cwInterface -> IO (Id CWChannel)
wlanChannel cwInterface  =
    sendMsg cwInterface (mkSelector "wlanChannel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the currently active physical layer (PHY) mode of the Wi-Fi interface.
--
-- Returns kCWPHYModeNone if an error occurs.
--
-- ObjC selector: @- activePHYMode@
activePHYMode :: IsCWInterface cwInterface => cwInterface -> IO CWPHYMode
activePHYMode cwInterface  =
    fmap (coerce :: CLong -> CWPHYMode) $ sendMsg cwInterface (mkSelector "activePHYMode") retCLong []

-- | Returns the current service set identifier (SSID) of the Wi-Fi interface, encoded as a string.
--
-- Returns nil if an error occurs, or if the interface is not participating in a Wi-Fi network, or if the SSID can not be encoded as a valid UTF-8 or WinLatin1 string.
--
-- Note: SSID information is not available unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- ssid@
ssid :: IsCWInterface cwInterface => cwInterface -> IO (Id NSString)
ssid cwInterface  =
    sendMsg cwInterface (mkSelector "ssid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the current service set identifier (SSID) for the interface, encapsulated in an NSData object.
--
-- Returns nil if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- Note: SSID information is not available unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- ssidData@
ssidData :: IsCWInterface cwInterface => cwInterface -> IO (Id NSData)
ssidData cwInterface  =
    sendMsg cwInterface (mkSelector "ssidData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the current basic service set identifier (BSSID) of the Wi-Fi interface, returned as an UTF-8 string.
--
-- Returns a UTF-8 string using hexadecimal characters formatted as XX:XX:XX:XX:XX:XX. Returns nil if an error occurred, or if the interface is not participating in a Wi-Fi network.
--
-- Note: BSSID information is not available unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- bssid@
bssid :: IsCWInterface cwInterface => cwInterface -> IO (Id NSString)
bssid cwInterface  =
    sendMsg cwInterface (mkSelector "bssid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the current received signal strength indication (RSSI) measurement (dBm) for the Wi-Fi interface.
--
-- Returns 0 if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- ObjC selector: @- rssiValue@
rssiValue :: IsCWInterface cwInterface => cwInterface -> IO CLong
rssiValue cwInterface  =
    sendMsg cwInterface (mkSelector "rssiValue") retCLong []

-- | Returns the current noise measurement (dBm) for the Wi-Fi interface.
--
-- Returns 0 if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- ObjC selector: @- noiseMeasurement@
noiseMeasurement :: IsCWInterface cwInterface => cwInterface -> IO CLong
noiseMeasurement cwInterface  =
    sendMsg cwInterface (mkSelector "noiseMeasurement") retCLong []

-- | Returns the current security type of the Wi-Fi interface.
--
-- Returns kCWSecurityUnknown if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- ObjC selector: @- security@
security :: IsCWInterface cwInterface => cwInterface -> IO CWSecurity
security cwInterface  =
    fmap (coerce :: CLong -> CWSecurity) $ sendMsg cwInterface (mkSelector "security") retCLong []

-- | Returns the current transmit rate (Mbps) for the Wi-Fi interface.
--
-- Returns 0 if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- ObjC selector: @- transmitRate@
transmitRate :: IsCWInterface cwInterface => cwInterface -> IO CDouble
transmitRate cwInterface  =
    sendMsg cwInterface (mkSelector "transmitRate") retCDouble []

-- | Returns the currently adopted country code (ISO/IEC 3166-1:1997) for the Wi-Fi interface.
--
-- Returns nil if an error occurs, or if the Wi-Fi interface is off.
--
-- Note: Country code information is not available unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- countryCode@
countryCode :: IsCWInterface cwInterface => cwInterface -> IO (Id NSString)
countryCode cwInterface  =
    sendMsg cwInterface (mkSelector "countryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the current operating mode for the Wi-Fi interface.
--
-- Returns kCWInterfaceModeNone if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- ObjC selector: @- interfaceMode@
interfaceMode :: IsCWInterface cwInterface => cwInterface -> IO CWInterfaceMode
interfaceMode cwInterface  =
    fmap (coerce :: CLong -> CWInterfaceMode) $ sendMsg cwInterface (mkSelector "interfaceMode") retCLong []

-- | Returns the current transmit power (mW) for the Wi-Fi interface.
--
-- Returns 0 if an error occurs.
--
-- ObjC selector: @- transmitPower@
transmitPower :: IsCWInterface cwInterface => cwInterface -> IO CLong
transmitPower cwInterface  =
    sendMsg cwInterface (mkSelector "transmitPower") retCLong []

-- | Returns the hardware media access control (MAC) address for the Wi-Fi interface, returned as an UTF-8 string.
--
-- The standard format for printing a MAC-48 address XX:XX:XX:XX:XX:XX is used to represent the MAC address as a string.  Returns nil if an error occurs.
--
-- ObjC selector: @- hardwareAddress@
hardwareAddress :: IsCWInterface cwInterface => cwInterface -> IO (Id NSString)
hardwareAddress cwInterface  =
    sendMsg cwInterface (mkSelector "hardwareAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns: YES if the corresponding network service is active, NO otherwise.
--
-- Indicates the network service state of the Wi-Fi interface.
--
-- Returns NO if an error occurs.
--
-- ObjC selector: @- serviceActive@
serviceActive :: IsCWInterface cwInterface => cwInterface -> IO Bool
serviceActive cwInterface  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwInterface (mkSelector "serviceActive") retCULong []

-- | Returns: An NSSet of CWNetwork objects.
--
-- Returns the scan results currently in the scan cache for the Wi-Fi interface.
--
-- Returns nil if an error occurs.
--
-- ObjC selector: @- cachedScanResults@
cachedScanResults :: IsCWInterface cwInterface => cwInterface -> IO (Id NSSet)
cachedScanResults cwInterface  =
    sendMsg cwInterface (mkSelector "cachedScanResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the current configuration for the Wi-Fi interface.
--
-- Returns nil if an error occurs.
--
-- ObjC selector: @- configuration@
configuration :: IsCWInterface cwInterface => cwInterface -> IO (Id CWConfiguration)
configuration cwInterface  =
    sendMsg cwInterface (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns: An NSSet of NSString objects.
--
-- Returns the list of available Wi-Fi interface names (e.g. "en0").
--
-- Returns an empty NSArray object if no Wi-Fi interfaces exist. Returns nil if an error occurs.
--
-- ObjC selector: @+ interfaceNames@
interfaceNames :: IO (Id NSSet)
interfaceNames  =
  do
    cls' <- getRequiredClass "CWInterface"
    sendClassMsg cls' (mkSelector "interfaceNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience method for getting a CWInterface object for the default Wi-Fi interface.
--
-- ObjC selector: @+ interface@
interface :: IO (Id CWInterface)
interface  =
  do
    cls' <- getRequiredClass "CWInterface"
    sendClassMsg cls' (mkSelector "interface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @name@ — The name of an available Wi-Fi interface.
--
-- Convenience method for getting a CWInterface object bound to the Wi-Fi interface with a specific interface name.
--
-- Use +[CWInterface interfaceNames] to get a list of available Wi-Fi interface names. Returns a CWInterface object for the default Wi-Fi interface if no interface name is specified.
--
-- ObjC selector: @+ interfaceWithName:@
interfaceWithName :: IsNSString name => name -> IO (Id CWInterface)
interfaceWithName name =
  do
    cls' <- getRequiredClass "CWInterface"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "interfaceWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @name@ — The name of an available Wi-Fi interface.
--
-- Initializes a CWInterface object, binding to the Wi-Fi interface with a specific interface name.
--
-- Use +[CWInterface interfaceNames] to get a list of available Wi-Fi interface names. Returns a CWInterface object for the default Wi-Fi interface if no interface name is specified.
--
-- ObjC selector: @- initWithInterfaceName:@
initWithInterfaceName :: (IsCWInterface cwInterface, IsNSString name) => cwInterface -> name -> IO (Id CWInterface)
initWithInterfaceName cwInterface  name =
  withObjCPtr name $ \raw_name ->
      sendMsg cwInterface (mkSelector "initWithInterfaceName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @power@ — A BOOL value indicating Wi-Fi power state. Specify YES to turn on the Wi-Fi interface.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs.  This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Sets the Wi-Fi interface power state.
--
-- ObjC selector: @- setPower:error:@
setPower_error :: (IsCWInterface cwInterface, IsNSError error_) => cwInterface -> Bool -> error_ -> IO Bool
setPower_error cwInterface  power error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwInterface (mkSelector "setPower:error:") retCULong [argCULong (if power then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @channel@ — A CWChannel object.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Sets the Wi-Fi interface channel.
--
-- Setting the channel while the interface is associated to a Wi-Fi network is not permitted.
--
-- ObjC selector: @- setWLANChannel:error:@
setWLANChannel_error :: (IsCWInterface cwInterface, IsCWChannel channel, IsNSError error_) => cwInterface -> channel -> error_ -> IO Bool
setWLANChannel_error cwInterface  channel error_ =
  withObjCPtr channel $ \raw_channel ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwInterface (mkSelector "setWLANChannel:error:") retCULong [argPtr (castPtr raw_channel :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @key@ — An NSData object containing the pairwise master key (PMK). Passing nil clear the PMK for the Wi-Fi interface.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Sets the Wi-Fi interface pairwise master key (PMK).
--
-- The specified key must be exactly 32 octets.
--
-- ObjC selector: @- setPairwiseMasterKey:error:@
setPairwiseMasterKey_error :: (IsCWInterface cwInterface, IsNSData key, IsNSError error_) => cwInterface -> key -> error_ -> IO Bool
setPairwiseMasterKey_error cwInterface  key error_ =
  withObjCPtr key $ \raw_key ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwInterface (mkSelector "setPairwiseMasterKey:error:") retCULong [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @key@ — An NSData object containing the WEP key. Passing nil clears the WEP key for the Wi-Fi interface.
--
-- @flags@ — A bitmask indicating which CWCipherKeyFlags to use for the specified WEP key.
--
-- @index@ — An NSInteger indicating which default key index (1-4) to use for the specified key.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Sets the Wi-Fi interface WEP key.
--
-- ObjC selector: @- setWEPKey:flags:index:error:@
setWEPKey_flags_index_error :: (IsCWInterface cwInterface, IsNSData key, IsNSError error_) => cwInterface -> key -> CWCipherKeyFlags -> CLong -> error_ -> IO Bool
setWEPKey_flags_index_error cwInterface  key flags index error_ =
  withObjCPtr key $ \raw_key ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwInterface (mkSelector "setWEPKey:flags:index:error:") retCULong [argPtr (castPtr raw_key :: Ptr ()), argCULong (coerce flags), argCLong index, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @ssid@ — Probe request SSID.   Pass an SSID to perform a directed scan for hidden Wi-Fi networks. This parameter is optional.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: An NSSet of CWNetwork objects, or nil if an error occurs.
--
-- Performs a scan for Wi-Fi networks and returns scan results to the caller.
--
-- This method will block for the duration of the scan.
--
-- Note: Returned networks will not contain BSSID information unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- scanForNetworksWithSSID:error:@
scanForNetworksWithSSID_error :: (IsCWInterface cwInterface, IsNSData ssid, IsNSError error_) => cwInterface -> ssid -> error_ -> IO (Id NSSet)
scanForNetworksWithSSID_error cwInterface  ssid error_ =
  withObjCPtr ssid $ \raw_ssid ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg cwInterface (mkSelector "scanForNetworksWithSSID:error:") (retPtr retVoid) [argPtr (castPtr raw_ssid :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @ssid@ — Probe request SSID. Pass an SSID to perform a directed scan for hidden Wi-Fi networks. This parameter is optional.
--
-- @includeHidden@ — Indicate whether or not hidden networks should not be filtered from the returned scan results.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: An NSSet of CWNetwork objects, or nil if an error occurs.
--
-- Performs a scan for Wi-Fi networks and returns scan results to the caller.
--
-- This method will block for the duration of the scan.
--
-- Note: Returned networks will not contain BSSID information unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- scanForNetworksWithSSID:includeHidden:error:@
scanForNetworksWithSSID_includeHidden_error :: (IsCWInterface cwInterface, IsNSData ssid, IsNSError error_) => cwInterface -> ssid -> Bool -> error_ -> IO (Id NSSet)
scanForNetworksWithSSID_includeHidden_error cwInterface  ssid includeHidden error_ =
  withObjCPtr ssid $ \raw_ssid ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg cwInterface (mkSelector "scanForNetworksWithSSID:includeHidden:error:") (retPtr retVoid) [argPtr (castPtr raw_ssid :: Ptr ()), argCULong (if includeHidden then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @networkName@ — Probe request SSID, encoded as an UTF-8 string. Pass a networkName to perform a directed scan for hidden Wi-Fi networks. This parameter is optional.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: An NSSet of CWNetwork objects, or nil if an error occurs.
--
-- Performs a scan for Wi-Fi networks and returns scan results to the caller.
--
-- This method will block for the duration of the scan.
--
-- Note: Returned networks will not contain BSSID information unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- scanForNetworksWithName:error:@
scanForNetworksWithName_error :: (IsCWInterface cwInterface, IsNSString networkName, IsNSError error_) => cwInterface -> networkName -> error_ -> IO (Id NSSet)
scanForNetworksWithName_error cwInterface  networkName error_ =
  withObjCPtr networkName $ \raw_networkName ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg cwInterface (mkSelector "scanForNetworksWithName:error:") (retPtr retVoid) [argPtr (castPtr raw_networkName :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @networkName@ — Probe request SSID, encoded as an UTF-8 string. Pass a networkName to perform a directed scan for hidden Wi-Fi networks. This parameter is optional.
--
-- @includeHidden@ — Indicate whether or not hidden networks should not be filtered from the returned scan results.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: An NSSet of CWNetwork objects, or nil if an error occurs.
--
-- Performs a scan for Wi-Fi networks and returns scan results to the caller.
--
-- This method will block for the duration of the scan.
--
-- Note: Returned networks will not contain BSSID information unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- scanForNetworksWithName:includeHidden:error:@
scanForNetworksWithName_includeHidden_error :: (IsCWInterface cwInterface, IsNSString networkName, IsNSError error_) => cwInterface -> networkName -> Bool -> error_ -> IO (Id NSSet)
scanForNetworksWithName_includeHidden_error cwInterface  networkName includeHidden error_ =
  withObjCPtr networkName $ \raw_networkName ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg cwInterface (mkSelector "scanForNetworksWithName:includeHidden:error:") (retPtr retVoid) [argPtr (castPtr raw_networkName :: Ptr ()), argCULong (if includeHidden then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @network@ — The network to which the Wi-Fi interface will associate.
--
-- @password@ — The network passphrase or key. Required for association to WEP, WPA Personal, and WPA2 Personal networks.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Associates to a W-Fi network using the specified passphrase.
--
-- This method will block for the duration of the association.
--
-- ObjC selector: @- associateToNetwork:password:error:@
associateToNetwork_password_error :: (IsCWInterface cwInterface, IsCWNetwork network, IsNSString password, IsNSError error_) => cwInterface -> network -> password -> error_ -> IO Bool
associateToNetwork_password_error cwInterface  network password error_ =
  withObjCPtr network $ \raw_network ->
    withObjCPtr password $ \raw_password ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwInterface (mkSelector "associateToNetwork:password:error:") retCULong [argPtr (castPtr raw_network :: Ptr ()), argPtr (castPtr raw_password :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Disassociates from the current Wi-Fi network.
--
-- ObjC selector: @- disassociate@
disassociate :: IsCWInterface cwInterface => cwInterface -> IO ()
disassociate cwInterface  =
    sendMsg cwInterface (mkSelector "disassociate") retVoid []

-- | @network@ — The network to which the Wi-Fi interface will associate.
--
-- @username@ — The username to use for 802.1X authentication.
--
-- @password@ — The password to use for 802.1X authentication.
--
-- @identity@ — The identity to use for IEEE 802.1X authentication. Holds the corresponding client certificate.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Associates to an enterprise W-Fi network using the specified 802.1X credentials.
--
-- This method will block for the duration of the association.
--
-- ObjC selector: @- associateToEnterpriseNetwork:identity:username:password:error:@
associateToEnterpriseNetwork_identity_username_password_error :: (IsCWInterface cwInterface, IsCWNetwork network, IsNSString username, IsNSString password, IsNSError error_) => cwInterface -> network -> Ptr () -> username -> password -> error_ -> IO Bool
associateToEnterpriseNetwork_identity_username_password_error cwInterface  network identity username password error_ =
  withObjCPtr network $ \raw_network ->
    withObjCPtr username $ \raw_username ->
      withObjCPtr password $ \raw_password ->
        withObjCPtr error_ $ \raw_error_ ->
            fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwInterface (mkSelector "associateToEnterpriseNetwork:identity:username:password:error:") retCULong [argPtr (castPtr raw_network :: Ptr ()), argPtr identity, argPtr (castPtr raw_username :: Ptr ()), argPtr (castPtr raw_password :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @ssidData@ — The SSID to use for the IBSS network. Pass nil to use the machine name as the IBSS network name.
--
-- @security@ — The CWIBSSModeSecurity type.
--
-- @channel@ — The channel on which the IBSS network will be created.
--
-- @password@ — The password to be used. This paramter is required for kCWIBSSModeSecurityWEP40 or kCWIBSSModeSecurityWEP104 security types.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Creates a computer-to-computer (IBSS) network.
--
-- ObjC selector: @- startIBSSModeWithSSID:security:channel:password:error:@
startIBSSModeWithSSID_security_channel_password_error :: (IsCWInterface cwInterface, IsNSData ssidData, IsNSString password, IsNSError error_) => cwInterface -> ssidData -> CWIBSSModeSecurity -> CULong -> password -> error_ -> IO Bool
startIBSSModeWithSSID_security_channel_password_error cwInterface  ssidData security channel password error_ =
  withObjCPtr ssidData $ \raw_ssidData ->
    withObjCPtr password $ \raw_password ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwInterface (mkSelector "startIBSSModeWithSSID:security:channel:password:error:") retCULong [argPtr (castPtr raw_ssidData :: Ptr ()), argCLong (coerce security), argCULong channel, argPtr (castPtr raw_password :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @configuration@ — The Wi-Fi configuration to commit to disk.
--
-- @authorization@ — An SFAuthorization object to use for authorizing the commit. This parameter is optional.
--
-- @error@ — An NSError object passed by reference, which upon return will contain the error if an error occurs. This parameter is optional.
--
-- Returns: Returns YES upon success, or NO if an error occurred.
--
-- Commits a CWConfiguration for the given Wi-Fi interface.
--
-- This method requires the caller have root privileges or obtain administrator privileges using the SFAuthorization API.
--
-- ObjC selector: @- commitConfiguration:authorization:error:@
commitConfiguration_authorization_error :: (IsCWInterface cwInterface, IsCWConfiguration configuration, IsSFAuthorization authorization, IsNSError error_) => cwInterface -> configuration -> authorization -> error_ -> IO Bool
commitConfiguration_authorization_error cwInterface  configuration authorization error_ =
  withObjCPtr configuration $ \raw_configuration ->
    withObjCPtr authorization $ \raw_authorization ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwInterface (mkSelector "commitConfiguration:authorization:error:") retCULong [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_authorization :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Returns the BSD name of the Wi-Fi interface (e.g. "en0").
--
-- ObjC selector: @- interfaceName@
interfaceName :: IsCWInterface cwInterface => cwInterface -> IO RawId
interfaceName cwInterface  =
    fmap (RawId . castPtr) $ sendMsg cwInterface (mkSelector "interfaceName") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @powerOn@
powerOnSelector :: Selector
powerOnSelector = mkSelector "powerOn"

-- | @Selector@ for @supportedWLANChannels@
supportedWLANChannelsSelector :: Selector
supportedWLANChannelsSelector = mkSelector "supportedWLANChannels"

-- | @Selector@ for @wlanChannel@
wlanChannelSelector :: Selector
wlanChannelSelector = mkSelector "wlanChannel"

-- | @Selector@ for @activePHYMode@
activePHYModeSelector :: Selector
activePHYModeSelector = mkSelector "activePHYMode"

-- | @Selector@ for @ssid@
ssidSelector :: Selector
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @ssidData@
ssidDataSelector :: Selector
ssidDataSelector = mkSelector "ssidData"

-- | @Selector@ for @bssid@
bssidSelector :: Selector
bssidSelector = mkSelector "bssid"

-- | @Selector@ for @rssiValue@
rssiValueSelector :: Selector
rssiValueSelector = mkSelector "rssiValue"

-- | @Selector@ for @noiseMeasurement@
noiseMeasurementSelector :: Selector
noiseMeasurementSelector = mkSelector "noiseMeasurement"

-- | @Selector@ for @security@
securitySelector :: Selector
securitySelector = mkSelector "security"

-- | @Selector@ for @transmitRate@
transmitRateSelector :: Selector
transmitRateSelector = mkSelector "transmitRate"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @interfaceMode@
interfaceModeSelector :: Selector
interfaceModeSelector = mkSelector "interfaceMode"

-- | @Selector@ for @transmitPower@
transmitPowerSelector :: Selector
transmitPowerSelector = mkSelector "transmitPower"

-- | @Selector@ for @hardwareAddress@
hardwareAddressSelector :: Selector
hardwareAddressSelector = mkSelector "hardwareAddress"

-- | @Selector@ for @serviceActive@
serviceActiveSelector :: Selector
serviceActiveSelector = mkSelector "serviceActive"

-- | @Selector@ for @cachedScanResults@
cachedScanResultsSelector :: Selector
cachedScanResultsSelector = mkSelector "cachedScanResults"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @interfaceNames@
interfaceNamesSelector :: Selector
interfaceNamesSelector = mkSelector "interfaceNames"

-- | @Selector@ for @interface@
interfaceSelector :: Selector
interfaceSelector = mkSelector "interface"

-- | @Selector@ for @interfaceWithName:@
interfaceWithNameSelector :: Selector
interfaceWithNameSelector = mkSelector "interfaceWithName:"

-- | @Selector@ for @initWithInterfaceName:@
initWithInterfaceNameSelector :: Selector
initWithInterfaceNameSelector = mkSelector "initWithInterfaceName:"

-- | @Selector@ for @setPower:error:@
setPower_errorSelector :: Selector
setPower_errorSelector = mkSelector "setPower:error:"

-- | @Selector@ for @setWLANChannel:error:@
setWLANChannel_errorSelector :: Selector
setWLANChannel_errorSelector = mkSelector "setWLANChannel:error:"

-- | @Selector@ for @setPairwiseMasterKey:error:@
setPairwiseMasterKey_errorSelector :: Selector
setPairwiseMasterKey_errorSelector = mkSelector "setPairwiseMasterKey:error:"

-- | @Selector@ for @setWEPKey:flags:index:error:@
setWEPKey_flags_index_errorSelector :: Selector
setWEPKey_flags_index_errorSelector = mkSelector "setWEPKey:flags:index:error:"

-- | @Selector@ for @scanForNetworksWithSSID:error:@
scanForNetworksWithSSID_errorSelector :: Selector
scanForNetworksWithSSID_errorSelector = mkSelector "scanForNetworksWithSSID:error:"

-- | @Selector@ for @scanForNetworksWithSSID:includeHidden:error:@
scanForNetworksWithSSID_includeHidden_errorSelector :: Selector
scanForNetworksWithSSID_includeHidden_errorSelector = mkSelector "scanForNetworksWithSSID:includeHidden:error:"

-- | @Selector@ for @scanForNetworksWithName:error:@
scanForNetworksWithName_errorSelector :: Selector
scanForNetworksWithName_errorSelector = mkSelector "scanForNetworksWithName:error:"

-- | @Selector@ for @scanForNetworksWithName:includeHidden:error:@
scanForNetworksWithName_includeHidden_errorSelector :: Selector
scanForNetworksWithName_includeHidden_errorSelector = mkSelector "scanForNetworksWithName:includeHidden:error:"

-- | @Selector@ for @associateToNetwork:password:error:@
associateToNetwork_password_errorSelector :: Selector
associateToNetwork_password_errorSelector = mkSelector "associateToNetwork:password:error:"

-- | @Selector@ for @disassociate@
disassociateSelector :: Selector
disassociateSelector = mkSelector "disassociate"

-- | @Selector@ for @associateToEnterpriseNetwork:identity:username:password:error:@
associateToEnterpriseNetwork_identity_username_password_errorSelector :: Selector
associateToEnterpriseNetwork_identity_username_password_errorSelector = mkSelector "associateToEnterpriseNetwork:identity:username:password:error:"

-- | @Selector@ for @startIBSSModeWithSSID:security:channel:password:error:@
startIBSSModeWithSSID_security_channel_password_errorSelector :: Selector
startIBSSModeWithSSID_security_channel_password_errorSelector = mkSelector "startIBSSModeWithSSID:security:channel:password:error:"

-- | @Selector@ for @commitConfiguration:authorization:error:@
commitConfiguration_authorization_errorSelector :: Selector
commitConfiguration_authorization_errorSelector = mkSelector "commitConfiguration:authorization:error:"

-- | @Selector@ for @interfaceName@
interfaceNameSelector :: Selector
interfaceNameSelector = mkSelector "interfaceName"

