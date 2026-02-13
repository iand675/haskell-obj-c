{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , activePHYModeSelector
  , associateToEnterpriseNetwork_identity_username_password_errorSelector
  , associateToNetwork_password_errorSelector
  , bssidSelector
  , cachedScanResultsSelector
  , commitConfiguration_authorization_errorSelector
  , configurationSelector
  , countryCodeSelector
  , disassociateSelector
  , hardwareAddressSelector
  , initWithInterfaceNameSelector
  , interfaceModeSelector
  , interfaceNameSelector
  , interfaceNamesSelector
  , interfaceSelector
  , interfaceWithNameSelector
  , noiseMeasurementSelector
  , powerOnSelector
  , rssiValueSelector
  , scanForNetworksWithName_errorSelector
  , scanForNetworksWithName_includeHidden_errorSelector
  , scanForNetworksWithSSID_errorSelector
  , scanForNetworksWithSSID_includeHidden_errorSelector
  , securitySelector
  , serviceActiveSelector
  , setPairwiseMasterKey_errorSelector
  , setPower_errorSelector
  , setWEPKey_flags_index_errorSelector
  , setWLANChannel_errorSelector
  , ssidDataSelector
  , ssidSelector
  , startIBSSModeWithSSID_security_channel_password_errorSelector
  , supportedWLANChannelsSelector
  , transmitPowerSelector
  , transmitRateSelector
  , wlanChannelSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
powerOn cwInterface =
  sendMessage cwInterface powerOnSelector

-- | Returns: An NSSet of CWChannel objects.
--
-- Returns the set of channels supported by the Wi-Fi interface for the currently adopted country code.
--
-- Returns nil if an error occurs.
--
-- ObjC selector: @- supportedWLANChannels@
supportedWLANChannels :: IsCWInterface cwInterface => cwInterface -> IO (Id NSSet)
supportedWLANChannels cwInterface =
  sendMessage cwInterface supportedWLANChannelsSelector

-- | Returns the current channel of the Wi-Fi interface.
--
-- Returns nil if an error occurs.
--
-- ObjC selector: @- wlanChannel@
wlanChannel :: IsCWInterface cwInterface => cwInterface -> IO (Id CWChannel)
wlanChannel cwInterface =
  sendMessage cwInterface wlanChannelSelector

-- | Returns the currently active physical layer (PHY) mode of the Wi-Fi interface.
--
-- Returns kCWPHYModeNone if an error occurs.
--
-- ObjC selector: @- activePHYMode@
activePHYMode :: IsCWInterface cwInterface => cwInterface -> IO CWPHYMode
activePHYMode cwInterface =
  sendMessage cwInterface activePHYModeSelector

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
ssid cwInterface =
  sendMessage cwInterface ssidSelector

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
ssidData cwInterface =
  sendMessage cwInterface ssidDataSelector

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
bssid cwInterface =
  sendMessage cwInterface bssidSelector

-- | Returns the current received signal strength indication (RSSI) measurement (dBm) for the Wi-Fi interface.
--
-- Returns 0 if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- ObjC selector: @- rssiValue@
rssiValue :: IsCWInterface cwInterface => cwInterface -> IO CLong
rssiValue cwInterface =
  sendMessage cwInterface rssiValueSelector

-- | Returns the current noise measurement (dBm) for the Wi-Fi interface.
--
-- Returns 0 if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- ObjC selector: @- noiseMeasurement@
noiseMeasurement :: IsCWInterface cwInterface => cwInterface -> IO CLong
noiseMeasurement cwInterface =
  sendMessage cwInterface noiseMeasurementSelector

-- | Returns the current security type of the Wi-Fi interface.
--
-- Returns kCWSecurityUnknown if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- ObjC selector: @- security@
security :: IsCWInterface cwInterface => cwInterface -> IO CWSecurity
security cwInterface =
  sendMessage cwInterface securitySelector

-- | Returns the current transmit rate (Mbps) for the Wi-Fi interface.
--
-- Returns 0 if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- ObjC selector: @- transmitRate@
transmitRate :: IsCWInterface cwInterface => cwInterface -> IO CDouble
transmitRate cwInterface =
  sendMessage cwInterface transmitRateSelector

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
countryCode cwInterface =
  sendMessage cwInterface countryCodeSelector

-- | Returns the current operating mode for the Wi-Fi interface.
--
-- Returns kCWInterfaceModeNone if an error occurs, or if the interface is not participating in a Wi-Fi network.
--
-- ObjC selector: @- interfaceMode@
interfaceMode :: IsCWInterface cwInterface => cwInterface -> IO CWInterfaceMode
interfaceMode cwInterface =
  sendMessage cwInterface interfaceModeSelector

-- | Returns the current transmit power (mW) for the Wi-Fi interface.
--
-- Returns 0 if an error occurs.
--
-- ObjC selector: @- transmitPower@
transmitPower :: IsCWInterface cwInterface => cwInterface -> IO CLong
transmitPower cwInterface =
  sendMessage cwInterface transmitPowerSelector

-- | Returns the hardware media access control (MAC) address for the Wi-Fi interface, returned as an UTF-8 string.
--
-- The standard format for printing a MAC-48 address XX:XX:XX:XX:XX:XX is used to represent the MAC address as a string.  Returns nil if an error occurs.
--
-- ObjC selector: @- hardwareAddress@
hardwareAddress :: IsCWInterface cwInterface => cwInterface -> IO (Id NSString)
hardwareAddress cwInterface =
  sendMessage cwInterface hardwareAddressSelector

-- | Returns: YES if the corresponding network service is active, NO otherwise.
--
-- Indicates the network service state of the Wi-Fi interface.
--
-- Returns NO if an error occurs.
--
-- ObjC selector: @- serviceActive@
serviceActive :: IsCWInterface cwInterface => cwInterface -> IO Bool
serviceActive cwInterface =
  sendMessage cwInterface serviceActiveSelector

-- | Returns: An NSSet of CWNetwork objects.
--
-- Returns the scan results currently in the scan cache for the Wi-Fi interface.
--
-- Returns nil if an error occurs.
--
-- ObjC selector: @- cachedScanResults@
cachedScanResults :: IsCWInterface cwInterface => cwInterface -> IO (Id NSSet)
cachedScanResults cwInterface =
  sendMessage cwInterface cachedScanResultsSelector

-- | Returns the current configuration for the Wi-Fi interface.
--
-- Returns nil if an error occurs.
--
-- ObjC selector: @- configuration@
configuration :: IsCWInterface cwInterface => cwInterface -> IO (Id CWConfiguration)
configuration cwInterface =
  sendMessage cwInterface configurationSelector

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
    sendClassMessage cls' interfaceNamesSelector

-- | Convenience method for getting a CWInterface object for the default Wi-Fi interface.
--
-- ObjC selector: @+ interface@
interface :: IO (Id CWInterface)
interface  =
  do
    cls' <- getRequiredClass "CWInterface"
    sendClassMessage cls' interfaceSelector

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
    sendClassMessage cls' interfaceWithNameSelector (toNSString name)

-- | @name@ — The name of an available Wi-Fi interface.
--
-- Initializes a CWInterface object, binding to the Wi-Fi interface with a specific interface name.
--
-- Use +[CWInterface interfaceNames] to get a list of available Wi-Fi interface names. Returns a CWInterface object for the default Wi-Fi interface if no interface name is specified.
--
-- ObjC selector: @- initWithInterfaceName:@
initWithInterfaceName :: (IsCWInterface cwInterface, IsNSString name) => cwInterface -> name -> IO (Id CWInterface)
initWithInterfaceName cwInterface name =
  sendOwnedMessage cwInterface initWithInterfaceNameSelector (toNSString name)

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
setPower_error cwInterface power error_ =
  sendMessage cwInterface setPower_errorSelector power (toNSError error_)

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
setWLANChannel_error cwInterface channel error_ =
  sendMessage cwInterface setWLANChannel_errorSelector (toCWChannel channel) (toNSError error_)

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
setPairwiseMasterKey_error cwInterface key error_ =
  sendMessage cwInterface setPairwiseMasterKey_errorSelector (toNSData key) (toNSError error_)

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
setWEPKey_flags_index_error cwInterface key flags index error_ =
  sendMessage cwInterface setWEPKey_flags_index_errorSelector (toNSData key) flags index (toNSError error_)

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
scanForNetworksWithSSID_error cwInterface ssid error_ =
  sendMessage cwInterface scanForNetworksWithSSID_errorSelector (toNSData ssid) (toNSError error_)

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
scanForNetworksWithSSID_includeHidden_error cwInterface ssid includeHidden error_ =
  sendMessage cwInterface scanForNetworksWithSSID_includeHidden_errorSelector (toNSData ssid) includeHidden (toNSError error_)

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
scanForNetworksWithName_error cwInterface networkName error_ =
  sendMessage cwInterface scanForNetworksWithName_errorSelector (toNSString networkName) (toNSError error_)

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
scanForNetworksWithName_includeHidden_error cwInterface networkName includeHidden error_ =
  sendMessage cwInterface scanForNetworksWithName_includeHidden_errorSelector (toNSString networkName) includeHidden (toNSError error_)

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
associateToNetwork_password_error cwInterface network password error_ =
  sendMessage cwInterface associateToNetwork_password_errorSelector (toCWNetwork network) (toNSString password) (toNSError error_)

-- | Disassociates from the current Wi-Fi network.
--
-- ObjC selector: @- disassociate@
disassociate :: IsCWInterface cwInterface => cwInterface -> IO ()
disassociate cwInterface =
  sendMessage cwInterface disassociateSelector

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
associateToEnterpriseNetwork_identity_username_password_error cwInterface network identity username password error_ =
  sendMessage cwInterface associateToEnterpriseNetwork_identity_username_password_errorSelector (toCWNetwork network) identity (toNSString username) (toNSString password) (toNSError error_)

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
startIBSSModeWithSSID_security_channel_password_error cwInterface ssidData security channel password error_ =
  sendMessage cwInterface startIBSSModeWithSSID_security_channel_password_errorSelector (toNSData ssidData) security channel (toNSString password) (toNSError error_)

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
commitConfiguration_authorization_error cwInterface configuration authorization error_ =
  sendMessage cwInterface commitConfiguration_authorization_errorSelector (toCWConfiguration configuration) (toSFAuthorization authorization) (toNSError error_)

-- | Returns the BSD name of the Wi-Fi interface (e.g. "en0").
--
-- ObjC selector: @- interfaceName@
interfaceName :: IsCWInterface cwInterface => cwInterface -> IO RawId
interfaceName cwInterface =
  sendMessage cwInterface interfaceNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @powerOn@
powerOnSelector :: Selector '[] Bool
powerOnSelector = mkSelector "powerOn"

-- | @Selector@ for @supportedWLANChannels@
supportedWLANChannelsSelector :: Selector '[] (Id NSSet)
supportedWLANChannelsSelector = mkSelector "supportedWLANChannels"

-- | @Selector@ for @wlanChannel@
wlanChannelSelector :: Selector '[] (Id CWChannel)
wlanChannelSelector = mkSelector "wlanChannel"

-- | @Selector@ for @activePHYMode@
activePHYModeSelector :: Selector '[] CWPHYMode
activePHYModeSelector = mkSelector "activePHYMode"

-- | @Selector@ for @ssid@
ssidSelector :: Selector '[] (Id NSString)
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @ssidData@
ssidDataSelector :: Selector '[] (Id NSData)
ssidDataSelector = mkSelector "ssidData"

-- | @Selector@ for @bssid@
bssidSelector :: Selector '[] (Id NSString)
bssidSelector = mkSelector "bssid"

-- | @Selector@ for @rssiValue@
rssiValueSelector :: Selector '[] CLong
rssiValueSelector = mkSelector "rssiValue"

-- | @Selector@ for @noiseMeasurement@
noiseMeasurementSelector :: Selector '[] CLong
noiseMeasurementSelector = mkSelector "noiseMeasurement"

-- | @Selector@ for @security@
securitySelector :: Selector '[] CWSecurity
securitySelector = mkSelector "security"

-- | @Selector@ for @transmitRate@
transmitRateSelector :: Selector '[] CDouble
transmitRateSelector = mkSelector "transmitRate"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector '[] (Id NSString)
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @interfaceMode@
interfaceModeSelector :: Selector '[] CWInterfaceMode
interfaceModeSelector = mkSelector "interfaceMode"

-- | @Selector@ for @transmitPower@
transmitPowerSelector :: Selector '[] CLong
transmitPowerSelector = mkSelector "transmitPower"

-- | @Selector@ for @hardwareAddress@
hardwareAddressSelector :: Selector '[] (Id NSString)
hardwareAddressSelector = mkSelector "hardwareAddress"

-- | @Selector@ for @serviceActive@
serviceActiveSelector :: Selector '[] Bool
serviceActiveSelector = mkSelector "serviceActive"

-- | @Selector@ for @cachedScanResults@
cachedScanResultsSelector :: Selector '[] (Id NSSet)
cachedScanResultsSelector = mkSelector "cachedScanResults"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id CWConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @interfaceNames@
interfaceNamesSelector :: Selector '[] (Id NSSet)
interfaceNamesSelector = mkSelector "interfaceNames"

-- | @Selector@ for @interface@
interfaceSelector :: Selector '[] (Id CWInterface)
interfaceSelector = mkSelector "interface"

-- | @Selector@ for @interfaceWithName:@
interfaceWithNameSelector :: Selector '[Id NSString] (Id CWInterface)
interfaceWithNameSelector = mkSelector "interfaceWithName:"

-- | @Selector@ for @initWithInterfaceName:@
initWithInterfaceNameSelector :: Selector '[Id NSString] (Id CWInterface)
initWithInterfaceNameSelector = mkSelector "initWithInterfaceName:"

-- | @Selector@ for @setPower:error:@
setPower_errorSelector :: Selector '[Bool, Id NSError] Bool
setPower_errorSelector = mkSelector "setPower:error:"

-- | @Selector@ for @setWLANChannel:error:@
setWLANChannel_errorSelector :: Selector '[Id CWChannel, Id NSError] Bool
setWLANChannel_errorSelector = mkSelector "setWLANChannel:error:"

-- | @Selector@ for @setPairwiseMasterKey:error:@
setPairwiseMasterKey_errorSelector :: Selector '[Id NSData, Id NSError] Bool
setPairwiseMasterKey_errorSelector = mkSelector "setPairwiseMasterKey:error:"

-- | @Selector@ for @setWEPKey:flags:index:error:@
setWEPKey_flags_index_errorSelector :: Selector '[Id NSData, CWCipherKeyFlags, CLong, Id NSError] Bool
setWEPKey_flags_index_errorSelector = mkSelector "setWEPKey:flags:index:error:"

-- | @Selector@ for @scanForNetworksWithSSID:error:@
scanForNetworksWithSSID_errorSelector :: Selector '[Id NSData, Id NSError] (Id NSSet)
scanForNetworksWithSSID_errorSelector = mkSelector "scanForNetworksWithSSID:error:"

-- | @Selector@ for @scanForNetworksWithSSID:includeHidden:error:@
scanForNetworksWithSSID_includeHidden_errorSelector :: Selector '[Id NSData, Bool, Id NSError] (Id NSSet)
scanForNetworksWithSSID_includeHidden_errorSelector = mkSelector "scanForNetworksWithSSID:includeHidden:error:"

-- | @Selector@ for @scanForNetworksWithName:error:@
scanForNetworksWithName_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSSet)
scanForNetworksWithName_errorSelector = mkSelector "scanForNetworksWithName:error:"

-- | @Selector@ for @scanForNetworksWithName:includeHidden:error:@
scanForNetworksWithName_includeHidden_errorSelector :: Selector '[Id NSString, Bool, Id NSError] (Id NSSet)
scanForNetworksWithName_includeHidden_errorSelector = mkSelector "scanForNetworksWithName:includeHidden:error:"

-- | @Selector@ for @associateToNetwork:password:error:@
associateToNetwork_password_errorSelector :: Selector '[Id CWNetwork, Id NSString, Id NSError] Bool
associateToNetwork_password_errorSelector = mkSelector "associateToNetwork:password:error:"

-- | @Selector@ for @disassociate@
disassociateSelector :: Selector '[] ()
disassociateSelector = mkSelector "disassociate"

-- | @Selector@ for @associateToEnterpriseNetwork:identity:username:password:error:@
associateToEnterpriseNetwork_identity_username_password_errorSelector :: Selector '[Id CWNetwork, Ptr (), Id NSString, Id NSString, Id NSError] Bool
associateToEnterpriseNetwork_identity_username_password_errorSelector = mkSelector "associateToEnterpriseNetwork:identity:username:password:error:"

-- | @Selector@ for @startIBSSModeWithSSID:security:channel:password:error:@
startIBSSModeWithSSID_security_channel_password_errorSelector :: Selector '[Id NSData, CWIBSSModeSecurity, CULong, Id NSString, Id NSError] Bool
startIBSSModeWithSSID_security_channel_password_errorSelector = mkSelector "startIBSSModeWithSSID:security:channel:password:error:"

-- | @Selector@ for @commitConfiguration:authorization:error:@
commitConfiguration_authorization_errorSelector :: Selector '[Id CWConfiguration, Id SFAuthorization, Id NSError] Bool
commitConfiguration_authorization_errorSelector = mkSelector "commitConfiguration:authorization:error:"

-- | @Selector@ for @interfaceName@
interfaceNameSelector :: Selector '[] RawId
interfaceNameSelector = mkSelector "interfaceName"

