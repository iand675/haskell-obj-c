{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEVPNProtocolIKEv2
--
-- The NEVPNProtocolIKEv2 class declares the programmatic interface of an object that manages the IKEv2-specific portion of a VPN configuration.
--
-- Instances of this class use IKE version 2 for key negotiation. Instances of this class are thread safe.
--
-- Generated bindings for @NEVPNProtocolIKEv2@.
module ObjC.NetworkExtension.NEVPNProtocolIKEv2
  ( NEVPNProtocolIKEv2
  , IsNEVPNProtocolIKEv2(..)
  , deadPeerDetectionRate
  , setDeadPeerDetectionRate
  , serverCertificateIssuerCommonName
  , setServerCertificateIssuerCommonName
  , serverCertificateCommonName
  , setServerCertificateCommonName
  , certificateType
  , setCertificateType
  , useConfigurationAttributeInternalIPSubnet
  , setUseConfigurationAttributeInternalIPSubnet
  , ikeSecurityAssociationParameters
  , childSecurityAssociationParameters
  , disableMOBIKE
  , setDisableMOBIKE
  , disableRedirect
  , setDisableRedirect
  , enablePFS
  , setEnablePFS
  , allowPostQuantumKeyExchangeFallback
  , setAllowPostQuantumKeyExchangeFallback
  , enableRevocationCheck
  , setEnableRevocationCheck
  , strictRevocationCheck
  , setStrictRevocationCheck
  , minimumTLSVersion
  , setMinimumTLSVersion
  , maximumTLSVersion
  , setMaximumTLSVersion
  , enableFallback
  , setEnableFallback
  , mtu
  , setMtu
  , ppkConfiguration
  , setPpkConfiguration
  , deadPeerDetectionRateSelector
  , setDeadPeerDetectionRateSelector
  , serverCertificateIssuerCommonNameSelector
  , setServerCertificateIssuerCommonNameSelector
  , serverCertificateCommonNameSelector
  , setServerCertificateCommonNameSelector
  , certificateTypeSelector
  , setCertificateTypeSelector
  , useConfigurationAttributeInternalIPSubnetSelector
  , setUseConfigurationAttributeInternalIPSubnetSelector
  , ikeSecurityAssociationParametersSelector
  , childSecurityAssociationParametersSelector
  , disableMOBIKESelector
  , setDisableMOBIKESelector
  , disableRedirectSelector
  , setDisableRedirectSelector
  , enablePFSSelector
  , setEnablePFSSelector
  , allowPostQuantumKeyExchangeFallbackSelector
  , setAllowPostQuantumKeyExchangeFallbackSelector
  , enableRevocationCheckSelector
  , setEnableRevocationCheckSelector
  , strictRevocationCheckSelector
  , setStrictRevocationCheckSelector
  , minimumTLSVersionSelector
  , setMinimumTLSVersionSelector
  , maximumTLSVersionSelector
  , setMaximumTLSVersionSelector
  , enableFallbackSelector
  , setEnableFallbackSelector
  , mtuSelector
  , setMtuSelector
  , ppkConfigurationSelector
  , setPpkConfigurationSelector

  -- * Enum types
  , NEVPNIKEv2CertificateType(NEVPNIKEv2CertificateType)
  , pattern NEVPNIKEv2CertificateTypeRSA
  , pattern NEVPNIKEv2CertificateTypeECDSA256
  , pattern NEVPNIKEv2CertificateTypeECDSA384
  , pattern NEVPNIKEv2CertificateTypeECDSA521
  , pattern NEVPNIKEv2CertificateTypeEd25519
  , pattern NEVPNIKEv2CertificateTypeRSAPSS
  , NEVPNIKEv2DeadPeerDetectionRate(NEVPNIKEv2DeadPeerDetectionRate)
  , pattern NEVPNIKEv2DeadPeerDetectionRateNone
  , pattern NEVPNIKEv2DeadPeerDetectionRateLow
  , pattern NEVPNIKEv2DeadPeerDetectionRateMedium
  , pattern NEVPNIKEv2DeadPeerDetectionRateHigh
  , NEVPNIKEv2TLSVersion(NEVPNIKEv2TLSVersion)
  , pattern NEVPNIKEv2TLSVersionDefault
  , pattern NEVPNIKEv2TLSVersion1_0
  , pattern NEVPNIKEv2TLSVersion1_1
  , pattern NEVPNIKEv2TLSVersion1_2

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
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | deadPeerDetectionRate
--
-- How frequently the IKEv2 client will run the dead peer detection algorithm.  Default is NEVPNIKEv2DeadPeerDetectionRateMedium.
--
-- ObjC selector: @- deadPeerDetectionRate@
deadPeerDetectionRate :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO NEVPNIKEv2DeadPeerDetectionRate
deadPeerDetectionRate nevpnProtocolIKEv2  =
    fmap (coerce :: CLong -> NEVPNIKEv2DeadPeerDetectionRate) $ sendMsg nevpnProtocolIKEv2 (mkSelector "deadPeerDetectionRate") retCLong []

-- | deadPeerDetectionRate
--
-- How frequently the IKEv2 client will run the dead peer detection algorithm.  Default is NEVPNIKEv2DeadPeerDetectionRateMedium.
--
-- ObjC selector: @- setDeadPeerDetectionRate:@
setDeadPeerDetectionRate :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> NEVPNIKEv2DeadPeerDetectionRate -> IO ()
setDeadPeerDetectionRate nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setDeadPeerDetectionRate:") retVoid [argCLong (coerce value)]

-- | serverCertificateIssuerCommonName
--
-- A string containing the Subject Common Name field of the Certificate Authority certificate that issued the IKEv2 server's certificate.
--
-- ObjC selector: @- serverCertificateIssuerCommonName@
serverCertificateIssuerCommonName :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO (Id NSString)
serverCertificateIssuerCommonName nevpnProtocolIKEv2  =
    sendMsg nevpnProtocolIKEv2 (mkSelector "serverCertificateIssuerCommonName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | serverCertificateIssuerCommonName
--
-- A string containing the Subject Common Name field of the Certificate Authority certificate that issued the IKEv2 server's certificate.
--
-- ObjC selector: @- setServerCertificateIssuerCommonName:@
setServerCertificateIssuerCommonName :: (IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2, IsNSString value) => nevpnProtocolIKEv2 -> value -> IO ()
setServerCertificateIssuerCommonName nevpnProtocolIKEv2  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nevpnProtocolIKEv2 (mkSelector "setServerCertificateIssuerCommonName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | serverCertificateCommonName
--
-- A string containing the value to verify in the IKEv2 server certificate's Subject Common Name field.
--
-- ObjC selector: @- serverCertificateCommonName@
serverCertificateCommonName :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO (Id NSString)
serverCertificateCommonName nevpnProtocolIKEv2  =
    sendMsg nevpnProtocolIKEv2 (mkSelector "serverCertificateCommonName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | serverCertificateCommonName
--
-- A string containing the value to verify in the IKEv2 server certificate's Subject Common Name field.
--
-- ObjC selector: @- setServerCertificateCommonName:@
setServerCertificateCommonName :: (IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2, IsNSString value) => nevpnProtocolIKEv2 -> value -> IO ()
setServerCertificateCommonName nevpnProtocolIKEv2  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nevpnProtocolIKEv2 (mkSelector "setServerCertificateCommonName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | certificateType
--
-- contains the type of certificate if an certificate is configured.  Default is RSA.
--
-- ObjC selector: @- certificateType@
certificateType :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO NEVPNIKEv2CertificateType
certificateType nevpnProtocolIKEv2  =
    fmap (coerce :: CLong -> NEVPNIKEv2CertificateType) $ sendMsg nevpnProtocolIKEv2 (mkSelector "certificateType") retCLong []

-- | certificateType
--
-- contains the type of certificate if an certificate is configured.  Default is RSA.
--
-- ObjC selector: @- setCertificateType:@
setCertificateType :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> NEVPNIKEv2CertificateType -> IO ()
setCertificateType nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setCertificateType:") retVoid [argCLong (coerce value)]

-- | useConfigurationAttributeInternalIPSubnet
--
-- Boolean indicating if client should use INTERNAL_IP4_SUBNET / INTERNAL_IP6_SUBNET attributes.  Default is False.
--
-- ObjC selector: @- useConfigurationAttributeInternalIPSubnet@
useConfigurationAttributeInternalIPSubnet :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO Bool
useConfigurationAttributeInternalIPSubnet nevpnProtocolIKEv2  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnProtocolIKEv2 (mkSelector "useConfigurationAttributeInternalIPSubnet") retCULong []

-- | useConfigurationAttributeInternalIPSubnet
--
-- Boolean indicating if client should use INTERNAL_IP4_SUBNET / INTERNAL_IP6_SUBNET attributes.  Default is False.
--
-- ObjC selector: @- setUseConfigurationAttributeInternalIPSubnet:@
setUseConfigurationAttributeInternalIPSubnet :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> Bool -> IO ()
setUseConfigurationAttributeInternalIPSubnet nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setUseConfigurationAttributeInternalIPSubnet:") retVoid [argCULong (if value then 1 else 0)]

-- | IKESecurityAssociationParameters
--
-- Parameters for the IKE SA
--
-- ObjC selector: @- IKESecurityAssociationParameters@
ikeSecurityAssociationParameters :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO (Id NEVPNIKEv2SecurityAssociationParameters)
ikeSecurityAssociationParameters nevpnProtocolIKEv2  =
    sendMsg nevpnProtocolIKEv2 (mkSelector "IKESecurityAssociationParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | childSecurityAssociationParameters
--
-- Parameters for the child SA
--
-- ObjC selector: @- childSecurityAssociationParameters@
childSecurityAssociationParameters :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO (Id NEVPNIKEv2SecurityAssociationParameters)
childSecurityAssociationParameters nevpnProtocolIKEv2  =
    sendMsg nevpnProtocolIKEv2 (mkSelector "childSecurityAssociationParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | disableMOBIKE
--
-- Disable MOBIKE negotiation. Default is NO.
--
-- ObjC selector: @- disableMOBIKE@
disableMOBIKE :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO Bool
disableMOBIKE nevpnProtocolIKEv2  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnProtocolIKEv2 (mkSelector "disableMOBIKE") retCULong []

-- | disableMOBIKE
--
-- Disable MOBIKE negotiation. Default is NO.
--
-- ObjC selector: @- setDisableMOBIKE:@
setDisableMOBIKE :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> Bool -> IO ()
setDisableMOBIKE nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setDisableMOBIKE:") retVoid [argCULong (if value then 1 else 0)]

-- | disableRedirect
--
-- Disable Server Redirect. Default is NO.
--
-- ObjC selector: @- disableRedirect@
disableRedirect :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO Bool
disableRedirect nevpnProtocolIKEv2  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnProtocolIKEv2 (mkSelector "disableRedirect") retCULong []

-- | disableRedirect
--
-- Disable Server Redirect. Default is NO.
--
-- ObjC selector: @- setDisableRedirect:@
setDisableRedirect :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> Bool -> IO ()
setDisableRedirect nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setDisableRedirect:") retVoid [argCULong (if value then 1 else 0)]

-- | enablePFS
--
-- Enable Perfect Forward Secrecy. Default is NO.
--
-- ObjC selector: @- enablePFS@
enablePFS :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO Bool
enablePFS nevpnProtocolIKEv2  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnProtocolIKEv2 (mkSelector "enablePFS") retCULong []

-- | enablePFS
--
-- Enable Perfect Forward Secrecy. Default is NO.
--
-- ObjC selector: @- setEnablePFS:@
setEnablePFS :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> Bool -> IO ()
setEnablePFS nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setEnablePFS:") retVoid [argCULong (if value then 1 else 0)]

-- | allowPostQuantumKeyExchangeFallback
--
-- Allow servers that do not support post-quantum key exchanges to skip them. This property has no effect if no post-quantum key exchange methods     are configured for the IKE SA or Child SA (see NEVPNIKEv2SecurityAssociationParameters.postQuantumKeyExchangeMethods). Default is NO.
--
-- ObjC selector: @- allowPostQuantumKeyExchangeFallback@
allowPostQuantumKeyExchangeFallback :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO Bool
allowPostQuantumKeyExchangeFallback nevpnProtocolIKEv2  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnProtocolIKEv2 (mkSelector "allowPostQuantumKeyExchangeFallback") retCULong []

-- | allowPostQuantumKeyExchangeFallback
--
-- Allow servers that do not support post-quantum key exchanges to skip them. This property has no effect if no post-quantum key exchange methods     are configured for the IKE SA or Child SA (see NEVPNIKEv2SecurityAssociationParameters.postQuantumKeyExchangeMethods). Default is NO.
--
-- ObjC selector: @- setAllowPostQuantumKeyExchangeFallback:@
setAllowPostQuantumKeyExchangeFallback :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> Bool -> IO ()
setAllowPostQuantumKeyExchangeFallback nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setAllowPostQuantumKeyExchangeFallback:") retVoid [argCULong (if value then 1 else 0)]

-- | enableRevocationCheck
--
-- Enable certificate revocation check. Default is NO.
--
-- ObjC selector: @- enableRevocationCheck@
enableRevocationCheck :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO Bool
enableRevocationCheck nevpnProtocolIKEv2  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnProtocolIKEv2 (mkSelector "enableRevocationCheck") retCULong []

-- | enableRevocationCheck
--
-- Enable certificate revocation check. Default is NO.
--
-- ObjC selector: @- setEnableRevocationCheck:@
setEnableRevocationCheck :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> Bool -> IO ()
setEnableRevocationCheck nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setEnableRevocationCheck:") retVoid [argCULong (if value then 1 else 0)]

-- | strictRevocationCheck
--
-- Require positive certificate revocation check response for peer certificate validation to pass. Default is NO.
--
-- ObjC selector: @- strictRevocationCheck@
strictRevocationCheck :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO Bool
strictRevocationCheck nevpnProtocolIKEv2  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnProtocolIKEv2 (mkSelector "strictRevocationCheck") retCULong []

-- | strictRevocationCheck
--
-- Require positive certificate revocation check response for peer certificate validation to pass. Default is NO.
--
-- ObjC selector: @- setStrictRevocationCheck:@
setStrictRevocationCheck :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> Bool -> IO ()
setStrictRevocationCheck nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setStrictRevocationCheck:") retVoid [argCULong (if value then 1 else 0)]

-- | minimumTLSVersion
--
-- Sets a minimum TLS version to allow for EAP-TLS authentication. Default is NEVPNIKEv2TLSVersionDefault.
--
-- ObjC selector: @- minimumTLSVersion@
minimumTLSVersion :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO NEVPNIKEv2TLSVersion
minimumTLSVersion nevpnProtocolIKEv2  =
    fmap (coerce :: CLong -> NEVPNIKEv2TLSVersion) $ sendMsg nevpnProtocolIKEv2 (mkSelector "minimumTLSVersion") retCLong []

-- | minimumTLSVersion
--
-- Sets a minimum TLS version to allow for EAP-TLS authentication. Default is NEVPNIKEv2TLSVersionDefault.
--
-- ObjC selector: @- setMinimumTLSVersion:@
setMinimumTLSVersion :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> NEVPNIKEv2TLSVersion -> IO ()
setMinimumTLSVersion nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setMinimumTLSVersion:") retVoid [argCLong (coerce value)]

-- | maximumTLSVersion
--
-- Sets a maximum TLS version to allow for EAP-TLS authentication. Default is NEVPNIKEv2TLSVersionDefault.
--
-- ObjC selector: @- maximumTLSVersion@
maximumTLSVersion :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO NEVPNIKEv2TLSVersion
maximumTLSVersion nevpnProtocolIKEv2  =
    fmap (coerce :: CLong -> NEVPNIKEv2TLSVersion) $ sendMsg nevpnProtocolIKEv2 (mkSelector "maximumTLSVersion") retCLong []

-- | maximumTLSVersion
--
-- Sets a maximum TLS version to allow for EAP-TLS authentication. Default is NEVPNIKEv2TLSVersionDefault.
--
-- ObjC selector: @- setMaximumTLSVersion:@
setMaximumTLSVersion :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> NEVPNIKEv2TLSVersion -> IO ()
setMaximumTLSVersion nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setMaximumTLSVersion:") retVoid [argCLong (coerce value)]

-- | enableFallback
--
-- Enable Fallback is used to support Wi-Fi Assist. Wi-Fi Assist allows connections for foreground apps to switch over     to Cellular Data when WiFi connectivity is poor. By setting the EnableFallback key, the device will bring up a tunnel over     Cellular Data to carry traffic that is eligible for Wi-Fi Assist and also requires VPN. Enabling fallback requires that the     server support multiple tunnels for a single user. Default is NO.
--
-- ObjC selector: @- enableFallback@
enableFallback :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO Bool
enableFallback nevpnProtocolIKEv2  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnProtocolIKEv2 (mkSelector "enableFallback") retCULong []

-- | enableFallback
--
-- Enable Fallback is used to support Wi-Fi Assist. Wi-Fi Assist allows connections for foreground apps to switch over     to Cellular Data when WiFi connectivity is poor. By setting the EnableFallback key, the device will bring up a tunnel over     Cellular Data to carry traffic that is eligible for Wi-Fi Assist and also requires VPN. Enabling fallback requires that the     server support multiple tunnels for a single user. Default is NO.
--
-- ObjC selector: @- setEnableFallback:@
setEnableFallback :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> Bool -> IO ()
setEnableFallback nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setEnableFallback:") retVoid [argCULong (if value then 1 else 0)]

-- | mtu
--
-- Maximum Transmission Unit (MTU) size in bytes to assign to the tunnel interface.
--
-- ObjC selector: @- mtu@
mtu :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO CULong
mtu nevpnProtocolIKEv2  =
    sendMsg nevpnProtocolIKEv2 (mkSelector "mtu") retCULong []

-- | mtu
--
-- Maximum Transmission Unit (MTU) size in bytes to assign to the tunnel interface.
--
-- ObjC selector: @- setMtu:@
setMtu :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> CULong -> IO ()
setMtu nevpnProtocolIKEv2  value =
    sendMsg nevpnProtocolIKEv2 (mkSelector "setMtu:") retVoid [argCULong value]

-- | ppkConfiguration
--
-- Configuration for the use of a Post-quantum Pre-shared Key (PPK).
--
-- ObjC selector: @- ppkConfiguration@
ppkConfiguration :: IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2 => nevpnProtocolIKEv2 -> IO (Id NEVPNIKEv2PPKConfiguration)
ppkConfiguration nevpnProtocolIKEv2  =
    sendMsg nevpnProtocolIKEv2 (mkSelector "ppkConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ppkConfiguration
--
-- Configuration for the use of a Post-quantum Pre-shared Key (PPK).
--
-- ObjC selector: @- setPpkConfiguration:@
setPpkConfiguration :: (IsNEVPNProtocolIKEv2 nevpnProtocolIKEv2, IsNEVPNIKEv2PPKConfiguration value) => nevpnProtocolIKEv2 -> value -> IO ()
setPpkConfiguration nevpnProtocolIKEv2  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nevpnProtocolIKEv2 (mkSelector "setPpkConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deadPeerDetectionRate@
deadPeerDetectionRateSelector :: Selector
deadPeerDetectionRateSelector = mkSelector "deadPeerDetectionRate"

-- | @Selector@ for @setDeadPeerDetectionRate:@
setDeadPeerDetectionRateSelector :: Selector
setDeadPeerDetectionRateSelector = mkSelector "setDeadPeerDetectionRate:"

-- | @Selector@ for @serverCertificateIssuerCommonName@
serverCertificateIssuerCommonNameSelector :: Selector
serverCertificateIssuerCommonNameSelector = mkSelector "serverCertificateIssuerCommonName"

-- | @Selector@ for @setServerCertificateIssuerCommonName:@
setServerCertificateIssuerCommonNameSelector :: Selector
setServerCertificateIssuerCommonNameSelector = mkSelector "setServerCertificateIssuerCommonName:"

-- | @Selector@ for @serverCertificateCommonName@
serverCertificateCommonNameSelector :: Selector
serverCertificateCommonNameSelector = mkSelector "serverCertificateCommonName"

-- | @Selector@ for @setServerCertificateCommonName:@
setServerCertificateCommonNameSelector :: Selector
setServerCertificateCommonNameSelector = mkSelector "setServerCertificateCommonName:"

-- | @Selector@ for @certificateType@
certificateTypeSelector :: Selector
certificateTypeSelector = mkSelector "certificateType"

-- | @Selector@ for @setCertificateType:@
setCertificateTypeSelector :: Selector
setCertificateTypeSelector = mkSelector "setCertificateType:"

-- | @Selector@ for @useConfigurationAttributeInternalIPSubnet@
useConfigurationAttributeInternalIPSubnetSelector :: Selector
useConfigurationAttributeInternalIPSubnetSelector = mkSelector "useConfigurationAttributeInternalIPSubnet"

-- | @Selector@ for @setUseConfigurationAttributeInternalIPSubnet:@
setUseConfigurationAttributeInternalIPSubnetSelector :: Selector
setUseConfigurationAttributeInternalIPSubnetSelector = mkSelector "setUseConfigurationAttributeInternalIPSubnet:"

-- | @Selector@ for @IKESecurityAssociationParameters@
ikeSecurityAssociationParametersSelector :: Selector
ikeSecurityAssociationParametersSelector = mkSelector "IKESecurityAssociationParameters"

-- | @Selector@ for @childSecurityAssociationParameters@
childSecurityAssociationParametersSelector :: Selector
childSecurityAssociationParametersSelector = mkSelector "childSecurityAssociationParameters"

-- | @Selector@ for @disableMOBIKE@
disableMOBIKESelector :: Selector
disableMOBIKESelector = mkSelector "disableMOBIKE"

-- | @Selector@ for @setDisableMOBIKE:@
setDisableMOBIKESelector :: Selector
setDisableMOBIKESelector = mkSelector "setDisableMOBIKE:"

-- | @Selector@ for @disableRedirect@
disableRedirectSelector :: Selector
disableRedirectSelector = mkSelector "disableRedirect"

-- | @Selector@ for @setDisableRedirect:@
setDisableRedirectSelector :: Selector
setDisableRedirectSelector = mkSelector "setDisableRedirect:"

-- | @Selector@ for @enablePFS@
enablePFSSelector :: Selector
enablePFSSelector = mkSelector "enablePFS"

-- | @Selector@ for @setEnablePFS:@
setEnablePFSSelector :: Selector
setEnablePFSSelector = mkSelector "setEnablePFS:"

-- | @Selector@ for @allowPostQuantumKeyExchangeFallback@
allowPostQuantumKeyExchangeFallbackSelector :: Selector
allowPostQuantumKeyExchangeFallbackSelector = mkSelector "allowPostQuantumKeyExchangeFallback"

-- | @Selector@ for @setAllowPostQuantumKeyExchangeFallback:@
setAllowPostQuantumKeyExchangeFallbackSelector :: Selector
setAllowPostQuantumKeyExchangeFallbackSelector = mkSelector "setAllowPostQuantumKeyExchangeFallback:"

-- | @Selector@ for @enableRevocationCheck@
enableRevocationCheckSelector :: Selector
enableRevocationCheckSelector = mkSelector "enableRevocationCheck"

-- | @Selector@ for @setEnableRevocationCheck:@
setEnableRevocationCheckSelector :: Selector
setEnableRevocationCheckSelector = mkSelector "setEnableRevocationCheck:"

-- | @Selector@ for @strictRevocationCheck@
strictRevocationCheckSelector :: Selector
strictRevocationCheckSelector = mkSelector "strictRevocationCheck"

-- | @Selector@ for @setStrictRevocationCheck:@
setStrictRevocationCheckSelector :: Selector
setStrictRevocationCheckSelector = mkSelector "setStrictRevocationCheck:"

-- | @Selector@ for @minimumTLSVersion@
minimumTLSVersionSelector :: Selector
minimumTLSVersionSelector = mkSelector "minimumTLSVersion"

-- | @Selector@ for @setMinimumTLSVersion:@
setMinimumTLSVersionSelector :: Selector
setMinimumTLSVersionSelector = mkSelector "setMinimumTLSVersion:"

-- | @Selector@ for @maximumTLSVersion@
maximumTLSVersionSelector :: Selector
maximumTLSVersionSelector = mkSelector "maximumTLSVersion"

-- | @Selector@ for @setMaximumTLSVersion:@
setMaximumTLSVersionSelector :: Selector
setMaximumTLSVersionSelector = mkSelector "setMaximumTLSVersion:"

-- | @Selector@ for @enableFallback@
enableFallbackSelector :: Selector
enableFallbackSelector = mkSelector "enableFallback"

-- | @Selector@ for @setEnableFallback:@
setEnableFallbackSelector :: Selector
setEnableFallbackSelector = mkSelector "setEnableFallback:"

-- | @Selector@ for @mtu@
mtuSelector :: Selector
mtuSelector = mkSelector "mtu"

-- | @Selector@ for @setMtu:@
setMtuSelector :: Selector
setMtuSelector = mkSelector "setMtu:"

-- | @Selector@ for @ppkConfiguration@
ppkConfigurationSelector :: Selector
ppkConfigurationSelector = mkSelector "ppkConfiguration"

-- | @Selector@ for @setPpkConfiguration:@
setPpkConfigurationSelector :: Selector
setPpkConfigurationSelector = mkSelector "setPpkConfiguration:"

