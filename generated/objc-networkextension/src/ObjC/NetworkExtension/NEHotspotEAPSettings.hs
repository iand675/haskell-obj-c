{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEHotspotEAPSettings
--
-- NEHotspotEAPSettings class provides a set of properties that are required   to configure a WPA/WPA2 Enterprise or Hotspot 2.0 Wi-Fi networks.
--
-- Generated bindings for @NEHotspotEAPSettings@.
module ObjC.NetworkExtension.NEHotspotEAPSettings
  ( NEHotspotEAPSettings
  , IsNEHotspotEAPSettings(..)
  , setIdentity
  , setTrustedServerCertificates
  , ttlsInnerAuthenticationType
  , setTtlsInnerAuthenticationType
  , tlsClientCertificateRequired
  , setTlsClientCertificateRequired
  , preferredTLSVersion
  , setPreferredTLSVersion
  , setIdentitySelector
  , setTrustedServerCertificatesSelector
  , ttlsInnerAuthenticationTypeSelector
  , setTtlsInnerAuthenticationTypeSelector
  , tlsClientCertificateRequiredSelector
  , setTlsClientCertificateRequiredSelector
  , preferredTLSVersionSelector
  , setPreferredTLSVersionSelector

  -- * Enum types
  , NEHotspotConfigurationEAPTLSVersion(NEHotspotConfigurationEAPTLSVersion)
  , pattern NEHotspotConfigurationEAPTLSVersion_1_0
  , pattern NEHotspotConfigurationEAPTLSVersion_1_1
  , pattern NEHotspotConfigurationEAPTLSVersion_1_2
  , NEHotspotConfigurationTTLSInnerAuthenticationType(NEHotspotConfigurationTTLSInnerAuthenticationType)
  , pattern NEHotspotConfigurationEAPTTLSInnerAuthenticationPAP
  , pattern NEHotspotConfigurationEAPTTLSInnerAuthenticationCHAP
  , pattern NEHotspotConfigurationEAPTTLSInnerAuthenticationMSCHAP
  , pattern NEHotspotConfigurationEAPTTLSInnerAuthenticationMSCHAPv2
  , pattern NEHotspotConfigurationEAPTTLSInnerAuthenticationEAP

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

-- | setIdentity
--
-- Setter to configure the EAP peer identity. The application needs to store this identity in keychain access group "$(TeamIdentifierPrefix)com.apple.networkextensionsharing". The API uses SecItemCopyMatching to obtain persistent reference for this identity from application's keychain and uses that at the time of EAP authentication. This property is mandatory when EAP-TLS is desired or tlsClientCertificateRequired is set to YES.
--
-- @identity@ — The identity of the EAP Peer. This is a SecIdentityRef object that contains a SecKeyRef object and an associated SecCertificateRef object.
--
-- Returns: returns NO if the parameter is not an object of SecIdentityRef type or if the persistent reference is not found in the application's keychain else returns YES.
--
-- ObjC selector: @- setIdentity:@
setIdentity :: IsNEHotspotEAPSettings neHotspotEAPSettings => neHotspotEAPSettings -> Ptr () -> IO Bool
setIdentity neHotspotEAPSettings  identity =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neHotspotEAPSettings (mkSelector "setIdentity:") retCULong [argPtr identity]

-- | setTrustedServerCertificates
--
-- Setter to configure an array of trusted server certificates used for trust evaluation of the server certificate.
--
-- @certificates@ — Each value in the array is a SecCertificateRef object. Application needs to store the certificates in keychain access group "$(TeamIdentifierPrefix)com.apple.networkextensionsharing". The API uses SecItemCopyMatching to obtain persistent reference for each certificate from application's keychain and uses that at the time os EAP authentication. Number of elements in the array cannot be more than 10.
--
-- Returns: returns NO if any element in the array is not an object of type SecCertificateRef or if API fails to find persistent reference for each element from the application's keychain else return YES.
--
-- ObjC selector: @- setTrustedServerCertificates:@
setTrustedServerCertificates :: (IsNEHotspotEAPSettings neHotspotEAPSettings, IsNSArray certificates) => neHotspotEAPSettings -> certificates -> IO Bool
setTrustedServerCertificates neHotspotEAPSettings  certificates =
withObjCPtr certificates $ \raw_certificates ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg neHotspotEAPSettings (mkSelector "setTrustedServerCertificates:") retCULong [argPtr (castPtr raw_certificates :: Ptr ())]

-- | ttlsInnerAuthentication
--
-- Specifies the inner authentication used by the TTLS module.   Possible values are PAP, CHAP, MSCHAP, MSCHAPv2, and EAP. Defaults to EAP.
--
-- ObjC selector: @- ttlsInnerAuthenticationType@
ttlsInnerAuthenticationType :: IsNEHotspotEAPSettings neHotspotEAPSettings => neHotspotEAPSettings -> IO NEHotspotConfigurationTTLSInnerAuthenticationType
ttlsInnerAuthenticationType neHotspotEAPSettings  =
  fmap (coerce :: CLong -> NEHotspotConfigurationTTLSInnerAuthenticationType) $ sendMsg neHotspotEAPSettings (mkSelector "ttlsInnerAuthenticationType") retCLong []

-- | ttlsInnerAuthentication
--
-- Specifies the inner authentication used by the TTLS module.   Possible values are PAP, CHAP, MSCHAP, MSCHAPv2, and EAP. Defaults to EAP.
--
-- ObjC selector: @- setTtlsInnerAuthenticationType:@
setTtlsInnerAuthenticationType :: IsNEHotspotEAPSettings neHotspotEAPSettings => neHotspotEAPSettings -> NEHotspotConfigurationTTLSInnerAuthenticationType -> IO ()
setTtlsInnerAuthenticationType neHotspotEAPSettings  value =
  sendMsg neHotspotEAPSettings (mkSelector "setTtlsInnerAuthenticationType:") retVoid [argCLong (coerce value)]

-- | isTLSClientCertificateRequired
--
-- If YES, supports two-factor authentication for EAP-TTLS, PEAP, or EAP-FAST.   If NO, allows for zero-factor authentication for EAP-TLS. The default is YES for EAP-TLS,   and NO for other EAP types.
--
-- ObjC selector: @- tlsClientCertificateRequired@
tlsClientCertificateRequired :: IsNEHotspotEAPSettings neHotspotEAPSettings => neHotspotEAPSettings -> IO Bool
tlsClientCertificateRequired neHotspotEAPSettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neHotspotEAPSettings (mkSelector "tlsClientCertificateRequired") retCULong []

-- | isTLSClientCertificateRequired
--
-- If YES, supports two-factor authentication for EAP-TTLS, PEAP, or EAP-FAST.   If NO, allows for zero-factor authentication for EAP-TLS. The default is YES for EAP-TLS,   and NO for other EAP types.
--
-- ObjC selector: @- setTlsClientCertificateRequired:@
setTlsClientCertificateRequired :: IsNEHotspotEAPSettings neHotspotEAPSettings => neHotspotEAPSettings -> Bool -> IO ()
setTlsClientCertificateRequired neHotspotEAPSettings  value =
  sendMsg neHotspotEAPSettings (mkSelector "setTlsClientCertificateRequired:") retVoid [argCULong (if value then 1 else 0)]

-- | preferredTLSVersion
--
-- TLS version to use during the TLS handshake.   Default value is NEHotspotConfigurationEAPTLSVersion_1_2.
--
-- ObjC selector: @- preferredTLSVersion@
preferredTLSVersion :: IsNEHotspotEAPSettings neHotspotEAPSettings => neHotspotEAPSettings -> IO NEHotspotConfigurationEAPTLSVersion
preferredTLSVersion neHotspotEAPSettings  =
  fmap (coerce :: CLong -> NEHotspotConfigurationEAPTLSVersion) $ sendMsg neHotspotEAPSettings (mkSelector "preferredTLSVersion") retCLong []

-- | preferredTLSVersion
--
-- TLS version to use during the TLS handshake.   Default value is NEHotspotConfigurationEAPTLSVersion_1_2.
--
-- ObjC selector: @- setPreferredTLSVersion:@
setPreferredTLSVersion :: IsNEHotspotEAPSettings neHotspotEAPSettings => neHotspotEAPSettings -> NEHotspotConfigurationEAPTLSVersion -> IO ()
setPreferredTLSVersion neHotspotEAPSettings  value =
  sendMsg neHotspotEAPSettings (mkSelector "setPreferredTLSVersion:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setIdentity:@
setIdentitySelector :: Selector
setIdentitySelector = mkSelector "setIdentity:"

-- | @Selector@ for @setTrustedServerCertificates:@
setTrustedServerCertificatesSelector :: Selector
setTrustedServerCertificatesSelector = mkSelector "setTrustedServerCertificates:"

-- | @Selector@ for @ttlsInnerAuthenticationType@
ttlsInnerAuthenticationTypeSelector :: Selector
ttlsInnerAuthenticationTypeSelector = mkSelector "ttlsInnerAuthenticationType"

-- | @Selector@ for @setTtlsInnerAuthenticationType:@
setTtlsInnerAuthenticationTypeSelector :: Selector
setTtlsInnerAuthenticationTypeSelector = mkSelector "setTtlsInnerAuthenticationType:"

-- | @Selector@ for @tlsClientCertificateRequired@
tlsClientCertificateRequiredSelector :: Selector
tlsClientCertificateRequiredSelector = mkSelector "tlsClientCertificateRequired"

-- | @Selector@ for @setTlsClientCertificateRequired:@
setTlsClientCertificateRequiredSelector :: Selector
setTlsClientCertificateRequiredSelector = mkSelector "setTlsClientCertificateRequired:"

-- | @Selector@ for @preferredTLSVersion@
preferredTLSVersionSelector :: Selector
preferredTLSVersionSelector = mkSelector "preferredTLSVersion"

-- | @Selector@ for @setPreferredTLSVersion:@
setPreferredTLSVersionSelector :: Selector
setPreferredTLSVersionSelector = mkSelector "setPreferredTLSVersion:"

