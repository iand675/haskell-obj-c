{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Exposes Matter-specific information from an operational X.509 certificate.
--
-- Note: This class does not support parsing certificates related to Device Attestation.
--
-- Generated bindings for @MTRCertificateInfo@.
module ObjC.Matter.MTRCertificateInfo
  ( MTRCertificateInfo
  , IsMTRCertificateInfo(..)
  , new
  , init_
  , initWithTLVBytes
  , issuer
  , subject
  , notBefore
  , notAfter
  , publicKeyData
  , initSelector
  , initWithTLVBytesSelector
  , issuerSelector
  , newSelector
  , notAfterSelector
  , notBeforeSelector
  , publicKeyDataSelector
  , subjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MTRCertificateInfo)
new  =
  do
    cls' <- getRequiredClass "MTRCertificateInfo"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id MTRCertificateInfo)
init_ mtrCertificateInfo =
  sendOwnedMessage mtrCertificateInfo initSelector

-- | Initializes the receiver with an operational certificate in Matter TLV format.
--
-- This can be a node operational certificate, a Matter intermediate certificate, or a Matter root certificate.
--
-- ObjC selector: @- initWithTLVBytes:@
initWithTLVBytes :: (IsMTRCertificateInfo mtrCertificateInfo, IsNSData bytes) => mtrCertificateInfo -> bytes -> IO (Id MTRCertificateInfo)
initWithTLVBytes mtrCertificateInfo bytes =
  sendOwnedMessage mtrCertificateInfo initWithTLVBytesSelector (toNSData bytes)

-- | The Distinguished Name of the issuer of the certificate.
--
-- For a node operational certificate, the issuer will match the subject of the root certificate or intermediate certificate that represents the entity that issued the node operational certificate.
--
-- For an intermediate certificate, the issuer will match the subject of the root certificate.
--
-- Matter root certificates are self-signed, i.e. the issuer and the subject are the same.
--
-- ObjC selector: @- issuer@
issuer :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id MTRDistinguishedNameInfo)
issuer mtrCertificateInfo =
  sendMessage mtrCertificateInfo issuerSelector

-- | The Distinguished Name of the entity represented by the certificate.
--
-- ObjC selector: @- subject@
subject :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id MTRDistinguishedNameInfo)
subject mtrCertificateInfo =
  sendMessage mtrCertificateInfo subjectSelector

-- | @- notBefore@
notBefore :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id NSDate)
notBefore mtrCertificateInfo =
  sendMessage mtrCertificateInfo notBeforeSelector

-- | @- notAfter@
notAfter :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id NSDate)
notAfter mtrCertificateInfo =
  sendMessage mtrCertificateInfo notAfterSelector

-- | Public key data for this certificate
--
-- ObjC selector: @- publicKeyData@
publicKeyData :: IsMTRCertificateInfo mtrCertificateInfo => mtrCertificateInfo -> IO (Id NSData)
publicKeyData mtrCertificateInfo =
  sendMessage mtrCertificateInfo publicKeyDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRCertificateInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRCertificateInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTLVBytes:@
initWithTLVBytesSelector :: Selector '[Id NSData] (Id MTRCertificateInfo)
initWithTLVBytesSelector = mkSelector "initWithTLVBytes:"

-- | @Selector@ for @issuer@
issuerSelector :: Selector '[] (Id MTRDistinguishedNameInfo)
issuerSelector = mkSelector "issuer"

-- | @Selector@ for @subject@
subjectSelector :: Selector '[] (Id MTRDistinguishedNameInfo)
subjectSelector = mkSelector "subject"

-- | @Selector@ for @notBefore@
notBeforeSelector :: Selector '[] (Id NSDate)
notBeforeSelector = mkSelector "notBefore"

-- | @Selector@ for @notAfter@
notAfterSelector :: Selector '[] (Id NSDate)
notAfterSelector = mkSelector "notAfter"

-- | @Selector@ for @publicKeyData@
publicKeyDataSelector :: Selector '[] (Id NSData)
publicKeyDataSelector = mkSelector "publicKeyData"

