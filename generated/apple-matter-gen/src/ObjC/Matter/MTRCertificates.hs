{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCertificates@.
module ObjC.Matter.MTRCertificates
  ( MTRCertificates
  , IsMTRCertificates(..)
  , init_
  , new
  , createRootCertificate_issuerID_fabricID_validityPeriod_error
  , createRootCertificate_issuerID_fabricID_error
  , createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_validityPeriod_error
  , createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_error
  , createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_validityPeriod_error
  , createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_error
  , keypair_matchesCertificate
  , isCertificate_equalTo
  , createCertificateSigningRequest_error
  , convertX509Certificate
  , convertMatterCertificate
  , publicKeyFromCSR_error
  , generateRootCertificate_issuerId_fabricId_error
  , generateIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerId_fabricId_error
  , generateOperationalCertificate_signingCertificate_operationalPublicKey_fabricId_nodeId_caseAuthenticatedTags_error
  , generateCertificateSigningRequest_error
  , convertMatterCertificateSelector
  , convertX509CertificateSelector
  , createCertificateSigningRequest_errorSelector
  , createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_errorSelector
  , createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_validityPeriod_errorSelector
  , createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_errorSelector
  , createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_validityPeriod_errorSelector
  , createRootCertificate_issuerID_fabricID_errorSelector
  , createRootCertificate_issuerID_fabricID_validityPeriod_errorSelector
  , generateCertificateSigningRequest_errorSelector
  , generateIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerId_fabricId_errorSelector
  , generateOperationalCertificate_signingCertificate_operationalPublicKey_fabricId_nodeId_caseAuthenticatedTags_errorSelector
  , generateRootCertificate_issuerId_fabricId_errorSelector
  , initSelector
  , isCertificate_equalToSelector
  , keypair_matchesCertificateSelector
  , newSelector
  , publicKeyFromCSR_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRCertificates mtrCertificates => mtrCertificates -> IO (Id MTRCertificates)
init_ mtrCertificates =
  sendOwnedMessage mtrCertificates initSelector

-- | @+ new@
new :: IO (Id MTRCertificates)
new  =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendOwnedClassMessage cls' newSelector

-- | Create a root (self-signed) X.509 DER encoded certificate that has the right fields to be a valid Matter root certificate.
--
-- If issuerID is not nil, it's unsignedLongLongValue will be used for the matter-rcac-id attribute in the subject distinguished name of the resulting certificate.
--
-- If issuerID is nil, a random value will be generated for matter-rcac-id.
--
-- If fabricID is not nil, it will be included in the subject DN of the certificate.  In this case it must be a valid Matter fabric id.
--
-- validityPeriod specifies when the certificate will be valid. Note that there is currently no mechanism available in Matter to update or rotate the root certificate of a fabric installed on a device. A certificate with no expiration time can be created by specifying [NSDate distantFuture] for the end of the period.
--
-- On failure returns nil and if "error" is not null sets *error to the relevant error.
--
-- ObjC selector: @+ createRootCertificate:issuerID:fabricID:validityPeriod:error:@
createRootCertificate_issuerID_fabricID_validityPeriod_error :: (IsNSNumber issuerID, IsNSNumber fabricID, IsNSDateInterval validityPeriod, IsNSError error_) => RawId -> issuerID -> fabricID -> validityPeriod -> error_ -> IO (Id NSData)
createRootCertificate_issuerID_fabricID_validityPeriod_error keypair issuerID fabricID validityPeriod error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' createRootCertificate_issuerID_fabricID_validityPeriod_errorSelector keypair (toNSNumber issuerID) (toNSNumber fabricID) (toNSDateInterval validityPeriod) (toNSError error_)

-- | As above, but defaults to no expiration time.
--
-- ObjC selector: @+ createRootCertificate:issuerID:fabricID:error:@
createRootCertificate_issuerID_fabricID_error :: (IsNSNumber issuerID, IsNSNumber fabricID, IsNSError error_) => RawId -> issuerID -> fabricID -> error_ -> IO (Id NSData)
createRootCertificate_issuerID_fabricID_error keypair issuerID fabricID error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' createRootCertificate_issuerID_fabricID_errorSelector keypair (toNSNumber issuerID) (toNSNumber fabricID) (toNSError error_)

-- | Create an intermediate X.509 DER encoded certificate that has the right fields to be a valid Matter intermediate certificate.
--
-- If issuerID is not nil, it's unsignedLongLongValue will be used for the matter-icac-id attribute in the subject distinguished name of the resulting certificate.
--
-- If issuerID is nil, a random value will be generated for matter-icac-id.
--
-- If fabricID is not nil, it will be included in the subject DN of the certificate.  In this case it must be a valid Matter fabric id.
--
-- validityPeriod specifies when the certificate will be valid. A certificate with no expiration time can be created by specifying [NSDate distantFuture] for the end of the period.
--
-- On failure returns nil and if "error" is not null sets *error to the relevant error.
--
-- ObjC selector: @+ createIntermediateCertificate:rootCertificate:intermediatePublicKey:issuerID:fabricID:validityPeriod:error:@
createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_validityPeriod_error :: (IsNSData rootCertificate, IsNSNumber issuerID, IsNSNumber fabricID, IsNSDateInterval validityPeriod, IsNSError error_) => RawId -> rootCertificate -> Ptr () -> issuerID -> fabricID -> validityPeriod -> error_ -> IO (Id NSData)
createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_validityPeriod_error rootKeypair rootCertificate intermediatePublicKey issuerID fabricID validityPeriod error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_validityPeriod_errorSelector rootKeypair (toNSData rootCertificate) intermediatePublicKey (toNSNumber issuerID) (toNSNumber fabricID) (toNSDateInterval validityPeriod) (toNSError error_)

-- | As above, but defaults to no expiration time.
--
-- ObjC selector: @+ createIntermediateCertificate:rootCertificate:intermediatePublicKey:issuerID:fabricID:error:@
createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_error :: (IsNSData rootCertificate, IsNSNumber issuerID, IsNSNumber fabricID, IsNSError error_) => RawId -> rootCertificate -> Ptr () -> issuerID -> fabricID -> error_ -> IO (Id NSData)
createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_error rootKeypair rootCertificate intermediatePublicKey issuerID fabricID error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_errorSelector rootKeypair (toNSData rootCertificate) intermediatePublicKey (toNSNumber issuerID) (toNSNumber fabricID) (toNSError error_)

-- | Create an X.509 DER encoded certificate that has the right fields to be a valid Matter operational certificate.
--
-- signingKeypair and signingCertificate are the root or intermediate that is signing the operational certificate.
--
-- nodeID and fabricID are expected to be 64-bit unsigned integers.
--
-- nodeID must be a valid Matter operational node id.
--
-- fabricID must be a valid Matter fabric id.
--
-- caseAuthenticatedTags may be nil to indicate no CASE Authenticated Tags should be used.  If caseAuthenticatedTags is not nil, it must contain at most 3 numbers, which are expected to be 32-bit unsigned Case Authenticated Tag values.
--
-- validityPeriod specifies when the certificate will be valid. A certificate with no expiration time can be created by specifying [NSDate distantFuture] for the end of the period.
--
-- On failure returns nil and if "error" is not null sets *error to the relevant error.
--
-- ObjC selector: @+ createOperationalCertificate:signingCertificate:operationalPublicKey:fabricID:nodeID:caseAuthenticatedTags:validityPeriod:error:@
createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_validityPeriod_error :: (IsNSData signingCertificate, IsNSNumber fabricID, IsNSNumber nodeID, IsNSSet caseAuthenticatedTags, IsNSDateInterval validityPeriod, IsNSError error_) => RawId -> signingCertificate -> Ptr () -> fabricID -> nodeID -> caseAuthenticatedTags -> validityPeriod -> error_ -> IO (Id NSData)
createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_validityPeriod_error signingKeypair signingCertificate operationalPublicKey fabricID nodeID caseAuthenticatedTags validityPeriod error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_validityPeriod_errorSelector signingKeypair (toNSData signingCertificate) operationalPublicKey (toNSNumber fabricID) (toNSNumber nodeID) (toNSSet caseAuthenticatedTags) (toNSDateInterval validityPeriod) (toNSError error_)

-- | As above, but defaults to no expiration time.
--
-- ObjC selector: @+ createOperationalCertificate:signingCertificate:operationalPublicKey:fabricID:nodeID:caseAuthenticatedTags:error:@
createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_error :: (IsNSData signingCertificate, IsNSNumber fabricID, IsNSNumber nodeID, IsNSSet caseAuthenticatedTags, IsNSError error_) => RawId -> signingCertificate -> Ptr () -> fabricID -> nodeID -> caseAuthenticatedTags -> error_ -> IO (Id NSData)
createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_error signingKeypair signingCertificate operationalPublicKey fabricID nodeID caseAuthenticatedTags error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_errorSelector signingKeypair (toNSData signingCertificate) operationalPublicKey (toNSNumber fabricID) (toNSNumber nodeID) (toNSSet caseAuthenticatedTags) (toNSError error_)

-- | Check whether the given keypair's public key matches the given certificate's public key.  The certificate is expected to be an X.509 DER encoded certificate.
--
-- Will return NO on failures to extract public keys from the objects.
--
-- ObjC selector: @+ keypair:matchesCertificate:@
keypair_matchesCertificate :: IsNSData certificate => RawId -> certificate -> IO Bool
keypair_matchesCertificate keypair certificate =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' keypair_matchesCertificateSelector keypair (toNSData certificate)

-- | Check whether two X.509 DER encoded certificates are equivalent, in the sense of having the same public key and the same subject DN.  Returns NO if public keys or subject DNs cannot be extracted from the certificates.
--
-- ObjC selector: @+ isCertificate:equalTo:@
isCertificate_equalTo :: (IsNSData certificate1, IsNSData certificate2) => certificate1 -> certificate2 -> IO Bool
isCertificate_equalTo certificate1 certificate2 =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' isCertificate_equalToSelector (toNSData certificate1) (toNSData certificate2)

-- | Generate a PKCS#10 certificate signing request from a MTRKeypair.  This can then be used to request an operational or ICA certificate from an external certificate authority.
--
-- The CSR will have the subject OU DN set to 'CSA', because omitting all identifying information altogether often trips up CSR parsing code.  The CA being used should expect this and ignore the request subject, producing a subject that matches the rules for Matter certificates.
--
-- On failure returns nil and if "error" is not null sets *error to the relevant error.
--
-- ObjC selector: @+ createCertificateSigningRequest:error:@
createCertificateSigningRequest_error :: IsNSError error_ => RawId -> error_ -> IO (Id NSData)
createCertificateSigningRequest_error keypair error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' createCertificateSigningRequest_errorSelector keypair (toNSError error_)

-- | Convert the given X.509v3 DER encoded certificate to the Matter certificate format.
--
-- Returns nil if the conversion fails (e.g. if the input data cannot be parsed as a DER encoded X.509 certificate, or if the certificate cannot be represented in the Matter certificate format).
--
-- ObjC selector: @+ convertX509Certificate:@
convertX509Certificate :: IsNSData x509Certificate => x509Certificate -> IO (Id NSData)
convertX509Certificate x509Certificate =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' convertX509CertificateSelector (toNSData x509Certificate)

-- | Convert the given Matter TLV encoded certificate to the X.509v3 DER encoded format.
--
-- Returns nil if the conversion fails (e.g. if the input data cannot be parsed as a Matter TLV encoded certificate, or if the certificate cannot be represented in the X.509v3 DER format).
--
-- ObjC selector: @+ convertMatterCertificate:@
convertMatterCertificate :: IsNSData matterCertificate => matterCertificate -> IO (Id NSData)
convertMatterCertificate matterCertificate =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' convertMatterCertificateSelector (toNSData matterCertificate)

-- | Extract the public key from the given PKCS#10 certificate signing request. This is the public key that a certificate issued in response to the request would need to have.
--
-- ObjC selector: @+ publicKeyFromCSR:error:@
publicKeyFromCSR_error :: (IsNSData csr, IsNSError error_) => csr -> error_ -> IO (Id NSData)
publicKeyFromCSR_error csr error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' publicKeyFromCSR_errorSelector (toNSData csr) (toNSError error_)

-- | @+ generateRootCertificate:issuerId:fabricId:error:@
generateRootCertificate_issuerId_fabricId_error :: (IsNSNumber issuerId, IsNSNumber fabricId, IsNSError error_) => RawId -> issuerId -> fabricId -> error_ -> IO (Id NSData)
generateRootCertificate_issuerId_fabricId_error keypair issuerId fabricId error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' generateRootCertificate_issuerId_fabricId_errorSelector keypair (toNSNumber issuerId) (toNSNumber fabricId) (toNSError error_)

-- | @+ generateIntermediateCertificate:rootCertificate:intermediatePublicKey:issuerId:fabricId:error:@
generateIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerId_fabricId_error :: (IsNSData rootCertificate, IsNSNumber issuerId, IsNSNumber fabricId, IsNSError error_) => RawId -> rootCertificate -> Ptr () -> issuerId -> fabricId -> error_ -> IO (Id NSData)
generateIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerId_fabricId_error rootKeypair rootCertificate intermediatePublicKey issuerId fabricId error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' generateIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerId_fabricId_errorSelector rootKeypair (toNSData rootCertificate) intermediatePublicKey (toNSNumber issuerId) (toNSNumber fabricId) (toNSError error_)

-- | @+ generateOperationalCertificate:signingCertificate:operationalPublicKey:fabricId:nodeId:caseAuthenticatedTags:error:@
generateOperationalCertificate_signingCertificate_operationalPublicKey_fabricId_nodeId_caseAuthenticatedTags_error :: (IsNSData signingCertificate, IsNSNumber fabricId, IsNSNumber nodeId, IsNSArray caseAuthenticatedTags, IsNSError error_) => RawId -> signingCertificate -> Ptr () -> fabricId -> nodeId -> caseAuthenticatedTags -> error_ -> IO (Id NSData)
generateOperationalCertificate_signingCertificate_operationalPublicKey_fabricId_nodeId_caseAuthenticatedTags_error signingKeypair signingCertificate operationalPublicKey fabricId nodeId caseAuthenticatedTags error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' generateOperationalCertificate_signingCertificate_operationalPublicKey_fabricId_nodeId_caseAuthenticatedTags_errorSelector signingKeypair (toNSData signingCertificate) operationalPublicKey (toNSNumber fabricId) (toNSNumber nodeId) (toNSArray caseAuthenticatedTags) (toNSError error_)

-- | @+ generateCertificateSigningRequest:error:@
generateCertificateSigningRequest_error :: IsNSError error_ => RawId -> error_ -> IO (Id NSData)
generateCertificateSigningRequest_error keypair error_ =
  do
    cls' <- getRequiredClass "MTRCertificates"
    sendClassMessage cls' generateCertificateSigningRequest_errorSelector keypair (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRCertificates)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRCertificates)
newSelector = mkSelector "new"

-- | @Selector@ for @createRootCertificate:issuerID:fabricID:validityPeriod:error:@
createRootCertificate_issuerID_fabricID_validityPeriod_errorSelector :: Selector '[RawId, Id NSNumber, Id NSNumber, Id NSDateInterval, Id NSError] (Id NSData)
createRootCertificate_issuerID_fabricID_validityPeriod_errorSelector = mkSelector "createRootCertificate:issuerID:fabricID:validityPeriod:error:"

-- | @Selector@ for @createRootCertificate:issuerID:fabricID:error:@
createRootCertificate_issuerID_fabricID_errorSelector :: Selector '[RawId, Id NSNumber, Id NSNumber, Id NSError] (Id NSData)
createRootCertificate_issuerID_fabricID_errorSelector = mkSelector "createRootCertificate:issuerID:fabricID:error:"

-- | @Selector@ for @createIntermediateCertificate:rootCertificate:intermediatePublicKey:issuerID:fabricID:validityPeriod:error:@
createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_validityPeriod_errorSelector :: Selector '[RawId, Id NSData, Ptr (), Id NSNumber, Id NSNumber, Id NSDateInterval, Id NSError] (Id NSData)
createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_validityPeriod_errorSelector = mkSelector "createIntermediateCertificate:rootCertificate:intermediatePublicKey:issuerID:fabricID:validityPeriod:error:"

-- | @Selector@ for @createIntermediateCertificate:rootCertificate:intermediatePublicKey:issuerID:fabricID:error:@
createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_errorSelector :: Selector '[RawId, Id NSData, Ptr (), Id NSNumber, Id NSNumber, Id NSError] (Id NSData)
createIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerID_fabricID_errorSelector = mkSelector "createIntermediateCertificate:rootCertificate:intermediatePublicKey:issuerID:fabricID:error:"

-- | @Selector@ for @createOperationalCertificate:signingCertificate:operationalPublicKey:fabricID:nodeID:caseAuthenticatedTags:validityPeriod:error:@
createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_validityPeriod_errorSelector :: Selector '[RawId, Id NSData, Ptr (), Id NSNumber, Id NSNumber, Id NSSet, Id NSDateInterval, Id NSError] (Id NSData)
createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_validityPeriod_errorSelector = mkSelector "createOperationalCertificate:signingCertificate:operationalPublicKey:fabricID:nodeID:caseAuthenticatedTags:validityPeriod:error:"

-- | @Selector@ for @createOperationalCertificate:signingCertificate:operationalPublicKey:fabricID:nodeID:caseAuthenticatedTags:error:@
createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_errorSelector :: Selector '[RawId, Id NSData, Ptr (), Id NSNumber, Id NSNumber, Id NSSet, Id NSError] (Id NSData)
createOperationalCertificate_signingCertificate_operationalPublicKey_fabricID_nodeID_caseAuthenticatedTags_errorSelector = mkSelector "createOperationalCertificate:signingCertificate:operationalPublicKey:fabricID:nodeID:caseAuthenticatedTags:error:"

-- | @Selector@ for @keypair:matchesCertificate:@
keypair_matchesCertificateSelector :: Selector '[RawId, Id NSData] Bool
keypair_matchesCertificateSelector = mkSelector "keypair:matchesCertificate:"

-- | @Selector@ for @isCertificate:equalTo:@
isCertificate_equalToSelector :: Selector '[Id NSData, Id NSData] Bool
isCertificate_equalToSelector = mkSelector "isCertificate:equalTo:"

-- | @Selector@ for @createCertificateSigningRequest:error:@
createCertificateSigningRequest_errorSelector :: Selector '[RawId, Id NSError] (Id NSData)
createCertificateSigningRequest_errorSelector = mkSelector "createCertificateSigningRequest:error:"

-- | @Selector@ for @convertX509Certificate:@
convertX509CertificateSelector :: Selector '[Id NSData] (Id NSData)
convertX509CertificateSelector = mkSelector "convertX509Certificate:"

-- | @Selector@ for @convertMatterCertificate:@
convertMatterCertificateSelector :: Selector '[Id NSData] (Id NSData)
convertMatterCertificateSelector = mkSelector "convertMatterCertificate:"

-- | @Selector@ for @publicKeyFromCSR:error:@
publicKeyFromCSR_errorSelector :: Selector '[Id NSData, Id NSError] (Id NSData)
publicKeyFromCSR_errorSelector = mkSelector "publicKeyFromCSR:error:"

-- | @Selector@ for @generateRootCertificate:issuerId:fabricId:error:@
generateRootCertificate_issuerId_fabricId_errorSelector :: Selector '[RawId, Id NSNumber, Id NSNumber, Id NSError] (Id NSData)
generateRootCertificate_issuerId_fabricId_errorSelector = mkSelector "generateRootCertificate:issuerId:fabricId:error:"

-- | @Selector@ for @generateIntermediateCertificate:rootCertificate:intermediatePublicKey:issuerId:fabricId:error:@
generateIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerId_fabricId_errorSelector :: Selector '[RawId, Id NSData, Ptr (), Id NSNumber, Id NSNumber, Id NSError] (Id NSData)
generateIntermediateCertificate_rootCertificate_intermediatePublicKey_issuerId_fabricId_errorSelector = mkSelector "generateIntermediateCertificate:rootCertificate:intermediatePublicKey:issuerId:fabricId:error:"

-- | @Selector@ for @generateOperationalCertificate:signingCertificate:operationalPublicKey:fabricId:nodeId:caseAuthenticatedTags:error:@
generateOperationalCertificate_signingCertificate_operationalPublicKey_fabricId_nodeId_caseAuthenticatedTags_errorSelector :: Selector '[RawId, Id NSData, Ptr (), Id NSNumber, Id NSNumber, Id NSArray, Id NSError] (Id NSData)
generateOperationalCertificate_signingCertificate_operationalPublicKey_fabricId_nodeId_caseAuthenticatedTags_errorSelector = mkSelector "generateOperationalCertificate:signingCertificate:operationalPublicKey:fabricId:nodeId:caseAuthenticatedTags:error:"

-- | @Selector@ for @generateCertificateSigningRequest:error:@
generateCertificateSigningRequest_errorSelector :: Selector '[RawId, Id NSError] (Id NSData)
generateCertificateSigningRequest_errorSelector = mkSelector "generateCertificateSigningRequest:error:"

