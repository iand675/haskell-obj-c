{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceControllerStartupParams@.
module ObjC.Matter.MTRDeviceControllerStartupParams
  ( MTRDeviceControllerStartupParams
  , IsMTRDeviceControllerStartupParams(..)
  , init_
  , new
  , initWithIPK_fabricID_nocSigner
  , initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate
  , initWithSigningKeypair_fabricId_ipk
  , initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipk
  , nocSigner
  , fabricID
  , ipk
  , vendorID
  , setVendorID
  , nodeID
  , setNodeID
  , caseAuthenticatedTags
  , setCaseAuthenticatedTags
  , rootCertificate
  , setRootCertificate
  , intermediateCertificate
  , setIntermediateCertificate
  , operationalCertificate
  , operationalKeypair
  , setOperationalKeypair
  , operationalCertificateIssuer
  , setOperationalCertificateIssuer
  , operationalCertificateIssuerQueue
  , setOperationalCertificateIssuerQueue
  , fabricId
  , vendorId
  , setVendorId
  , nodeId
  , setNodeId
  , caseAuthenticatedTagsSelector
  , fabricIDSelector
  , fabricIdSelector
  , initSelector
  , initWithIPK_fabricID_nocSignerSelector
  , initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector
  , initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipkSelector
  , initWithSigningKeypair_fabricId_ipkSelector
  , intermediateCertificateSelector
  , ipkSelector
  , newSelector
  , nocSignerSelector
  , nodeIDSelector
  , nodeIdSelector
  , operationalCertificateIssuerQueueSelector
  , operationalCertificateIssuerSelector
  , operationalCertificateSelector
  , operationalKeypairSelector
  , rootCertificateSelector
  , setCaseAuthenticatedTagsSelector
  , setIntermediateCertificateSelector
  , setNodeIDSelector
  , setNodeIdSelector
  , setOperationalCertificateIssuerQueueSelector
  , setOperationalCertificateIssuerSelector
  , setOperationalKeypairSelector
  , setRootCertificateSelector
  , setVendorIDSelector
  , setVendorIdSelector
  , vendorIDSelector
  , vendorIdSelector


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
init_ :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id MTRDeviceControllerStartupParams)
init_ mtrDeviceControllerStartupParams =
  sendOwnedMessage mtrDeviceControllerStartupParams initSelector

-- | @+ new@
new :: IO (Id MTRDeviceControllerStartupParams)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceControllerStartupParams"
    sendOwnedClassMessage cls' newSelector

-- | Prepare to initialize a controller given a keypair to use for signing operational certificates.
--
-- A controller created from MTRDeviceControllerStartupParams initialized with this method will be able to issue operational certificates to devices it commissions, using nocSigner to sign them.
--
-- @ipk@ — The Identity Protection Key, must be 16 bytes in length
--
-- @fabricID@ — The fabric identifier, must be non-zero.
--
-- ObjC selector: @- initWithIPK:fabricID:nocSigner:@
initWithIPK_fabricID_nocSigner :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSData ipk, IsNSNumber fabricID) => mtrDeviceControllerStartupParams -> ipk -> fabricID -> RawId -> IO (Id MTRDeviceControllerStartupParams)
initWithIPK_fabricID_nocSigner mtrDeviceControllerStartupParams ipk fabricID nocSigner =
  sendOwnedMessage mtrDeviceControllerStartupParams initWithIPK_fabricID_nocSignerSelector (toNSData ipk) (toNSNumber fabricID) nocSigner

-- | Prepare to initialize a controller that is not able to sign operational certificates itself, and therefore needs to be provided with a complete operational certificate chain.  This initialization method should be used when none of the certificate-signing private keys are available locally.
--
-- A controller created from MTRDeviceControllerStartupParams initialized with this method will not be able to commission devices unless operationalCertificateIssuer and operationalCertificateIssuerQueue are set.
--
-- The fabric id and node id to use for the controller will be derived from the provided operationalCertificate.
--
-- @ipk@ — The Identity Protection Key, must be 16 bytes in length
--
-- @intermediateCertificate@ — may be nil if operationalCertificate is directly signed by rootCertificate.
--
-- ObjC selector: @- initWithIPK:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:@
initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSData ipk, IsNSData operationalCertificate, IsNSData intermediateCertificate, IsNSData rootCertificate) => mtrDeviceControllerStartupParams -> ipk -> RawId -> operationalCertificate -> intermediateCertificate -> rootCertificate -> IO (Id MTRDeviceControllerStartupParams)
initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate mtrDeviceControllerStartupParams ipk operationalKeypair operationalCertificate intermediateCertificate rootCertificate =
  sendOwnedMessage mtrDeviceControllerStartupParams initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector (toNSData ipk) operationalKeypair (toNSData operationalCertificate) (toNSData intermediateCertificate) (toNSData rootCertificate)

-- | @- initWithSigningKeypair:fabricId:ipk:@
initWithSigningKeypair_fabricId_ipk :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSData ipk) => mtrDeviceControllerStartupParams -> RawId -> CULong -> ipk -> IO (Id MTRDeviceControllerStartupParams)
initWithSigningKeypair_fabricId_ipk mtrDeviceControllerStartupParams nocSigner fabricId ipk =
  sendOwnedMessage mtrDeviceControllerStartupParams initWithSigningKeypair_fabricId_ipkSelector nocSigner fabricId (toNSData ipk)

-- | @- initWithOperationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:ipk:@
initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipk :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSData operationalCertificate, IsNSData intermediateCertificate, IsNSData rootCertificate, IsNSData ipk) => mtrDeviceControllerStartupParams -> RawId -> operationalCertificate -> intermediateCertificate -> rootCertificate -> ipk -> IO (Id MTRDeviceControllerStartupParams)
initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipk mtrDeviceControllerStartupParams operationalKeypair operationalCertificate intermediateCertificate rootCertificate ipk =
  sendOwnedMessage mtrDeviceControllerStartupParams initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipkSelector operationalKeypair (toNSData operationalCertificate) (toNSData intermediateCertificate) (toNSData rootCertificate) (toNSData ipk)

-- | Keypair used to sign operational certificates.  This is the root CA keypair if not using an intermediate CA, the intermediate CA's keypair otherwise.
--
-- Allowed to be nil if this controller will not be issuing internally-generated operational certificates.  In that case, the MTRDeviceControllerStartupParams object must be initialized using initWithIPK:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate: (to provide the operational credentials for t2he controller itself).
--
-- ObjC selector: @- nocSigner@
nocSigner :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO RawId
nocSigner mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams nocSignerSelector

-- | Fabric id for the controller.  Must be set to a nonzero value.  This is scoped by the root public key, which is determined as follows:
--
-- * If a root certificate is provided, it is the public key of the root   certificate.
--
-- * If a root certificate is not provided, the root public key is the public   key of the nocSigner keypair, since in this case we are not using an   intermediate certificate.
--
-- ObjC selector: @- fabricID@
fabricID :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSNumber)
fabricID mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams fabricIDSelector

-- | IPK to use for the controller's fabric.  Allowed to change from the last time a controller was started on this fabric if a new IPK has been distributed to all the devices the controller wants to interact with.
--
-- ObjC selector: @- ipk@
ipk :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSData)
ipk mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams ipkSelector

-- | Vendor ID (allocated by the Connectivity Standards Alliance) for this controller.
--
-- If not nil, must not be the "standard" vendor id (0).
--
-- When creating a new fabric:
--
-- * Must not be nil.
--
-- When using an existing fabric:
--
-- * Will override existing value if not nil. Otherwise existing value will be   used.
--
-- ObjC selector: @- vendorID@
vendorID :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSNumber)
vendorID mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams vendorIDSelector

-- | Vendor ID (allocated by the Connectivity Standards Alliance) for this controller.
--
-- If not nil, must not be the "standard" vendor id (0).
--
-- When creating a new fabric:
--
-- * Must not be nil.
--
-- When using an existing fabric:
--
-- * Will override existing value if not nil. Otherwise existing value will be   used.
--
-- ObjC selector: @- setVendorID:@
setVendorID :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSNumber value) => mtrDeviceControllerStartupParams -> value -> IO ()
setVendorID mtrDeviceControllerStartupParams value =
  sendMessage mtrDeviceControllerStartupParams setVendorIDSelector (toNSNumber value)

-- | Node id for this controller.
--
-- If operationalCertificate is not nil, must be nil.  The provided operational certificate will be used as-is.
--
-- If not nil, must be a valid Matter operational node id.
--
-- If operationalCertificate is nil, nodeID and operationalKeypair are used to determine an operational certificate, as follows:
--
-- * When creating a new fabric:
--
-- ** nodeID is allowed to be nil to indicate that a random node id should be    generated.
--
-- * When using an existing fabric:
--
-- ** nodeID is allowed to be nil to indicate that the existing operational node    id should be used.  The existing operational keys will also be used,    unless operationalKeypair is provided.  The existing caseAuthenticatedTags    will be used.
--
-- ** If nodeID is not nil, a new operational certificate will be generated for    the provided node id (even if that matches the existing node id), using    either the operationalKeypair if that is provided or a new randomly    generated operational key, and using the provided caseAuthenticatedTags.
--
-- ObjC selector: @- nodeID@
nodeID :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSNumber)
nodeID mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams nodeIDSelector

-- | Node id for this controller.
--
-- If operationalCertificate is not nil, must be nil.  The provided operational certificate will be used as-is.
--
-- If not nil, must be a valid Matter operational node id.
--
-- If operationalCertificate is nil, nodeID and operationalKeypair are used to determine an operational certificate, as follows:
--
-- * When creating a new fabric:
--
-- ** nodeID is allowed to be nil to indicate that a random node id should be    generated.
--
-- * When using an existing fabric:
--
-- ** nodeID is allowed to be nil to indicate that the existing operational node    id should be used.  The existing operational keys will also be used,    unless operationalKeypair is provided.  The existing caseAuthenticatedTags    will be used.
--
-- ** If nodeID is not nil, a new operational certificate will be generated for    the provided node id (even if that matches the existing node id), using    either the operationalKeypair if that is provided or a new randomly    generated operational key, and using the provided caseAuthenticatedTags.
--
-- ObjC selector: @- setNodeID:@
setNodeID :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSNumber value) => mtrDeviceControllerStartupParams -> value -> IO ()
setNodeID mtrDeviceControllerStartupParams value =
  sendMessage mtrDeviceControllerStartupParams setNodeIDSelector (toNSNumber value)

-- | CASE authenticated tags to use for this controller's operational certificate.
--
-- Only allowed to be not nil if nodeID is not nil.  In particular, if operationalCertificate is not nil, must be nil.  The provided operational certificate will be used as-is.
--
-- If not nil, must contain at most 3 numbers, which are expected to be 32-bit unsigned Case Authenticated Tag values.
--
-- ObjC selector: @- caseAuthenticatedTags@
caseAuthenticatedTags :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSSet)
caseAuthenticatedTags mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams caseAuthenticatedTagsSelector

-- | CASE authenticated tags to use for this controller's operational certificate.
--
-- Only allowed to be not nil if nodeID is not nil.  In particular, if operationalCertificate is not nil, must be nil.  The provided operational certificate will be used as-is.
--
-- If not nil, must contain at most 3 numbers, which are expected to be 32-bit unsigned Case Authenticated Tag values.
--
-- ObjC selector: @- setCaseAuthenticatedTags:@
setCaseAuthenticatedTags :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSSet value) => mtrDeviceControllerStartupParams -> value -> IO ()
setCaseAuthenticatedTags mtrDeviceControllerStartupParams value =
  sendMessage mtrDeviceControllerStartupParams setCaseAuthenticatedTagsSelector (toNSSet value)

-- | Root certificate, in X.509 DER form, to use.
--
-- Must not be nil if an intermediate CA is being used, to allow determination of the root public key.
--
-- If not nil, and if an intermediate CA is not being used, the public key of this certificate must match the public key of nocSigner, if nocSigner is not nil.
--
-- When creating a new fabric:
--
-- * May be nil if nocSigner is not nil and an intermediate CA is not being   used.  In that case the nocSigner keypair, which is the keypair for the   root certificate, will be used to generate and sign a root certificate,   with a random issuer id.  In this case, the fabricID will be included in   the root certificate's subject DN.
--
-- When using an existing fabric:
--
-- * May be nil if nocSigner is not nil and an intermediate CA is not being   used.  In that case, the existing root certificate for the fabric will be   used.
--
-- * If not nil must satisfy the following properties:
--
-- 1) The public key must match the public key of the existing root      certificate.   2) The subject DN must match the subject DN of the existing root      certificate.
--
-- ObjC selector: @- rootCertificate@
rootCertificate :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSData)
rootCertificate mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams rootCertificateSelector

-- | Root certificate, in X.509 DER form, to use.
--
-- Must not be nil if an intermediate CA is being used, to allow determination of the root public key.
--
-- If not nil, and if an intermediate CA is not being used, the public key of this certificate must match the public key of nocSigner, if nocSigner is not nil.
--
-- When creating a new fabric:
--
-- * May be nil if nocSigner is not nil and an intermediate CA is not being   used.  In that case the nocSigner keypair, which is the keypair for the   root certificate, will be used to generate and sign a root certificate,   with a random issuer id.  In this case, the fabricID will be included in   the root certificate's subject DN.
--
-- When using an existing fabric:
--
-- * May be nil if nocSigner is not nil and an intermediate CA is not being   used.  In that case, the existing root certificate for the fabric will be   used.
--
-- * If not nil must satisfy the following properties:
--
-- 1) The public key must match the public key of the existing root      certificate.   2) The subject DN must match the subject DN of the existing root      certificate.
--
-- ObjC selector: @- setRootCertificate:@
setRootCertificate :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSData value) => mtrDeviceControllerStartupParams -> value -> IO ()
setRootCertificate mtrDeviceControllerStartupParams value =
  sendMessage mtrDeviceControllerStartupParams setRootCertificateSelector (toNSData value)

-- | Intermediate certificate, in X.509 DER form, to use.
--
-- If not nil, rootCertificate must not be nil, and the intermediate certificate must be signed by rootCertificate.
--
-- If not nil, and nocSigner is not nil, the public key of this certificate must match the public key of nocSigner.
--
-- When creating a new fabric:
--
-- * Must not be nil if an intermediate CA is being used.
--
-- * Must be nil if an intermediate CA is not being used.
--
-- When using an existing fabric:
--
-- * If not nil, will be used as the intermediate certificate for issuing   operational certificates.
--
-- * If nil:
--
-- * If nocSigner is not nil, there is an existing intermediate certificate,     and it matches the nocSigner public key, the existing intermediate     certificate will be used.
--
-- * Otherwise the fabric will not use an intermediate certificate.  This     allows switching from using an intermediate CA to not using one.
--
-- ObjC selector: @- intermediateCertificate@
intermediateCertificate :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSData)
intermediateCertificate mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams intermediateCertificateSelector

-- | Intermediate certificate, in X.509 DER form, to use.
--
-- If not nil, rootCertificate must not be nil, and the intermediate certificate must be signed by rootCertificate.
--
-- If not nil, and nocSigner is not nil, the public key of this certificate must match the public key of nocSigner.
--
-- When creating a new fabric:
--
-- * Must not be nil if an intermediate CA is being used.
--
-- * Must be nil if an intermediate CA is not being used.
--
-- When using an existing fabric:
--
-- * If not nil, will be used as the intermediate certificate for issuing   operational certificates.
--
-- * If nil:
--
-- * If nocSigner is not nil, there is an existing intermediate certificate,     and it matches the nocSigner public key, the existing intermediate     certificate will be used.
--
-- * Otherwise the fabric will not use an intermediate certificate.  This     allows switching from using an intermediate CA to not using one.
--
-- ObjC selector: @- setIntermediateCertificate:@
setIntermediateCertificate :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSData value) => mtrDeviceControllerStartupParams -> value -> IO ()
setIntermediateCertificate mtrDeviceControllerStartupParams value =
  sendMessage mtrDeviceControllerStartupParams setIntermediateCertificateSelector (toNSData value)

-- | Operational certificate, in X.509 DER form, to use.
--
-- If not nil, will be used as the operational certificate.  In this case operationalKeypair must not be nil.
--
-- If nil, an operational certificate will be determined as described in the documentation for nodeID.
--
-- ObjC selector: @- operationalCertificate@
operationalCertificate :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSData)
operationalCertificate mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams operationalCertificateSelector

-- | Operational keypair to use.  If operationalCertificate is not nil, the public key must match operationalCertificate.
--
-- If not nil, and if operationalCertificate is nil, a new operational certificate will be generated for the given operationalKeypair.  The node id for that certificate will be determined as described in the documentation for nodeID.
--
-- ObjC selector: @- operationalKeypair@
operationalKeypair :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO RawId
operationalKeypair mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams operationalKeypairSelector

-- | Operational keypair to use.  If operationalCertificate is not nil, the public key must match operationalCertificate.
--
-- If not nil, and if operationalCertificate is nil, a new operational certificate will be generated for the given operationalKeypair.  The node id for that certificate will be determined as described in the documentation for nodeID.
--
-- ObjC selector: @- setOperationalKeypair:@
setOperationalKeypair :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> RawId -> IO ()
setOperationalKeypair mtrDeviceControllerStartupParams value =
  sendMessage mtrDeviceControllerStartupParams setOperationalKeypairSelector value

-- | The certificate issuer delegate to use for issuing operational certificates when commissioning devices.  Allowed to be nil if this controller either does not issue operational certificates at all or internally generates the certificates to be issued.  In the latter case, nocSigner must not be nil.
--
-- ObjC selector: @- operationalCertificateIssuer@
operationalCertificateIssuer :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO RawId
operationalCertificateIssuer mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams operationalCertificateIssuerSelector

-- | The certificate issuer delegate to use for issuing operational certificates when commissioning devices.  Allowed to be nil if this controller either does not issue operational certificates at all or internally generates the certificates to be issued.  In the latter case, nocSigner must not be nil.
--
-- ObjC selector: @- setOperationalCertificateIssuer:@
setOperationalCertificateIssuer :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> RawId -> IO ()
setOperationalCertificateIssuer mtrDeviceControllerStartupParams value =
  sendMessage mtrDeviceControllerStartupParams setOperationalCertificateIssuerSelector value

-- | The dispatch queue on which operationalCertificateIssuer should be called. Allowed to be nil if and only if operationalCertificateIssuer is nil.
--
-- ObjC selector: @- operationalCertificateIssuerQueue@
operationalCertificateIssuerQueue :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSObject)
operationalCertificateIssuerQueue mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams operationalCertificateIssuerQueueSelector

-- | The dispatch queue on which operationalCertificateIssuer should be called. Allowed to be nil if and only if operationalCertificateIssuer is nil.
--
-- ObjC selector: @- setOperationalCertificateIssuerQueue:@
setOperationalCertificateIssuerQueue :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSObject value) => mtrDeviceControllerStartupParams -> value -> IO ()
setOperationalCertificateIssuerQueue mtrDeviceControllerStartupParams value =
  sendMessage mtrDeviceControllerStartupParams setOperationalCertificateIssuerQueueSelector (toNSObject value)

-- | @- fabricId@
fabricId :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO CULong
fabricId mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams fabricIdSelector

-- | @- vendorId@
vendorId :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSNumber)
vendorId mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams vendorIdSelector

-- | @- setVendorId:@
setVendorId :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSNumber value) => mtrDeviceControllerStartupParams -> value -> IO ()
setVendorId mtrDeviceControllerStartupParams value =
  sendMessage mtrDeviceControllerStartupParams setVendorIdSelector (toNSNumber value)

-- | @- nodeId@
nodeId :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSNumber)
nodeId mtrDeviceControllerStartupParams =
  sendMessage mtrDeviceControllerStartupParams nodeIdSelector

-- | @- setNodeId:@
setNodeId :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSNumber value) => mtrDeviceControllerStartupParams -> value -> IO ()
setNodeId mtrDeviceControllerStartupParams value =
  sendMessage mtrDeviceControllerStartupParams setNodeIdSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRDeviceControllerStartupParams)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRDeviceControllerStartupParams)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithIPK:fabricID:nocSigner:@
initWithIPK_fabricID_nocSignerSelector :: Selector '[Id NSData, Id NSNumber, RawId] (Id MTRDeviceControllerStartupParams)
initWithIPK_fabricID_nocSignerSelector = mkSelector "initWithIPK:fabricID:nocSigner:"

-- | @Selector@ for @initWithIPK:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:@
initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector :: Selector '[Id NSData, RawId, Id NSData, Id NSData, Id NSData] (Id MTRDeviceControllerStartupParams)
initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector = mkSelector "initWithIPK:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:"

-- | @Selector@ for @initWithSigningKeypair:fabricId:ipk:@
initWithSigningKeypair_fabricId_ipkSelector :: Selector '[RawId, CULong, Id NSData] (Id MTRDeviceControllerStartupParams)
initWithSigningKeypair_fabricId_ipkSelector = mkSelector "initWithSigningKeypair:fabricId:ipk:"

-- | @Selector@ for @initWithOperationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:ipk:@
initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipkSelector :: Selector '[RawId, Id NSData, Id NSData, Id NSData, Id NSData] (Id MTRDeviceControllerStartupParams)
initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipkSelector = mkSelector "initWithOperationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:ipk:"

-- | @Selector@ for @nocSigner@
nocSignerSelector :: Selector '[] RawId
nocSignerSelector = mkSelector "nocSigner"

-- | @Selector@ for @fabricID@
fabricIDSelector :: Selector '[] (Id NSNumber)
fabricIDSelector = mkSelector "fabricID"

-- | @Selector@ for @ipk@
ipkSelector :: Selector '[] (Id NSData)
ipkSelector = mkSelector "ipk"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector '[Id NSNumber] ()
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @caseAuthenticatedTags@
caseAuthenticatedTagsSelector :: Selector '[] (Id NSSet)
caseAuthenticatedTagsSelector = mkSelector "caseAuthenticatedTags"

-- | @Selector@ for @setCaseAuthenticatedTags:@
setCaseAuthenticatedTagsSelector :: Selector '[Id NSSet] ()
setCaseAuthenticatedTagsSelector = mkSelector "setCaseAuthenticatedTags:"

-- | @Selector@ for @rootCertificate@
rootCertificateSelector :: Selector '[] (Id NSData)
rootCertificateSelector = mkSelector "rootCertificate"

-- | @Selector@ for @setRootCertificate:@
setRootCertificateSelector :: Selector '[Id NSData] ()
setRootCertificateSelector = mkSelector "setRootCertificate:"

-- | @Selector@ for @intermediateCertificate@
intermediateCertificateSelector :: Selector '[] (Id NSData)
intermediateCertificateSelector = mkSelector "intermediateCertificate"

-- | @Selector@ for @setIntermediateCertificate:@
setIntermediateCertificateSelector :: Selector '[Id NSData] ()
setIntermediateCertificateSelector = mkSelector "setIntermediateCertificate:"

-- | @Selector@ for @operationalCertificate@
operationalCertificateSelector :: Selector '[] (Id NSData)
operationalCertificateSelector = mkSelector "operationalCertificate"

-- | @Selector@ for @operationalKeypair@
operationalKeypairSelector :: Selector '[] RawId
operationalKeypairSelector = mkSelector "operationalKeypair"

-- | @Selector@ for @setOperationalKeypair:@
setOperationalKeypairSelector :: Selector '[RawId] ()
setOperationalKeypairSelector = mkSelector "setOperationalKeypair:"

-- | @Selector@ for @operationalCertificateIssuer@
operationalCertificateIssuerSelector :: Selector '[] RawId
operationalCertificateIssuerSelector = mkSelector "operationalCertificateIssuer"

-- | @Selector@ for @setOperationalCertificateIssuer:@
setOperationalCertificateIssuerSelector :: Selector '[RawId] ()
setOperationalCertificateIssuerSelector = mkSelector "setOperationalCertificateIssuer:"

-- | @Selector@ for @operationalCertificateIssuerQueue@
operationalCertificateIssuerQueueSelector :: Selector '[] (Id NSObject)
operationalCertificateIssuerQueueSelector = mkSelector "operationalCertificateIssuerQueue"

-- | @Selector@ for @setOperationalCertificateIssuerQueue:@
setOperationalCertificateIssuerQueueSelector :: Selector '[Id NSObject] ()
setOperationalCertificateIssuerQueueSelector = mkSelector "setOperationalCertificateIssuerQueue:"

-- | @Selector@ for @fabricId@
fabricIdSelector :: Selector '[] CULong
fabricIdSelector = mkSelector "fabricId"

-- | @Selector@ for @vendorId@
vendorIdSelector :: Selector '[] (Id NSNumber)
vendorIdSelector = mkSelector "vendorId"

-- | @Selector@ for @setVendorId:@
setVendorIdSelector :: Selector '[Id NSNumber] ()
setVendorIdSelector = mkSelector "setVendorId:"

-- | @Selector@ for @nodeId@
nodeIdSelector :: Selector '[] (Id NSNumber)
nodeIdSelector = mkSelector "nodeId"

-- | @Selector@ for @setNodeId:@
setNodeIdSelector :: Selector '[Id NSNumber] ()
setNodeIdSelector = mkSelector "setNodeId:"

