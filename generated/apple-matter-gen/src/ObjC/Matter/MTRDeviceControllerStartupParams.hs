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
  , initSelector
  , newSelector
  , initWithIPK_fabricID_nocSignerSelector
  , initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector
  , initWithSigningKeypair_fabricId_ipkSelector
  , initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipkSelector
  , nocSignerSelector
  , fabricIDSelector
  , ipkSelector
  , vendorIDSelector
  , setVendorIDSelector
  , nodeIDSelector
  , setNodeIDSelector
  , caseAuthenticatedTagsSelector
  , setCaseAuthenticatedTagsSelector
  , rootCertificateSelector
  , setRootCertificateSelector
  , intermediateCertificateSelector
  , setIntermediateCertificateSelector
  , operationalCertificateSelector
  , operationalKeypairSelector
  , setOperationalKeypairSelector
  , operationalCertificateIssuerSelector
  , setOperationalCertificateIssuerSelector
  , operationalCertificateIssuerQueueSelector
  , setOperationalCertificateIssuerQueueSelector
  , fabricIdSelector
  , vendorIdSelector
  , setVendorIdSelector
  , nodeIdSelector
  , setNodeIdSelector


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

-- | @- init@
init_ :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id MTRDeviceControllerStartupParams)
init_ mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRDeviceControllerStartupParams)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceControllerStartupParams"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithIPK_fabricID_nocSigner mtrDeviceControllerStartupParams  ipk fabricID nocSigner =
  withObjCPtr ipk $ \raw_ipk ->
    withObjCPtr fabricID $ \raw_fabricID ->
        sendMsg mtrDeviceControllerStartupParams (mkSelector "initWithIPK:fabricID:nocSigner:") (retPtr retVoid) [argPtr (castPtr raw_ipk :: Ptr ()), argPtr (castPtr raw_fabricID :: Ptr ()), argPtr (castPtr (unRawId nocSigner) :: Ptr ())] >>= ownedObject . castPtr

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
initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate mtrDeviceControllerStartupParams  ipk operationalKeypair operationalCertificate intermediateCertificate rootCertificate =
  withObjCPtr ipk $ \raw_ipk ->
    withObjCPtr operationalCertificate $ \raw_operationalCertificate ->
      withObjCPtr intermediateCertificate $ \raw_intermediateCertificate ->
        withObjCPtr rootCertificate $ \raw_rootCertificate ->
            sendMsg mtrDeviceControllerStartupParams (mkSelector "initWithIPK:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:") (retPtr retVoid) [argPtr (castPtr raw_ipk :: Ptr ()), argPtr (castPtr (unRawId operationalKeypair) :: Ptr ()), argPtr (castPtr raw_operationalCertificate :: Ptr ()), argPtr (castPtr raw_intermediateCertificate :: Ptr ()), argPtr (castPtr raw_rootCertificate :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSigningKeypair:fabricId:ipk:@
initWithSigningKeypair_fabricId_ipk :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSData ipk) => mtrDeviceControllerStartupParams -> RawId -> CULong -> ipk -> IO (Id MTRDeviceControllerStartupParams)
initWithSigningKeypair_fabricId_ipk mtrDeviceControllerStartupParams  nocSigner fabricId ipk =
  withObjCPtr ipk $ \raw_ipk ->
      sendMsg mtrDeviceControllerStartupParams (mkSelector "initWithSigningKeypair:fabricId:ipk:") (retPtr retVoid) [argPtr (castPtr (unRawId nocSigner) :: Ptr ()), argCULong fabricId, argPtr (castPtr raw_ipk :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithOperationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:ipk:@
initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipk :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSData operationalCertificate, IsNSData intermediateCertificate, IsNSData rootCertificate, IsNSData ipk) => mtrDeviceControllerStartupParams -> RawId -> operationalCertificate -> intermediateCertificate -> rootCertificate -> ipk -> IO (Id MTRDeviceControllerStartupParams)
initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipk mtrDeviceControllerStartupParams  operationalKeypair operationalCertificate intermediateCertificate rootCertificate ipk =
  withObjCPtr operationalCertificate $ \raw_operationalCertificate ->
    withObjCPtr intermediateCertificate $ \raw_intermediateCertificate ->
      withObjCPtr rootCertificate $ \raw_rootCertificate ->
        withObjCPtr ipk $ \raw_ipk ->
            sendMsg mtrDeviceControllerStartupParams (mkSelector "initWithOperationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:ipk:") (retPtr retVoid) [argPtr (castPtr (unRawId operationalKeypair) :: Ptr ()), argPtr (castPtr raw_operationalCertificate :: Ptr ()), argPtr (castPtr raw_intermediateCertificate :: Ptr ()), argPtr (castPtr raw_rootCertificate :: Ptr ()), argPtr (castPtr raw_ipk :: Ptr ())] >>= ownedObject . castPtr

-- | Keypair used to sign operational certificates.  This is the root CA keypair if not using an intermediate CA, the intermediate CA's keypair otherwise.
--
-- Allowed to be nil if this controller will not be issuing internally-generated operational certificates.  In that case, the MTRDeviceControllerStartupParams object must be initialized using initWithIPK:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate: (to provide the operational credentials for t2he controller itself).
--
-- ObjC selector: @- nocSigner@
nocSigner :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO RawId
nocSigner mtrDeviceControllerStartupParams  =
    fmap (RawId . castPtr) $ sendMsg mtrDeviceControllerStartupParams (mkSelector "nocSigner") (retPtr retVoid) []

-- | Fabric id for the controller.  Must be set to a nonzero value.  This is scoped by the root public key, which is determined as follows:
--
-- * If a root certificate is provided, it is the public key of the root   certificate.
--
-- * If a root certificate is not provided, the root public key is the public   key of the nocSigner keypair, since in this case we are not using an   intermediate certificate.
--
-- ObjC selector: @- fabricID@
fabricID :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSNumber)
fabricID mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "fabricID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | IPK to use for the controller's fabric.  Allowed to change from the last time a controller was started on this fabric if a new IPK has been distributed to all the devices the controller wants to interact with.
--
-- ObjC selector: @- ipk@
ipk :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSData)
ipk mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "ipk") (retPtr retVoid) [] >>= retainedObject . castPtr

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
vendorID mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setVendorID mtrDeviceControllerStartupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerStartupParams (mkSelector "setVendorID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
nodeID mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setNodeID mtrDeviceControllerStartupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerStartupParams (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | CASE authenticated tags to use for this controller's operational certificate.
--
-- Only allowed to be not nil if nodeID is not nil.  In particular, if operationalCertificate is not nil, must be nil.  The provided operational certificate will be used as-is.
--
-- If not nil, must contain at most 3 numbers, which are expected to be 32-bit unsigned Case Authenticated Tag values.
--
-- ObjC selector: @- caseAuthenticatedTags@
caseAuthenticatedTags :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSSet)
caseAuthenticatedTags mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "caseAuthenticatedTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | CASE authenticated tags to use for this controller's operational certificate.
--
-- Only allowed to be not nil if nodeID is not nil.  In particular, if operationalCertificate is not nil, must be nil.  The provided operational certificate will be used as-is.
--
-- If not nil, must contain at most 3 numbers, which are expected to be 32-bit unsigned Case Authenticated Tag values.
--
-- ObjC selector: @- setCaseAuthenticatedTags:@
setCaseAuthenticatedTags :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSSet value) => mtrDeviceControllerStartupParams -> value -> IO ()
setCaseAuthenticatedTags mtrDeviceControllerStartupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerStartupParams (mkSelector "setCaseAuthenticatedTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
rootCertificate mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "rootCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setRootCertificate mtrDeviceControllerStartupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerStartupParams (mkSelector "setRootCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
intermediateCertificate mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "intermediateCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setIntermediateCertificate mtrDeviceControllerStartupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerStartupParams (mkSelector "setIntermediateCertificate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Operational certificate, in X.509 DER form, to use.
--
-- If not nil, will be used as the operational certificate.  In this case operationalKeypair must not be nil.
--
-- If nil, an operational certificate will be determined as described in the documentation for nodeID.
--
-- ObjC selector: @- operationalCertificate@
operationalCertificate :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSData)
operationalCertificate mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "operationalCertificate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Operational keypair to use.  If operationalCertificate is not nil, the public key must match operationalCertificate.
--
-- If not nil, and if operationalCertificate is nil, a new operational certificate will be generated for the given operationalKeypair.  The node id for that certificate will be determined as described in the documentation for nodeID.
--
-- ObjC selector: @- operationalKeypair@
operationalKeypair :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO RawId
operationalKeypair mtrDeviceControllerStartupParams  =
    fmap (RawId . castPtr) $ sendMsg mtrDeviceControllerStartupParams (mkSelector "operationalKeypair") (retPtr retVoid) []

-- | Operational keypair to use.  If operationalCertificate is not nil, the public key must match operationalCertificate.
--
-- If not nil, and if operationalCertificate is nil, a new operational certificate will be generated for the given operationalKeypair.  The node id for that certificate will be determined as described in the documentation for nodeID.
--
-- ObjC selector: @- setOperationalKeypair:@
setOperationalKeypair :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> RawId -> IO ()
setOperationalKeypair mtrDeviceControllerStartupParams  value =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "setOperationalKeypair:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The certificate issuer delegate to use for issuing operational certificates when commissioning devices.  Allowed to be nil if this controller either does not issue operational certificates at all or internally generates the certificates to be issued.  In the latter case, nocSigner must not be nil.
--
-- ObjC selector: @- operationalCertificateIssuer@
operationalCertificateIssuer :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO RawId
operationalCertificateIssuer mtrDeviceControllerStartupParams  =
    fmap (RawId . castPtr) $ sendMsg mtrDeviceControllerStartupParams (mkSelector "operationalCertificateIssuer") (retPtr retVoid) []

-- | The certificate issuer delegate to use for issuing operational certificates when commissioning devices.  Allowed to be nil if this controller either does not issue operational certificates at all or internally generates the certificates to be issued.  In the latter case, nocSigner must not be nil.
--
-- ObjC selector: @- setOperationalCertificateIssuer:@
setOperationalCertificateIssuer :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> RawId -> IO ()
setOperationalCertificateIssuer mtrDeviceControllerStartupParams  value =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "setOperationalCertificateIssuer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The dispatch queue on which operationalCertificateIssuer should be called. Allowed to be nil if and only if operationalCertificateIssuer is nil.
--
-- ObjC selector: @- operationalCertificateIssuerQueue@
operationalCertificateIssuerQueue :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSObject)
operationalCertificateIssuerQueue mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "operationalCertificateIssuerQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The dispatch queue on which operationalCertificateIssuer should be called. Allowed to be nil if and only if operationalCertificateIssuer is nil.
--
-- ObjC selector: @- setOperationalCertificateIssuerQueue:@
setOperationalCertificateIssuerQueue :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSObject value) => mtrDeviceControllerStartupParams -> value -> IO ()
setOperationalCertificateIssuerQueue mtrDeviceControllerStartupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerStartupParams (mkSelector "setOperationalCertificateIssuerQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricId@
fabricId :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO CULong
fabricId mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "fabricId") retCULong []

-- | @- vendorId@
vendorId :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSNumber)
vendorId mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "vendorId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVendorId:@
setVendorId :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSNumber value) => mtrDeviceControllerStartupParams -> value -> IO ()
setVendorId mtrDeviceControllerStartupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerStartupParams (mkSelector "setVendorId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nodeId@
nodeId :: IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams => mtrDeviceControllerStartupParams -> IO (Id NSNumber)
nodeId mtrDeviceControllerStartupParams  =
    sendMsg mtrDeviceControllerStartupParams (mkSelector "nodeId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeId:@
setNodeId :: (IsMTRDeviceControllerStartupParams mtrDeviceControllerStartupParams, IsNSNumber value) => mtrDeviceControllerStartupParams -> value -> IO ()
setNodeId mtrDeviceControllerStartupParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceControllerStartupParams (mkSelector "setNodeId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithIPK:fabricID:nocSigner:@
initWithIPK_fabricID_nocSignerSelector :: Selector
initWithIPK_fabricID_nocSignerSelector = mkSelector "initWithIPK:fabricID:nocSigner:"

-- | @Selector@ for @initWithIPK:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:@
initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector :: Selector
initWithIPK_operationalKeypair_operationalCertificate_intermediateCertificate_rootCertificateSelector = mkSelector "initWithIPK:operationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:"

-- | @Selector@ for @initWithSigningKeypair:fabricId:ipk:@
initWithSigningKeypair_fabricId_ipkSelector :: Selector
initWithSigningKeypair_fabricId_ipkSelector = mkSelector "initWithSigningKeypair:fabricId:ipk:"

-- | @Selector@ for @initWithOperationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:ipk:@
initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipkSelector :: Selector
initWithOperationalKeypair_operationalCertificate_intermediateCertificate_rootCertificate_ipkSelector = mkSelector "initWithOperationalKeypair:operationalCertificate:intermediateCertificate:rootCertificate:ipk:"

-- | @Selector@ for @nocSigner@
nocSignerSelector :: Selector
nocSignerSelector = mkSelector "nocSigner"

-- | @Selector@ for @fabricID@
fabricIDSelector :: Selector
fabricIDSelector = mkSelector "fabricID"

-- | @Selector@ for @ipk@
ipkSelector :: Selector
ipkSelector = mkSelector "ipk"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @caseAuthenticatedTags@
caseAuthenticatedTagsSelector :: Selector
caseAuthenticatedTagsSelector = mkSelector "caseAuthenticatedTags"

-- | @Selector@ for @setCaseAuthenticatedTags:@
setCaseAuthenticatedTagsSelector :: Selector
setCaseAuthenticatedTagsSelector = mkSelector "setCaseAuthenticatedTags:"

-- | @Selector@ for @rootCertificate@
rootCertificateSelector :: Selector
rootCertificateSelector = mkSelector "rootCertificate"

-- | @Selector@ for @setRootCertificate:@
setRootCertificateSelector :: Selector
setRootCertificateSelector = mkSelector "setRootCertificate:"

-- | @Selector@ for @intermediateCertificate@
intermediateCertificateSelector :: Selector
intermediateCertificateSelector = mkSelector "intermediateCertificate"

-- | @Selector@ for @setIntermediateCertificate:@
setIntermediateCertificateSelector :: Selector
setIntermediateCertificateSelector = mkSelector "setIntermediateCertificate:"

-- | @Selector@ for @operationalCertificate@
operationalCertificateSelector :: Selector
operationalCertificateSelector = mkSelector "operationalCertificate"

-- | @Selector@ for @operationalKeypair@
operationalKeypairSelector :: Selector
operationalKeypairSelector = mkSelector "operationalKeypair"

-- | @Selector@ for @setOperationalKeypair:@
setOperationalKeypairSelector :: Selector
setOperationalKeypairSelector = mkSelector "setOperationalKeypair:"

-- | @Selector@ for @operationalCertificateIssuer@
operationalCertificateIssuerSelector :: Selector
operationalCertificateIssuerSelector = mkSelector "operationalCertificateIssuer"

-- | @Selector@ for @setOperationalCertificateIssuer:@
setOperationalCertificateIssuerSelector :: Selector
setOperationalCertificateIssuerSelector = mkSelector "setOperationalCertificateIssuer:"

-- | @Selector@ for @operationalCertificateIssuerQueue@
operationalCertificateIssuerQueueSelector :: Selector
operationalCertificateIssuerQueueSelector = mkSelector "operationalCertificateIssuerQueue"

-- | @Selector@ for @setOperationalCertificateIssuerQueue:@
setOperationalCertificateIssuerQueueSelector :: Selector
setOperationalCertificateIssuerQueueSelector = mkSelector "setOperationalCertificateIssuerQueue:"

-- | @Selector@ for @fabricId@
fabricIdSelector :: Selector
fabricIdSelector = mkSelector "fabricId"

-- | @Selector@ for @vendorId@
vendorIdSelector :: Selector
vendorIdSelector = mkSelector "vendorId"

-- | @Selector@ for @setVendorId:@
setVendorIdSelector :: Selector
setVendorIdSelector = mkSelector "setVendorId:"

-- | @Selector@ for @nodeId@
nodeIdSelector :: Selector
nodeIdSelector = mkSelector "nodeId"

-- | @Selector@ for @setNodeId:@
setNodeIdSelector :: Selector
setNodeIdSelector = mkSelector "setNodeId:"

