{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRFabricInfo@.
module ObjC.Matter.MTRFabricInfo
  ( MTRFabricInfo
  , IsMTRFabricInfo(..)
  , init_
  , new
  , rootPublicKey
  , vendorID
  , fabricID
  , nodeID
  , label
  , rootCertificate
  , rootCertificateTLV
  , intermediateCertificate
  , intermediateCertificateTLV
  , operationalCertificate
  , operationalCertificateTLV
  , fabricIndex
  , fabricIDSelector
  , fabricIndexSelector
  , initSelector
  , intermediateCertificateSelector
  , intermediateCertificateTLVSelector
  , labelSelector
  , newSelector
  , nodeIDSelector
  , operationalCertificateSelector
  , operationalCertificateTLVSelector
  , rootCertificateSelector
  , rootCertificateTLVSelector
  , rootPublicKeySelector
  , vendorIDSelector


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
init_ :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id MTRFabricInfo)
init_ mtrFabricInfo =
  sendOwnedMessage mtrFabricInfo initSelector

-- | @+ new@
new :: IO (Id MTRFabricInfo)
new  =
  do
    cls' <- getRequiredClass "MTRFabricInfo"
    sendOwnedClassMessage cls' newSelector

-- | Root public key for the fabric.
--
-- ObjC selector: @- rootPublicKey@
rootPublicKey :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
rootPublicKey mtrFabricInfo =
  sendMessage mtrFabricInfo rootPublicKeySelector

-- | Vendor identifier for the fabric.
--
-- ObjC selector: @- vendorID@
vendorID :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSNumber)
vendorID mtrFabricInfo =
  sendMessage mtrFabricInfo vendorIDSelector

-- | Fabric identifier (scoped to the root public key) for the fabric.
--
-- ObjC selector: @- fabricID@
fabricID :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSNumber)
fabricID mtrFabricInfo =
  sendMessage mtrFabricInfo fabricIDSelector

-- | Node identifier for the given node on the fabric.
--
-- ObjC selector: @- nodeID@
nodeID :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSNumber)
nodeID mtrFabricInfo =
  sendMessage mtrFabricInfo nodeIDSelector

-- | The string label for the fabric.  May be empty.
--
-- ObjC selector: @- label@
label :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSString)
label mtrFabricInfo =
  sendMessage mtrFabricInfo labelSelector

-- | The root certificate for the fabric.  This might be nil if a root certificate is not available (e.g. if this is information about some remote node that we don't have root certificate information for).
--
-- ObjC selector: @- rootCertificate@
rootCertificate :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
rootCertificate mtrFabricInfo =
  sendMessage mtrFabricInfo rootCertificateSelector

-- | The same root certificate as rootCertificate, in Matter TLV format.
--
-- ObjC selector: @- rootCertificateTLV@
rootCertificateTLV :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
rootCertificateTLV mtrFabricInfo =
  sendMessage mtrFabricInfo rootCertificateTLVSelector

-- | The intermediate certificate for the node.  This might be nil if there is no intermediate certificate, or if the node is not on a fabric we have access to.
--
-- ObjC selector: @- intermediateCertificate@
intermediateCertificate :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
intermediateCertificate mtrFabricInfo =
  sendMessage mtrFabricInfo intermediateCertificateSelector

-- | The same intermediate certificate as intermediateCertificate, in Matter TLV format.
--
-- ObjC selector: @- intermediateCertificateTLV@
intermediateCertificateTLV :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
intermediateCertificateTLV mtrFabricInfo =
  sendMessage mtrFabricInfo intermediateCertificateTLVSelector

-- | The operational certificate for the node.  This might be nil if the node is not on a fabric we have access to.
--
-- ObjC selector: @- operationalCertificate@
operationalCertificate :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
operationalCertificate mtrFabricInfo =
  sendMessage mtrFabricInfo operationalCertificateSelector

-- | The same operational certificate as operationalCertificate, in Matter TLV format.
--
-- ObjC selector: @- operationalCertificateTLV@
operationalCertificateTLV :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSData)
operationalCertificateTLV mtrFabricInfo =
  sendMessage mtrFabricInfo operationalCertificateTLVSelector

-- | The fabric index which identifies the fabric on the node.
--
-- ObjC selector: @- fabricIndex@
fabricIndex :: IsMTRFabricInfo mtrFabricInfo => mtrFabricInfo -> IO (Id NSNumber)
fabricIndex mtrFabricInfo =
  sendMessage mtrFabricInfo fabricIndexSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRFabricInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRFabricInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @rootPublicKey@
rootPublicKeySelector :: Selector '[] (Id NSData)
rootPublicKeySelector = mkSelector "rootPublicKey"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @fabricID@
fabricIDSelector :: Selector '[] (Id NSNumber)
fabricIDSelector = mkSelector "fabricID"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @rootCertificate@
rootCertificateSelector :: Selector '[] (Id NSData)
rootCertificateSelector = mkSelector "rootCertificate"

-- | @Selector@ for @rootCertificateTLV@
rootCertificateTLVSelector :: Selector '[] (Id NSData)
rootCertificateTLVSelector = mkSelector "rootCertificateTLV"

-- | @Selector@ for @intermediateCertificate@
intermediateCertificateSelector :: Selector '[] (Id NSData)
intermediateCertificateSelector = mkSelector "intermediateCertificate"

-- | @Selector@ for @intermediateCertificateTLV@
intermediateCertificateTLVSelector :: Selector '[] (Id NSData)
intermediateCertificateTLVSelector = mkSelector "intermediateCertificateTLV"

-- | @Selector@ for @operationalCertificate@
operationalCertificateSelector :: Selector '[] (Id NSData)
operationalCertificateSelector = mkSelector "operationalCertificate"

-- | @Selector@ for @operationalCertificateTLV@
operationalCertificateTLVSelector :: Selector '[] (Id NSData)
operationalCertificateTLVSelector = mkSelector "operationalCertificateTLV"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

