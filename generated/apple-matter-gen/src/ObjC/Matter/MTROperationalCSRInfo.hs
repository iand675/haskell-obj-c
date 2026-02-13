{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents information relating to a certificate signing request for a Matter operational certificate.
--
-- Generated bindings for @MTROperationalCSRInfo@.
module ObjC.Matter.MTROperationalCSRInfo
  ( MTROperationalCSRInfo
  , IsMTROperationalCSRInfo(..)
  , initWithCSR_csrNonce_csrElementsTLV_attestationSignature
  , initWithCSRNonce_csrElementsTLV_attestationSignature
  , initWithCSRElementsTLV_attestationSignature
  , initWithCSRResponseParams
  , csr
  , csrNonce
  , csrElementsTLV
  , attestationSignature
  , attestationSignatureSelector
  , csrElementsTLVSelector
  , csrNonceSelector
  , csrSelector
  , initWithCSRElementsTLV_attestationSignatureSelector
  , initWithCSRNonce_csrElementsTLV_attestationSignatureSelector
  , initWithCSRResponseParamsSelector
  , initWithCSR_csrNonce_csrElementsTLV_attestationSignatureSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTROperationalCSRInfo by providing all the fields.  It's the caller's responsibility to ensure that csr and csrNonce match the csrElementsTLV.
--
-- ObjC selector: @- initWithCSR:csrNonce:csrElementsTLV:attestationSignature:@
initWithCSR_csrNonce_csrElementsTLV_attestationSignature :: (IsMTROperationalCSRInfo mtrOperationalCSRInfo, IsNSData csr, IsNSData csrNonce, IsNSData csrElementsTLV, IsNSData attestationSignature) => mtrOperationalCSRInfo -> csr -> csrNonce -> csrElementsTLV -> attestationSignature -> IO (Id MTROperationalCSRInfo)
initWithCSR_csrNonce_csrElementsTLV_attestationSignature mtrOperationalCSRInfo csr csrNonce csrElementsTLV attestationSignature =
  sendOwnedMessage mtrOperationalCSRInfo initWithCSR_csrNonce_csrElementsTLV_attestationSignatureSelector (toNSData csr) (toNSData csrNonce) (toNSData csrElementsTLV) (toNSData attestationSignature)

-- | Initialize an MTROperationalCSRInfo by providing the csrNonce (for example, the nonce the client initially supplied), and the csrElementsTLV and attestationSignature that the server returned.  This will ensure that csrNonce matches the data in csrElementsTLV, returning nil if it does not, and extract the csr from csrElementsTLV.
--
-- ObjC selector: @- initWithCSRNonce:csrElementsTLV:attestationSignature:@
initWithCSRNonce_csrElementsTLV_attestationSignature :: (IsMTROperationalCSRInfo mtrOperationalCSRInfo, IsNSData csrNonce, IsNSData csrElementsTLV, IsNSData attestationSignature) => mtrOperationalCSRInfo -> csrNonce -> csrElementsTLV -> attestationSignature -> IO (Id MTROperationalCSRInfo)
initWithCSRNonce_csrElementsTLV_attestationSignature mtrOperationalCSRInfo csrNonce csrElementsTLV attestationSignature =
  sendOwnedMessage mtrOperationalCSRInfo initWithCSRNonce_csrElementsTLV_attestationSignatureSelector (toNSData csrNonce) (toNSData csrElementsTLV) (toNSData attestationSignature)

-- | Initialize an MTROperationalCSRInfo by providing just the csrElementsTLV and attestationSignature (which can come from an MTROperationalCredentialsClusterCSRResponseParams).  This will extract the csr and csrNonce from the csrElementsTLV, if possible, and return nil if that fails.
--
-- ObjC selector: @- initWithCSRElementsTLV:attestationSignature:@
initWithCSRElementsTLV_attestationSignature :: (IsMTROperationalCSRInfo mtrOperationalCSRInfo, IsNSData csrElementsTLV, IsNSData attestationSignature) => mtrOperationalCSRInfo -> csrElementsTLV -> attestationSignature -> IO (Id MTROperationalCSRInfo)
initWithCSRElementsTLV_attestationSignature mtrOperationalCSRInfo csrElementsTLV attestationSignature =
  sendOwnedMessage mtrOperationalCSRInfo initWithCSRElementsTLV_attestationSignatureSelector (toNSData csrElementsTLV) (toNSData attestationSignature)

-- | Initialize an MTROperationalCSRInfo by providing an MTROperationalCredentialsClusterCSRResponseParams.  This will extract the relevant fields from the response data.
--
-- ObjC selector: @- initWithCSRResponseParams:@
initWithCSRResponseParams :: (IsMTROperationalCSRInfo mtrOperationalCSRInfo, IsMTROperationalCredentialsClusterCSRResponseParams responseParams) => mtrOperationalCSRInfo -> responseParams -> IO (Id MTROperationalCSRInfo)
initWithCSRResponseParams mtrOperationalCSRInfo responseParams =
  sendOwnedMessage mtrOperationalCSRInfo initWithCSRResponseParamsSelector (toMTROperationalCredentialsClusterCSRResponseParams responseParams)

-- | DER-encoded certificate signing request.
--
-- ObjC selector: @- csr@
csr :: IsMTROperationalCSRInfo mtrOperationalCSRInfo => mtrOperationalCSRInfo -> IO (Id NSData)
csr mtrOperationalCSRInfo =
  sendMessage mtrOperationalCSRInfo csrSelector

-- | The nonce associated with this CSR.
--
-- ObjC selector: @- csrNonce@
csrNonce :: IsMTROperationalCSRInfo mtrOperationalCSRInfo => mtrOperationalCSRInfo -> IO (Id NSData)
csrNonce mtrOperationalCSRInfo =
  sendMessage mtrOperationalCSRInfo csrNonceSelector

-- | TLV-encoded nocsr-elements structure.  This includes the "csr" and "csrNonce" fields, and can include additional vendor-specific information.
--
-- ObjC selector: @- csrElementsTLV@
csrElementsTLV :: IsMTROperationalCSRInfo mtrOperationalCSRInfo => mtrOperationalCSRInfo -> IO (Id NSData)
csrElementsTLV mtrOperationalCSRInfo =
  sendMessage mtrOperationalCSRInfo csrElementsTLVSelector

-- | A signature, using the device attestation private key of the device that created the CSR, over the concatenation of csrElementsTLV and the attestation challenge from the secure session.
--
-- The attestation challenge is available in MTRAttestionInfo.
--
-- ObjC selector: @- attestationSignature@
attestationSignature :: IsMTROperationalCSRInfo mtrOperationalCSRInfo => mtrOperationalCSRInfo -> IO (Id NSData)
attestationSignature mtrOperationalCSRInfo =
  sendMessage mtrOperationalCSRInfo attestationSignatureSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCSR:csrNonce:csrElementsTLV:attestationSignature:@
initWithCSR_csrNonce_csrElementsTLV_attestationSignatureSelector :: Selector '[Id NSData, Id NSData, Id NSData, Id NSData] (Id MTROperationalCSRInfo)
initWithCSR_csrNonce_csrElementsTLV_attestationSignatureSelector = mkSelector "initWithCSR:csrNonce:csrElementsTLV:attestationSignature:"

-- | @Selector@ for @initWithCSRNonce:csrElementsTLV:attestationSignature:@
initWithCSRNonce_csrElementsTLV_attestationSignatureSelector :: Selector '[Id NSData, Id NSData, Id NSData] (Id MTROperationalCSRInfo)
initWithCSRNonce_csrElementsTLV_attestationSignatureSelector = mkSelector "initWithCSRNonce:csrElementsTLV:attestationSignature:"

-- | @Selector@ for @initWithCSRElementsTLV:attestationSignature:@
initWithCSRElementsTLV_attestationSignatureSelector :: Selector '[Id NSData, Id NSData] (Id MTROperationalCSRInfo)
initWithCSRElementsTLV_attestationSignatureSelector = mkSelector "initWithCSRElementsTLV:attestationSignature:"

-- | @Selector@ for @initWithCSRResponseParams:@
initWithCSRResponseParamsSelector :: Selector '[Id MTROperationalCredentialsClusterCSRResponseParams] (Id MTROperationalCSRInfo)
initWithCSRResponseParamsSelector = mkSelector "initWithCSRResponseParams:"

-- | @Selector@ for @csr@
csrSelector :: Selector '[] (Id NSData)
csrSelector = mkSelector "csr"

-- | @Selector@ for @csrNonce@
csrNonceSelector :: Selector '[] (Id NSData)
csrNonceSelector = mkSelector "csrNonce"

-- | @Selector@ for @csrElementsTLV@
csrElementsTLVSelector :: Selector '[] (Id NSData)
csrElementsTLVSelector = mkSelector "csrElementsTLV"

-- | @Selector@ for @attestationSignature@
attestationSignatureSelector :: Selector '[] (Id NSData)
attestationSignatureSelector = mkSelector "attestationSignature"

