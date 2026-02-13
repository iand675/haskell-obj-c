{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSRInfo@.
module ObjC.Matter.CSRInfo
  ( CSRInfo
  , IsCSRInfo(..)
  , initWithNonce_elements_elementsSignature_csr
  , nonce
  , setNonce
  , elements
  , setElements
  , elementsSignature
  , setElementsSignature
  , csr
  , setCsr
  , csrSelector
  , elementsSelector
  , elementsSignatureSelector
  , initWithNonce_elements_elementsSignature_csrSelector
  , nonceSelector
  , setCsrSelector
  , setElementsSelector
  , setElementsSignatureSelector
  , setNonceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithNonce:elements:elementsSignature:csr:@
initWithNonce_elements_elementsSignature_csr :: (IsCSRInfo csrInfo, IsNSData nonce, IsNSData elements, IsNSData elementsSignature, IsNSData csr) => csrInfo -> nonce -> elements -> elementsSignature -> csr -> IO (Id CSRInfo)
initWithNonce_elements_elementsSignature_csr csrInfo nonce elements elementsSignature csr =
  sendOwnedMessage csrInfo initWithNonce_elements_elementsSignature_csrSelector (toNSData nonce) (toNSData elements) (toNSData elementsSignature) (toNSData csr)

-- | @- nonce@
nonce :: IsCSRInfo csrInfo => csrInfo -> IO (Id NSData)
nonce csrInfo =
  sendMessage csrInfo nonceSelector

-- | @- setNonce:@
setNonce :: (IsCSRInfo csrInfo, IsNSData value) => csrInfo -> value -> IO ()
setNonce csrInfo value =
  sendMessage csrInfo setNonceSelector (toNSData value)

-- | @- elements@
elements :: IsCSRInfo csrInfo => csrInfo -> IO (Id NSData)
elements csrInfo =
  sendMessage csrInfo elementsSelector

-- | @- setElements:@
setElements :: (IsCSRInfo csrInfo, IsNSData value) => csrInfo -> value -> IO ()
setElements csrInfo value =
  sendMessage csrInfo setElementsSelector (toNSData value)

-- | @- elementsSignature@
elementsSignature :: IsCSRInfo csrInfo => csrInfo -> IO (Id NSData)
elementsSignature csrInfo =
  sendMessage csrInfo elementsSignatureSelector

-- | @- setElementsSignature:@
setElementsSignature :: (IsCSRInfo csrInfo, IsNSData value) => csrInfo -> value -> IO ()
setElementsSignature csrInfo value =
  sendMessage csrInfo setElementsSignatureSelector (toNSData value)

-- | @- csr@
csr :: IsCSRInfo csrInfo => csrInfo -> IO (Id NSData)
csr csrInfo =
  sendMessage csrInfo csrSelector

-- | @- setCsr:@
setCsr :: (IsCSRInfo csrInfo, IsNSData value) => csrInfo -> value -> IO ()
setCsr csrInfo value =
  sendMessage csrInfo setCsrSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNonce:elements:elementsSignature:csr:@
initWithNonce_elements_elementsSignature_csrSelector :: Selector '[Id NSData, Id NSData, Id NSData, Id NSData] (Id CSRInfo)
initWithNonce_elements_elementsSignature_csrSelector = mkSelector "initWithNonce:elements:elementsSignature:csr:"

-- | @Selector@ for @nonce@
nonceSelector :: Selector '[] (Id NSData)
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @setNonce:@
setNonceSelector :: Selector '[Id NSData] ()
setNonceSelector = mkSelector "setNonce:"

-- | @Selector@ for @elements@
elementsSelector :: Selector '[] (Id NSData)
elementsSelector = mkSelector "elements"

-- | @Selector@ for @setElements:@
setElementsSelector :: Selector '[Id NSData] ()
setElementsSelector = mkSelector "setElements:"

-- | @Selector@ for @elementsSignature@
elementsSignatureSelector :: Selector '[] (Id NSData)
elementsSignatureSelector = mkSelector "elementsSignature"

-- | @Selector@ for @setElementsSignature:@
setElementsSignatureSelector :: Selector '[Id NSData] ()
setElementsSignatureSelector = mkSelector "setElementsSignature:"

-- | @Selector@ for @csr@
csrSelector :: Selector '[] (Id NSData)
csrSelector = mkSelector "csr"

-- | @Selector@ for @setCsr:@
setCsrSelector :: Selector '[Id NSData] ()
setCsrSelector = mkSelector "setCsr:"

