{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AttestationInfo@.
module ObjC.Matter.AttestationInfo
  ( AttestationInfo
  , IsAttestationInfo(..)
  , initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfo
  , challenge
  , setChallenge
  , nonce
  , setNonce
  , elements
  , setElements
  , elementsSignature
  , setElementsSignature
  , dac
  , setDac
  , pai
  , setPai
  , certificationDeclaration
  , setCertificationDeclaration
  , firmwareInfo
  , setFirmwareInfo
  , certificationDeclarationSelector
  , challengeSelector
  , dacSelector
  , elementsSelector
  , elementsSignatureSelector
  , firmwareInfoSelector
  , initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfoSelector
  , nonceSelector
  , paiSelector
  , setCertificationDeclarationSelector
  , setChallengeSelector
  , setDacSelector
  , setElementsSelector
  , setElementsSignatureSelector
  , setFirmwareInfoSelector
  , setNonceSelector
  , setPaiSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithChallenge:nonce:elements:elementsSignature:dac:pai:certificationDeclaration:firmwareInfo:@
initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfo :: (IsAttestationInfo attestationInfo, IsNSData challenge, IsNSData nonce, IsNSData elements, IsNSData elementsSignature, IsNSData dac, IsNSData pai, IsNSData certificationDeclaration, IsNSData firmwareInfo) => attestationInfo -> challenge -> nonce -> elements -> elementsSignature -> dac -> pai -> certificationDeclaration -> firmwareInfo -> IO (Id AttestationInfo)
initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfo attestationInfo challenge nonce elements elementsSignature dac pai certificationDeclaration firmwareInfo =
  sendOwnedMessage attestationInfo initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfoSelector (toNSData challenge) (toNSData nonce) (toNSData elements) (toNSData elementsSignature) (toNSData dac) (toNSData pai) (toNSData certificationDeclaration) (toNSData firmwareInfo)

-- | @- challenge@
challenge :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
challenge attestationInfo =
  sendMessage attestationInfo challengeSelector

-- | @- setChallenge:@
setChallenge :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setChallenge attestationInfo value =
  sendMessage attestationInfo setChallengeSelector (toNSData value)

-- | @- nonce@
nonce :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
nonce attestationInfo =
  sendMessage attestationInfo nonceSelector

-- | @- setNonce:@
setNonce :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setNonce attestationInfo value =
  sendMessage attestationInfo setNonceSelector (toNSData value)

-- | @- elements@
elements :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
elements attestationInfo =
  sendMessage attestationInfo elementsSelector

-- | @- setElements:@
setElements :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setElements attestationInfo value =
  sendMessage attestationInfo setElementsSelector (toNSData value)

-- | @- elementsSignature@
elementsSignature :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
elementsSignature attestationInfo =
  sendMessage attestationInfo elementsSignatureSelector

-- | @- setElementsSignature:@
setElementsSignature :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setElementsSignature attestationInfo value =
  sendMessage attestationInfo setElementsSignatureSelector (toNSData value)

-- | @- dac@
dac :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
dac attestationInfo =
  sendMessage attestationInfo dacSelector

-- | @- setDac:@
setDac :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setDac attestationInfo value =
  sendMessage attestationInfo setDacSelector (toNSData value)

-- | @- pai@
pai :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
pai attestationInfo =
  sendMessage attestationInfo paiSelector

-- | @- setPai:@
setPai :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setPai attestationInfo value =
  sendMessage attestationInfo setPaiSelector (toNSData value)

-- | @- certificationDeclaration@
certificationDeclaration :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
certificationDeclaration attestationInfo =
  sendMessage attestationInfo certificationDeclarationSelector

-- | @- setCertificationDeclaration:@
setCertificationDeclaration :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setCertificationDeclaration attestationInfo value =
  sendMessage attestationInfo setCertificationDeclarationSelector (toNSData value)

-- | @- firmwareInfo@
firmwareInfo :: IsAttestationInfo attestationInfo => attestationInfo -> IO (Id NSData)
firmwareInfo attestationInfo =
  sendMessage attestationInfo firmwareInfoSelector

-- | @- setFirmwareInfo:@
setFirmwareInfo :: (IsAttestationInfo attestationInfo, IsNSData value) => attestationInfo -> value -> IO ()
setFirmwareInfo attestationInfo value =
  sendMessage attestationInfo setFirmwareInfoSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChallenge:nonce:elements:elementsSignature:dac:pai:certificationDeclaration:firmwareInfo:@
initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfoSelector :: Selector '[Id NSData, Id NSData, Id NSData, Id NSData, Id NSData, Id NSData, Id NSData, Id NSData] (Id AttestationInfo)
initWithChallenge_nonce_elements_elementsSignature_dac_pai_certificationDeclaration_firmwareInfoSelector = mkSelector "initWithChallenge:nonce:elements:elementsSignature:dac:pai:certificationDeclaration:firmwareInfo:"

-- | @Selector@ for @challenge@
challengeSelector :: Selector '[] (Id NSData)
challengeSelector = mkSelector "challenge"

-- | @Selector@ for @setChallenge:@
setChallengeSelector :: Selector '[Id NSData] ()
setChallengeSelector = mkSelector "setChallenge:"

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

-- | @Selector@ for @dac@
dacSelector :: Selector '[] (Id NSData)
dacSelector = mkSelector "dac"

-- | @Selector@ for @setDac:@
setDacSelector :: Selector '[Id NSData] ()
setDacSelector = mkSelector "setDac:"

-- | @Selector@ for @pai@
paiSelector :: Selector '[] (Id NSData)
paiSelector = mkSelector "pai"

-- | @Selector@ for @setPai:@
setPaiSelector :: Selector '[Id NSData] ()
setPaiSelector = mkSelector "setPai:"

-- | @Selector@ for @certificationDeclaration@
certificationDeclarationSelector :: Selector '[] (Id NSData)
certificationDeclarationSelector = mkSelector "certificationDeclaration"

-- | @Selector@ for @setCertificationDeclaration:@
setCertificationDeclarationSelector :: Selector '[Id NSData] ()
setCertificationDeclarationSelector = mkSelector "setCertificationDeclaration:"

-- | @Selector@ for @firmwareInfo@
firmwareInfoSelector :: Selector '[] (Id NSData)
firmwareInfoSelector = mkSelector "firmwareInfo"

-- | @Selector@ for @setFirmwareInfo:@
setFirmwareInfoSelector :: Selector '[Id NSData] ()
setFirmwareInfoSelector = mkSelector "setFirmwareInfo:"

