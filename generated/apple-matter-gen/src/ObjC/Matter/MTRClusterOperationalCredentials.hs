{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Operational Credentials    This cluster is used to add or remove Operational Credentials on a Commissionee or Node, as well as manage the associated Fabrics.
--
-- Generated bindings for @MTRClusterOperationalCredentials@.
module ObjC.Matter.MTRClusterOperationalCredentials
  ( MTRClusterOperationalCredentials
  , IsMTRClusterOperationalCredentials(..)
  , attestationRequestWithParams_expectedValues_expectedValueInterval_completion
  , certificateChainRequestWithParams_expectedValues_expectedValueInterval_completion
  , csrRequestWithParams_expectedValues_expectedValueInterval_completion
  , addNOCWithParams_expectedValues_expectedValueInterval_completion
  , updateNOCWithParams_expectedValues_expectedValueInterval_completion
  , updateFabricLabelWithParams_expectedValues_expectedValueInterval_completion
  , removeFabricWithParams_expectedValues_expectedValueInterval_completion
  , addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completion
  , setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completion
  , setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completion
  , signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeNOCsWithParams
  , readAttributeFabricsWithParams
  , readAttributeSupportedFabricsWithParams
  , readAttributeCommissionedFabricsWithParams
  , readAttributeTrustedRootCertificatesWithParams
  , readAttributeCurrentFabricIndexWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , csrRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , addNOCWithParams_expectedValues_expectedValueInterval_completionHandler
  , updateNOCWithParams_expectedValues_expectedValueInterval_completionHandler
  , updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandler
  , removeFabricWithParams_expectedValues_expectedValueInterval_completionHandler
  , addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , addNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , addNOCWithParams_expectedValues_expectedValueInterval_completionSelector
  , addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , attestationRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , csrRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , csrRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCommissionedFabricsWithParamsSelector
  , readAttributeCurrentFabricIndexWithParamsSelector
  , readAttributeFabricsWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeNOCsWithParamsSelector
  , readAttributeSupportedFabricsWithParamsSelector
  , readAttributeTrustedRootCertificatesWithParamsSelector
  , removeFabricWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , removeFabricWithParams_expectedValues_expectedValueInterval_completionSelector
  , setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completionSelector
  , setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completionSelector
  , signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , updateNOCWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- attestationRequestWithParams:expectedValues:expectedValueInterval:completion:@
attestationRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAttestationRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
attestationRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials attestationRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalCredentialsClusterAttestationRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- certificateChainRequestWithParams:expectedValues:expectedValueInterval:completion:@
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterCertificateChainRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalCredentialsClusterCertificateChainRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- CSRRequestWithParams:expectedValues:expectedValueInterval:completion:@
csrRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterCSRRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
csrRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials csrRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalCredentialsClusterCSRRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- addNOCWithParams:expectedValues:expectedValueInterval:completion:@
addNOCWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddNOCParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addNOCWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials addNOCWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalCredentialsClusterAddNOCParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- updateNOCWithParams:expectedValues:expectedValueInterval:completion:@
updateNOCWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateNOCParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateNOCWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials updateNOCWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalCredentialsClusterUpdateNOCParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- updateFabricLabelWithParams:expectedValues:expectedValueInterval:completion:@
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateFabricLabelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalCredentialsClusterUpdateFabricLabelParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeFabricWithParams:expectedValues:expectedValueInterval:completion:@
removeFabricWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterRemoveFabricParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeFabricWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials removeFabricWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalCredentialsClusterRemoveFabricParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalCredentialsClusterAddTrustedRootCertificateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setVIDVerificationStatementWithParams:expectedValues:expectedValueInterval:completion:@
setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalCredentialsClusterSetVIDVerificationStatementParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setVIDVerificationStatementWithExpectedValues:expectedValueInterval:completion:@
setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- signVIDVerificationRequestWithParams:expectedValues:expectedValueInterval:completion:@
signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalCredentials signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalCredentialsClusterSignVIDVerificationRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeNOCsWithParams:@
readAttributeNOCsWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeNOCsWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeNOCsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFabricsWithParams:@
readAttributeFabricsWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeFabricsWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeFabricsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportedFabricsWithParams:@
readAttributeSupportedFabricsWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeSupportedFabricsWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeSupportedFabricsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCommissionedFabricsWithParams:@
readAttributeCommissionedFabricsWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeCommissionedFabricsWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeCommissionedFabricsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTrustedRootCertificatesWithParams:@
readAttributeTrustedRootCertificatesWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeTrustedRootCertificatesWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeTrustedRootCertificatesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentFabricIndexWithParams:@
readAttributeCurrentFabricIndexWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeCurrentFabricIndexWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeCurrentFabricIndexWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOperationalCredentials params =
  sendMessage mtrClusterOperationalCredentials readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials => mtrClusterOperationalCredentials -> IO (Id MTRClusterOperationalCredentials)
init_ mtrClusterOperationalCredentials =
  sendOwnedMessage mtrClusterOperationalCredentials initSelector

-- | @+ new@
new :: IO (Id MTRClusterOperationalCredentials)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOperationalCredentials"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRDevice device, IsNSObject queue) => mtrClusterOperationalCredentials -> device -> CUShort -> queue -> IO (Id MTRClusterOperationalCredentials)
initWithDevice_endpoint_queue mtrClusterOperationalCredentials device endpoint queue =
  sendOwnedMessage mtrClusterOperationalCredentials initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- attestationRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAttestationRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOperationalCredentials attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROperationalCredentialsClusterAttestationRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- certificateChainRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterCertificateChainRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOperationalCredentials certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROperationalCredentialsClusterCertificateChainRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- CSRRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
csrRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterCSRRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
csrRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOperationalCredentials csrRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROperationalCredentialsClusterCSRRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- addNOCWithParams:expectedValues:expectedValueInterval:completionHandler:@
addNOCWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddNOCParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addNOCWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOperationalCredentials addNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROperationalCredentialsClusterAddNOCParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- updateNOCWithParams:expectedValues:expectedValueInterval:completionHandler:@
updateNOCWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateNOCParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateNOCWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOperationalCredentials updateNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROperationalCredentialsClusterUpdateNOCParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- updateFabricLabelWithParams:expectedValues:expectedValueInterval:completionHandler:@
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateFabricLabelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOperationalCredentials updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROperationalCredentialsClusterUpdateFabricLabelParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- removeFabricWithParams:expectedValues:expectedValueInterval:completionHandler:@
removeFabricWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterRemoveFabricParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeFabricWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOperationalCredentials removeFabricWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROperationalCredentialsClusterRemoveFabricParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completionHandler:@
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOperationalCredentials addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROperationalCredentialsClusterAddTrustedRootCertificateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOperationalCredentials -> device -> endpointID -> queue -> IO (Id MTRClusterOperationalCredentials)
initWithDevice_endpointID_queue mtrClusterOperationalCredentials device endpointID queue =
  sendOwnedMessage mtrClusterOperationalCredentials initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attestationRequestWithParams:expectedValues:expectedValueInterval:completion:@
attestationRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalCredentialsClusterAttestationRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
attestationRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "attestationRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @certificateChainRequestWithParams:expectedValues:expectedValueInterval:completion:@
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalCredentialsClusterCertificateChainRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "certificateChainRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @CSRRequestWithParams:expectedValues:expectedValueInterval:completion:@
csrRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalCredentialsClusterCSRRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
csrRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "CSRRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addNOCWithParams:expectedValues:expectedValueInterval:completion:@
addNOCWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalCredentialsClusterAddNOCParams, Id NSArray, Id NSNumber, Ptr ()] ()
addNOCWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addNOCWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateNOCWithParams:expectedValues:expectedValueInterval:completion:@
updateNOCWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalCredentialsClusterUpdateNOCParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateNOCWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateNOCWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateFabricLabelWithParams:expectedValues:expectedValueInterval:completion:@
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalCredentialsClusterUpdateFabricLabelParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateFabricLabelWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeFabricWithParams:expectedValues:expectedValueInterval:completion:@
removeFabricWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalCredentialsClusterRemoveFabricParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeFabricWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeFabricWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalCredentialsClusterAddTrustedRootCertificateParams, Id NSArray, Id NSNumber, Ptr ()] ()
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setVIDVerificationStatementWithParams:expectedValues:expectedValueInterval:completion:@
setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalCredentialsClusterSetVIDVerificationStatementParams, Id NSArray, Id NSNumber, Ptr ()] ()
setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setVIDVerificationStatementWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setVIDVerificationStatementWithExpectedValues:expectedValueInterval:completion:@
setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "setVIDVerificationStatementWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @signVIDVerificationRequestWithParams:expectedValues:expectedValueInterval:completion:@
signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalCredentialsClusterSignVIDVerificationRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "signVIDVerificationRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeNOCsWithParams:@
readAttributeNOCsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNOCsWithParamsSelector = mkSelector "readAttributeNOCsWithParams:"

-- | @Selector@ for @readAttributeFabricsWithParams:@
readAttributeFabricsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFabricsWithParamsSelector = mkSelector "readAttributeFabricsWithParams:"

-- | @Selector@ for @readAttributeSupportedFabricsWithParams:@
readAttributeSupportedFabricsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedFabricsWithParamsSelector = mkSelector "readAttributeSupportedFabricsWithParams:"

-- | @Selector@ for @readAttributeCommissionedFabricsWithParams:@
readAttributeCommissionedFabricsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCommissionedFabricsWithParamsSelector = mkSelector "readAttributeCommissionedFabricsWithParams:"

-- | @Selector@ for @readAttributeTrustedRootCertificatesWithParams:@
readAttributeTrustedRootCertificatesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTrustedRootCertificatesWithParamsSelector = mkSelector "readAttributeTrustedRootCertificatesWithParams:"

-- | @Selector@ for @readAttributeCurrentFabricIndexWithParams:@
readAttributeCurrentFabricIndexWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentFabricIndexWithParamsSelector = mkSelector "readAttributeCurrentFabricIndexWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRClusterOperationalCredentials)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterOperationalCredentials)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterOperationalCredentials)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @attestationRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterAttestationRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "attestationRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @certificateChainRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterCertificateChainRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "certificateChainRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @CSRRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
csrRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterCSRRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
csrRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "CSRRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @addNOCWithParams:expectedValues:expectedValueInterval:completionHandler:@
addNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterAddNOCParams, Id NSArray, Id NSNumber, Ptr ()] ()
addNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "addNOCWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @updateNOCWithParams:expectedValues:expectedValueInterval:completionHandler:@
updateNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterUpdateNOCParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "updateNOCWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @updateFabricLabelWithParams:expectedValues:expectedValueInterval:completionHandler:@
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterUpdateFabricLabelParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "updateFabricLabelWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @removeFabricWithParams:expectedValues:expectedValueInterval:completionHandler:@
removeFabricWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterRemoveFabricParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeFabricWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "removeFabricWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completionHandler:@
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterAddTrustedRootCertificateParams, Id NSArray, Id NSNumber, Ptr ()] ()
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterOperationalCredentials)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

