{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster WebRTC Transport Provider    The WebRTC transport provider cluster provides a way for stream providers (e.g. Cameras) to stream or receive their data through WebRTC.
--
-- Generated bindings for @MTRClusterWebRTCTransportProvider@.
module ObjC.Matter.MTRClusterWebRTCTransportProvider
  ( MTRClusterWebRTCTransportProvider
  , IsMTRClusterWebRTCTransportProvider(..)
  , solicitOfferWithParams_expectedValues_expectedValueInterval_completion
  , provideOfferWithParams_expectedValues_expectedValueInterval_completion
  , provideAnswerWithParams_expectedValues_expectedValueInterval_completion
  , provideICECandidatesWithParams_expectedValues_expectedValueInterval_completion
  , endSessionWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentSessionsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , endSessionWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , provideAnswerWithParams_expectedValues_expectedValueInterval_completionSelector
  , provideICECandidatesWithParams_expectedValues_expectedValueInterval_completionSelector
  , provideOfferWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentSessionsWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , solicitOfferWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- solicitOfferWithParams:expectedValues:expectedValueInterval:completion:@
solicitOfferWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterSolicitOfferParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
solicitOfferWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportProvider params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWebRTCTransportProvider solicitOfferWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWebRTCTransportProviderClusterSolicitOfferParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- provideOfferWithParams:expectedValues:expectedValueInterval:completion:@
provideOfferWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterProvideOfferParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provideOfferWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportProvider params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWebRTCTransportProvider provideOfferWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWebRTCTransportProviderClusterProvideOfferParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- provideAnswerWithParams:expectedValues:expectedValueInterval:completion:@
provideAnswerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterProvideAnswerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provideAnswerWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportProvider params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWebRTCTransportProvider provideAnswerWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWebRTCTransportProviderClusterProvideAnswerParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- provideICECandidatesWithParams:expectedValues:expectedValueInterval:completion:@
provideICECandidatesWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provideICECandidatesWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportProvider params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWebRTCTransportProvider provideICECandidatesWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWebRTCTransportProviderClusterProvideICECandidatesParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- endSessionWithParams:expectedValues:expectedValueInterval:completion:@
endSessionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRWebRTCTransportProviderClusterEndSessionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
endSessionWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportProvider params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWebRTCTransportProvider endSessionWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWebRTCTransportProviderClusterEndSessionParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeCurrentSessionsWithParams:@
readAttributeCurrentSessionsWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeCurrentSessionsWithParams mtrClusterWebRTCTransportProvider params =
  sendMessage mtrClusterWebRTCTransportProvider readAttributeCurrentSessionsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWebRTCTransportProvider params =
  sendMessage mtrClusterWebRTCTransportProvider readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWebRTCTransportProvider params =
  sendMessage mtrClusterWebRTCTransportProvider readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWebRTCTransportProvider params =
  sendMessage mtrClusterWebRTCTransportProvider readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWebRTCTransportProvider params =
  sendMessage mtrClusterWebRTCTransportProvider readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRReadParams params) => mtrClusterWebRTCTransportProvider -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWebRTCTransportProvider params =
  sendMessage mtrClusterWebRTCTransportProvider readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider => mtrClusterWebRTCTransportProvider -> IO (Id MTRClusterWebRTCTransportProvider)
init_ mtrClusterWebRTCTransportProvider =
  sendOwnedMessage mtrClusterWebRTCTransportProvider initSelector

-- | @+ new@
new :: IO (Id MTRClusterWebRTCTransportProvider)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWebRTCTransportProvider"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWebRTCTransportProvider mtrClusterWebRTCTransportProvider, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWebRTCTransportProvider -> device -> endpointID -> queue -> IO (Id MTRClusterWebRTCTransportProvider)
initWithDevice_endpointID_queue mtrClusterWebRTCTransportProvider device endpointID queue =
  sendOwnedMessage mtrClusterWebRTCTransportProvider initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @solicitOfferWithParams:expectedValues:expectedValueInterval:completion:@
solicitOfferWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWebRTCTransportProviderClusterSolicitOfferParams, Id NSArray, Id NSNumber, Ptr ()] ()
solicitOfferWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "solicitOfferWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @provideOfferWithParams:expectedValues:expectedValueInterval:completion:@
provideOfferWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWebRTCTransportProviderClusterProvideOfferParams, Id NSArray, Id NSNumber, Ptr ()] ()
provideOfferWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provideOfferWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @provideAnswerWithParams:expectedValues:expectedValueInterval:completion:@
provideAnswerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWebRTCTransportProviderClusterProvideAnswerParams, Id NSArray, Id NSNumber, Ptr ()] ()
provideAnswerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provideAnswerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @provideICECandidatesWithParams:expectedValues:expectedValueInterval:completion:@
provideICECandidatesWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWebRTCTransportProviderClusterProvideICECandidatesParams, Id NSArray, Id NSNumber, Ptr ()] ()
provideICECandidatesWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provideICECandidatesWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @endSessionWithParams:expectedValues:expectedValueInterval:completion:@
endSessionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWebRTCTransportProviderClusterEndSessionParams, Id NSArray, Id NSNumber, Ptr ()] ()
endSessionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "endSessionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentSessionsWithParams:@
readAttributeCurrentSessionsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentSessionsWithParamsSelector = mkSelector "readAttributeCurrentSessionsWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterWebRTCTransportProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterWebRTCTransportProvider)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterWebRTCTransportProvider)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

