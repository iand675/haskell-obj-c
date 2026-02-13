{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster WebRTC Transport Requestor    The WebRTC transport requestor cluster provides a way for stream consumers (e.g. Matter Stream Viewer) to establish a WebRTC connection with a stream provider.
--
-- Generated bindings for @MTRClusterWebRTCTransportRequestor@.
module ObjC.Matter.MTRClusterWebRTCTransportRequestor
  ( MTRClusterWebRTCTransportRequestor
  , IsMTRClusterWebRTCTransportRequestor(..)
  , offerWithParams_expectedValues_expectedValueInterval_completion
  , answerWithParams_expectedValues_expectedValueInterval_completion
  , iceCandidatesWithParams_expectedValues_expectedValueInterval_completion
  , endWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentSessionsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , answerWithParams_expectedValues_expectedValueInterval_completionSelector
  , endWithParams_expectedValues_expectedValueInterval_completionSelector
  , iceCandidatesWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , offerWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentSessionsWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- offerWithParams:expectedValues:expectedValueInterval:completion:@
offerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterOfferParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportRequestor -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
offerWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportRequestor params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWebRTCTransportRequestor offerWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWebRTCTransportRequestorClusterOfferParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- answerWithParams:expectedValues:expectedValueInterval:completion:@
answerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterAnswerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportRequestor -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
answerWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportRequestor params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWebRTCTransportRequestor answerWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWebRTCTransportRequestorClusterAnswerParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- ICECandidatesWithParams:expectedValues:expectedValueInterval:completion:@
iceCandidatesWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterICECandidatesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportRequestor -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
iceCandidatesWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportRequestor params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWebRTCTransportRequestor iceCandidatesWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWebRTCTransportRequestorClusterICECandidatesParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- endWithParams:expectedValues:expectedValueInterval:completion:@
endWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterEndParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWebRTCTransportRequestor -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
endWithParams_expectedValues_expectedValueInterval_completion mtrClusterWebRTCTransportRequestor params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWebRTCTransportRequestor endWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWebRTCTransportRequestorClusterEndParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeCurrentSessionsWithParams:@
readAttributeCurrentSessionsWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeCurrentSessionsWithParams mtrClusterWebRTCTransportRequestor params =
  sendMessage mtrClusterWebRTCTransportRequestor readAttributeCurrentSessionsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWebRTCTransportRequestor params =
  sendMessage mtrClusterWebRTCTransportRequestor readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWebRTCTransportRequestor params =
  sendMessage mtrClusterWebRTCTransportRequestor readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWebRTCTransportRequestor params =
  sendMessage mtrClusterWebRTCTransportRequestor readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWebRTCTransportRequestor params =
  sendMessage mtrClusterWebRTCTransportRequestor readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrClusterWebRTCTransportRequestor -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWebRTCTransportRequestor params =
  sendMessage mtrClusterWebRTCTransportRequestor readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor => mtrClusterWebRTCTransportRequestor -> IO (Id MTRClusterWebRTCTransportRequestor)
init_ mtrClusterWebRTCTransportRequestor =
  sendOwnedMessage mtrClusterWebRTCTransportRequestor initSelector

-- | @+ new@
new :: IO (Id MTRClusterWebRTCTransportRequestor)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWebRTCTransportRequestor"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWebRTCTransportRequestor mtrClusterWebRTCTransportRequestor, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWebRTCTransportRequestor -> device -> endpointID -> queue -> IO (Id MTRClusterWebRTCTransportRequestor)
initWithDevice_endpointID_queue mtrClusterWebRTCTransportRequestor device endpointID queue =
  sendOwnedMessage mtrClusterWebRTCTransportRequestor initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offerWithParams:expectedValues:expectedValueInterval:completion:@
offerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWebRTCTransportRequestorClusterOfferParams, Id NSArray, Id NSNumber, Ptr ()] ()
offerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "offerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @answerWithParams:expectedValues:expectedValueInterval:completion:@
answerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWebRTCTransportRequestorClusterAnswerParams, Id NSArray, Id NSNumber, Ptr ()] ()
answerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "answerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @ICECandidatesWithParams:expectedValues:expectedValueInterval:completion:@
iceCandidatesWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWebRTCTransportRequestorClusterICECandidatesParams, Id NSArray, Id NSNumber, Ptr ()] ()
iceCandidatesWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "ICECandidatesWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @endWithParams:expectedValues:expectedValueInterval:completion:@
endWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWebRTCTransportRequestorClusterEndParams, Id NSArray, Id NSNumber, Ptr ()] ()
endWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "endWithParams:expectedValues:expectedValueInterval:completion:"

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
initSelector :: Selector '[] (Id MTRClusterWebRTCTransportRequestor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterWebRTCTransportRequestor)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterWebRTCTransportRequestor)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

