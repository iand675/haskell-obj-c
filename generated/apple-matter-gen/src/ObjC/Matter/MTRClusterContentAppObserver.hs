{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Content App Observer    This cluster provides an interface for sending targeted commands to an Observer of a Content App on a Video Player device such as a Streaming Media Player, Smart TV or Smart Screen. The cluster server for Content App Observer is implemented by an endpoint that communicates with a Content App, such as a Casting Video Client. The cluster client for Content App Observer is implemented by a Content App endpoint. A Content App is informed of the NodeId of an Observer when a binding is set on the Content App. The Content App can then send the ContentAppMessage to the Observer (server cluster), and the Observer responds with a ContentAppMessageResponse.
--
-- Generated bindings for @MTRClusterContentAppObserver@.
module ObjC.Matter.MTRClusterContentAppObserver
  ( MTRClusterContentAppObserver
  , IsMTRClusterContentAppObserver(..)
  , contentAppMessageWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , contentAppMessageWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
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

-- | @- contentAppMessageWithParams:expectedValues:expectedValueInterval:completion:@
contentAppMessageWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRContentAppObserverClusterContentAppMessageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentAppObserver -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
contentAppMessageWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentAppObserver params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentAppObserver contentAppMessageWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentAppObserverClusterContentAppMessageParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRReadParams params) => mtrClusterContentAppObserver -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterContentAppObserver params =
  sendMessage mtrClusterContentAppObserver readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRReadParams params) => mtrClusterContentAppObserver -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterContentAppObserver params =
  sendMessage mtrClusterContentAppObserver readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRReadParams params) => mtrClusterContentAppObserver -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterContentAppObserver params =
  sendMessage mtrClusterContentAppObserver readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRReadParams params) => mtrClusterContentAppObserver -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterContentAppObserver params =
  sendMessage mtrClusterContentAppObserver readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRReadParams params) => mtrClusterContentAppObserver -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterContentAppObserver params =
  sendMessage mtrClusterContentAppObserver readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterContentAppObserver mtrClusterContentAppObserver => mtrClusterContentAppObserver -> IO (Id MTRClusterContentAppObserver)
init_ mtrClusterContentAppObserver =
  sendOwnedMessage mtrClusterContentAppObserver initSelector

-- | @+ new@
new :: IO (Id MTRClusterContentAppObserver)
new  =
  do
    cls' <- getRequiredClass "MTRClusterContentAppObserver"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterContentAppObserver mtrClusterContentAppObserver, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterContentAppObserver -> device -> endpointID -> queue -> IO (Id MTRClusterContentAppObserver)
initWithDevice_endpointID_queue mtrClusterContentAppObserver device endpointID queue =
  sendOwnedMessage mtrClusterContentAppObserver initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentAppMessageWithParams:expectedValues:expectedValueInterval:completion:@
contentAppMessageWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentAppObserverClusterContentAppMessageParams, Id NSArray, Id NSNumber, Ptr ()] ()
contentAppMessageWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "contentAppMessageWithParams:expectedValues:expectedValueInterval:completion:"

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
initSelector :: Selector '[] (Id MTRClusterContentAppObserver)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterContentAppObserver)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterContentAppObserver)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

