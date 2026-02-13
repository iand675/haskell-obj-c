{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Audio Output    This cluster provides an interface for controlling the Output on a media device such as a TV.
--
-- Generated bindings for @MTRClusterAudioOutput@.
module ObjC.Matter.MTRClusterAudioOutput
  ( MTRClusterAudioOutput
  , IsMTRClusterAudioOutput(..)
  , selectOutputWithParams_expectedValues_expectedValueInterval_completion
  , renameOutputWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeOutputListWithParams
  , readAttributeCurrentOutputWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , selectOutputWithParams_expectedValues_expectedValueInterval_completionHandler
  , renameOutputWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentOutputWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeOutputListWithParamsSelector
  , renameOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , renameOutputWithParams_expectedValues_expectedValueInterval_completionSelector
  , selectOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , selectOutputWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- selectOutputWithParams:expectedValues:expectedValueInterval:completion:@
selectOutputWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRAudioOutputClusterSelectOutputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAudioOutput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selectOutputWithParams_expectedValues_expectedValueInterval_completion mtrClusterAudioOutput params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterAudioOutput selectOutputWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRAudioOutputClusterSelectOutputParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- renameOutputWithParams:expectedValues:expectedValueInterval:completion:@
renameOutputWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRAudioOutputClusterRenameOutputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAudioOutput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
renameOutputWithParams_expectedValues_expectedValueInterval_completion mtrClusterAudioOutput params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterAudioOutput renameOutputWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRAudioOutputClusterRenameOutputParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeOutputListWithParams:@
readAttributeOutputListWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeOutputListWithParams mtrClusterAudioOutput params =
  sendMessage mtrClusterAudioOutput readAttributeOutputListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentOutputWithParams:@
readAttributeCurrentOutputWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeCurrentOutputWithParams mtrClusterAudioOutput params =
  sendMessage mtrClusterAudioOutput readAttributeCurrentOutputWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAudioOutput params =
  sendMessage mtrClusterAudioOutput readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAudioOutput params =
  sendMessage mtrClusterAudioOutput readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAudioOutput params =
  sendMessage mtrClusterAudioOutput readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAudioOutput params =
  sendMessage mtrClusterAudioOutput readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRReadParams params) => mtrClusterAudioOutput -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAudioOutput params =
  sendMessage mtrClusterAudioOutput readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterAudioOutput mtrClusterAudioOutput => mtrClusterAudioOutput -> IO (Id MTRClusterAudioOutput)
init_ mtrClusterAudioOutput =
  sendOwnedMessage mtrClusterAudioOutput initSelector

-- | @+ new@
new :: IO (Id MTRClusterAudioOutput)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAudioOutput"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRDevice device, IsNSObject queue) => mtrClusterAudioOutput -> device -> CUShort -> queue -> IO (Id MTRClusterAudioOutput)
initWithDevice_endpoint_queue mtrClusterAudioOutput device endpoint queue =
  sendOwnedMessage mtrClusterAudioOutput initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- selectOutputWithParams:expectedValues:expectedValueInterval:completionHandler:@
selectOutputWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRAudioOutputClusterSelectOutputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAudioOutput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selectOutputWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAudioOutput params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterAudioOutput selectOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRAudioOutputClusterSelectOutputParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- renameOutputWithParams:expectedValues:expectedValueInterval:completionHandler:@
renameOutputWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRAudioOutputClusterRenameOutputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAudioOutput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
renameOutputWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAudioOutput params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterAudioOutput renameOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRAudioOutputClusterRenameOutputParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAudioOutput mtrClusterAudioOutput, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAudioOutput -> device -> endpointID -> queue -> IO (Id MTRClusterAudioOutput)
initWithDevice_endpointID_queue mtrClusterAudioOutput device endpointID queue =
  sendOwnedMessage mtrClusterAudioOutput initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectOutputWithParams:expectedValues:expectedValueInterval:completion:@
selectOutputWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRAudioOutputClusterSelectOutputParams, Id NSArray, Id NSNumber, Ptr ()] ()
selectOutputWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "selectOutputWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @renameOutputWithParams:expectedValues:expectedValueInterval:completion:@
renameOutputWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRAudioOutputClusterRenameOutputParams, Id NSArray, Id NSNumber, Ptr ()] ()
renameOutputWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "renameOutputWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeOutputListWithParams:@
readAttributeOutputListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOutputListWithParamsSelector = mkSelector "readAttributeOutputListWithParams:"

-- | @Selector@ for @readAttributeCurrentOutputWithParams:@
readAttributeCurrentOutputWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentOutputWithParamsSelector = mkSelector "readAttributeCurrentOutputWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterAudioOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterAudioOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterAudioOutput)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @selectOutputWithParams:expectedValues:expectedValueInterval:completionHandler:@
selectOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRAudioOutputClusterSelectOutputParams, Id NSArray, Id NSNumber, Ptr ()] ()
selectOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "selectOutputWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @renameOutputWithParams:expectedValues:expectedValueInterval:completionHandler:@
renameOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRAudioOutputClusterRenameOutputParams, Id NSArray, Id NSNumber, Ptr ()] ()
renameOutputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "renameOutputWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterAudioOutput)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

