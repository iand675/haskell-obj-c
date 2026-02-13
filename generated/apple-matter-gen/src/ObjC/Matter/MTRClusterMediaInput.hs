{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Media Input    This cluster provides an interface for controlling the Input Selector on a media device such as a TV.
--
-- Generated bindings for @MTRClusterMediaInput@.
module ObjC.Matter.MTRClusterMediaInput
  ( MTRClusterMediaInput
  , IsMTRClusterMediaInput(..)
  , selectInputWithParams_expectedValues_expectedValueInterval_completion
  , showInputStatusWithParams_expectedValues_expectedValueInterval_completion
  , showInputStatusWithExpectedValues_expectedValueInterval_completion
  , hideInputStatusWithParams_expectedValues_expectedValueInterval_completion
  , hideInputStatusWithExpectedValues_expectedValueInterval_completion
  , renameInputWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeInputListWithParams
  , readAttributeCurrentInputWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , selectInputWithParams_expectedValues_expectedValueInterval_completionHandler
  , showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler
  , showInputStatusWithExpectedValues_expectedValueInterval_completionHandler
  , hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler
  , hideInputStatusWithExpectedValues_expectedValueInterval_completionHandler
  , renameInputWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , hideInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , hideInputStatusWithExpectedValues_expectedValueInterval_completionSelector
  , hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , hideInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentInputWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeInputListWithParamsSelector
  , renameInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , renameInputWithParams_expectedValues_expectedValueInterval_completionSelector
  , selectInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , selectInputWithParams_expectedValues_expectedValueInterval_completionSelector
  , showInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , showInputStatusWithExpectedValues_expectedValueInterval_completionSelector
  , showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , showInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- selectInputWithParams:expectedValues:expectedValueInterval:completion:@
selectInputWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterSelectInputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selectInputWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaInput params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaInput selectInputWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaInputClusterSelectInputParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- showInputStatusWithParams:expectedValues:expectedValueInterval:completion:@
showInputStatusWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterShowInputStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
showInputStatusWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaInput params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaInput showInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaInputClusterShowInputStatusParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- showInputStatusWithExpectedValues:expectedValueInterval:completion:@
showInputStatusWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
showInputStatusWithExpectedValues_expectedValueInterval_completion mtrClusterMediaInput expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaInput showInputStatusWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- hideInputStatusWithParams:expectedValues:expectedValueInterval:completion:@
hideInputStatusWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterHideInputStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
hideInputStatusWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaInput params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaInput hideInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaInputClusterHideInputStatusParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- hideInputStatusWithExpectedValues:expectedValueInterval:completion:@
hideInputStatusWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
hideInputStatusWithExpectedValues_expectedValueInterval_completion mtrClusterMediaInput expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaInput hideInputStatusWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- renameInputWithParams:expectedValues:expectedValueInterval:completion:@
renameInputWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterRenameInputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
renameInputWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaInput params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMediaInput renameInputWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMediaInputClusterRenameInputParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeInputListWithParams:@
readAttributeInputListWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeInputListWithParams mtrClusterMediaInput params =
  sendMessage mtrClusterMediaInput readAttributeInputListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentInputWithParams:@
readAttributeCurrentInputWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeCurrentInputWithParams mtrClusterMediaInput params =
  sendMessage mtrClusterMediaInput readAttributeCurrentInputWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMediaInput params =
  sendMessage mtrClusterMediaInput readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMediaInput params =
  sendMessage mtrClusterMediaInput readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMediaInput params =
  sendMessage mtrClusterMediaInput readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMediaInput params =
  sendMessage mtrClusterMediaInput readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRReadParams params) => mtrClusterMediaInput -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMediaInput params =
  sendMessage mtrClusterMediaInput readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterMediaInput mtrClusterMediaInput => mtrClusterMediaInput -> IO (Id MTRClusterMediaInput)
init_ mtrClusterMediaInput =
  sendOwnedMessage mtrClusterMediaInput initSelector

-- | @+ new@
new :: IO (Id MTRClusterMediaInput)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMediaInput"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRDevice device, IsNSObject queue) => mtrClusterMediaInput -> device -> CUShort -> queue -> IO (Id MTRClusterMediaInput)
initWithDevice_endpoint_queue mtrClusterMediaInput device endpoint queue =
  sendOwnedMessage mtrClusterMediaInput initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- selectInputWithParams:expectedValues:expectedValueInterval:completionHandler:@
selectInputWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterSelectInputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selectInputWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaInput selectInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaInputClusterSelectInputParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- showInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:@
showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterShowInputStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaInput showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaInputClusterShowInputStatusParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- showInputStatusWithExpectedValues:expectedValueInterval:completionHandler:@
showInputStatusWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
showInputStatusWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaInput showInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- hideInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:@
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterHideInputStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaInput hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaInputClusterHideInputStatusParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- hideInputStatusWithExpectedValues:expectedValueInterval:completionHandler:@
hideInputStatusWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
hideInputStatusWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaInput hideInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- renameInputWithParams:expectedValues:expectedValueInterval:completionHandler:@
renameInputWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRMediaInputClusterRenameInputParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
renameInputWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaInput params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterMediaInput renameInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRMediaInputClusterRenameInputParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMediaInput mtrClusterMediaInput, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMediaInput -> device -> endpointID -> queue -> IO (Id MTRClusterMediaInput)
initWithDevice_endpointID_queue mtrClusterMediaInput device endpointID queue =
  sendOwnedMessage mtrClusterMediaInput initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectInputWithParams:expectedValues:expectedValueInterval:completion:@
selectInputWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaInputClusterSelectInputParams, Id NSArray, Id NSNumber, Ptr ()] ()
selectInputWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "selectInputWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @showInputStatusWithParams:expectedValues:expectedValueInterval:completion:@
showInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaInputClusterShowInputStatusParams, Id NSArray, Id NSNumber, Ptr ()] ()
showInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "showInputStatusWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @showInputStatusWithExpectedValues:expectedValueInterval:completion:@
showInputStatusWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
showInputStatusWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "showInputStatusWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @hideInputStatusWithParams:expectedValues:expectedValueInterval:completion:@
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaInputClusterHideInputStatusParams, Id NSArray, Id NSNumber, Ptr ()] ()
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "hideInputStatusWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @hideInputStatusWithExpectedValues:expectedValueInterval:completion:@
hideInputStatusWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
hideInputStatusWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "hideInputStatusWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @renameInputWithParams:expectedValues:expectedValueInterval:completion:@
renameInputWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMediaInputClusterRenameInputParams, Id NSArray, Id NSNumber, Ptr ()] ()
renameInputWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "renameInputWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeInputListWithParams:@
readAttributeInputListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInputListWithParamsSelector = mkSelector "readAttributeInputListWithParams:"

-- | @Selector@ for @readAttributeCurrentInputWithParams:@
readAttributeCurrentInputWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentInputWithParamsSelector = mkSelector "readAttributeCurrentInputWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterMediaInput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterMediaInput)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterMediaInput)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @selectInputWithParams:expectedValues:expectedValueInterval:completionHandler:@
selectInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaInputClusterSelectInputParams, Id NSArray, Id NSNumber, Ptr ()] ()
selectInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "selectInputWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @showInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:@
showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaInputClusterShowInputStatusParams, Id NSArray, Id NSNumber, Ptr ()] ()
showInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "showInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @showInputStatusWithExpectedValues:expectedValueInterval:completionHandler:@
showInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
showInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "showInputStatusWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @hideInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:@
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaInputClusterHideInputStatusParams, Id NSArray, Id NSNumber, Ptr ()] ()
hideInputStatusWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "hideInputStatusWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @hideInputStatusWithExpectedValues:expectedValueInterval:completionHandler:@
hideInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
hideInputStatusWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "hideInputStatusWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @renameInputWithParams:expectedValues:expectedValueInterval:completionHandler:@
renameInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRMediaInputClusterRenameInputParams, Id NSArray, Id NSNumber, Ptr ()] ()
renameInputWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "renameInputWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterMediaInput)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

