{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Keypad Input    This cluster provides an interface for controlling a device like a TV using action commands such as UP, DOWN, and SELECT.
--
-- Generated bindings for @MTRClusterKeypadInput@.
module ObjC.Matter.MTRClusterKeypadInput
  ( MTRClusterKeypadInput
  , IsMTRClusterKeypadInput(..)
  , sendKeyWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , sendKeyWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , sendKeyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , sendKeyWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sendKeyWithParams:expectedValues:expectedValueInterval:completion:@
sendKeyWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRKeypadInputClusterSendKeyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterKeypadInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
sendKeyWithParams_expectedValues_expectedValueInterval_completion mtrClusterKeypadInput params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterKeypadInput sendKeyWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRKeypadInputClusterSendKeyParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRReadParams params) => mtrClusterKeypadInput -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterKeypadInput params =
  sendMessage mtrClusterKeypadInput readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRReadParams params) => mtrClusterKeypadInput -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterKeypadInput params =
  sendMessage mtrClusterKeypadInput readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRReadParams params) => mtrClusterKeypadInput -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterKeypadInput params =
  sendMessage mtrClusterKeypadInput readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRReadParams params) => mtrClusterKeypadInput -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterKeypadInput params =
  sendMessage mtrClusterKeypadInput readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRReadParams params) => mtrClusterKeypadInput -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterKeypadInput params =
  sendMessage mtrClusterKeypadInput readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterKeypadInput mtrClusterKeypadInput => mtrClusterKeypadInput -> IO (Id MTRClusterKeypadInput)
init_ mtrClusterKeypadInput =
  sendOwnedMessage mtrClusterKeypadInput initSelector

-- | @+ new@
new :: IO (Id MTRClusterKeypadInput)
new  =
  do
    cls' <- getRequiredClass "MTRClusterKeypadInput"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRDevice device, IsNSObject queue) => mtrClusterKeypadInput -> device -> CUShort -> queue -> IO (Id MTRClusterKeypadInput)
initWithDevice_endpoint_queue mtrClusterKeypadInput device endpoint queue =
  sendOwnedMessage mtrClusterKeypadInput initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- sendKeyWithParams:expectedValues:expectedValueInterval:completionHandler:@
sendKeyWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRKeypadInputClusterSendKeyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterKeypadInput -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
sendKeyWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterKeypadInput params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterKeypadInput sendKeyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRKeypadInputClusterSendKeyParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterKeypadInput mtrClusterKeypadInput, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterKeypadInput -> device -> endpointID -> queue -> IO (Id MTRClusterKeypadInput)
initWithDevice_endpointID_queue mtrClusterKeypadInput device endpointID queue =
  sendOwnedMessage mtrClusterKeypadInput initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sendKeyWithParams:expectedValues:expectedValueInterval:completion:@
sendKeyWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRKeypadInputClusterSendKeyParams, Id NSArray, Id NSNumber, Ptr ()] ()
sendKeyWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "sendKeyWithParams:expectedValues:expectedValueInterval:completion:"

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
initSelector :: Selector '[] (Id MTRClusterKeypadInput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterKeypadInput)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterKeypadInput)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @sendKeyWithParams:expectedValues:expectedValueInterval:completionHandler:@
sendKeyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRKeypadInputClusterSendKeyParams, Id NSArray, Id NSNumber, Ptr ()] ()
sendKeyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "sendKeyWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterKeypadInput)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

