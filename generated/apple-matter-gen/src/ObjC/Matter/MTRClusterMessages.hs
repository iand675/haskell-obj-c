{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Messages    This cluster provides an interface for passing messages to be presented by a device.
--
-- Generated bindings for @MTRClusterMessages@.
module ObjC.Matter.MTRClusterMessages
  ( MTRClusterMessages
  , IsMTRClusterMessages(..)
  , presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completion
  , cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMessagesWithParams
  , readAttributeActiveMessageIDsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeActiveMessageIDsWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMessagesWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presentMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:@
presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMessages mtrClusterMessages, IsMTRMessagesClusterPresentMessagesRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMessages -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterMessages params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMessages presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMessagesClusterPresentMessagesRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- cancelMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMessages mtrClusterMessages, IsMTRMessagesClusterCancelMessagesRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMessages -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterMessages params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMessages cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMessagesClusterCancelMessagesRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeMessagesWithParams:@
readAttributeMessagesWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeMessagesWithParams mtrClusterMessages params =
  sendMessage mtrClusterMessages readAttributeMessagesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveMessageIDsWithParams:@
readAttributeActiveMessageIDsWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeActiveMessageIDsWithParams mtrClusterMessages params =
  sendMessage mtrClusterMessages readAttributeActiveMessageIDsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMessages params =
  sendMessage mtrClusterMessages readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMessages params =
  sendMessage mtrClusterMessages readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMessages params =
  sendMessage mtrClusterMessages readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMessages params =
  sendMessage mtrClusterMessages readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMessages params =
  sendMessage mtrClusterMessages readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterMessages mtrClusterMessages => mtrClusterMessages -> IO (Id MTRClusterMessages)
init_ mtrClusterMessages =
  sendOwnedMessage mtrClusterMessages initSelector

-- | @+ new@
new :: IO (Id MTRClusterMessages)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMessages"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMessages mtrClusterMessages, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMessages -> device -> endpointID -> queue -> IO (Id MTRClusterMessages)
initWithDevice_endpointID_queue mtrClusterMessages device endpointID queue =
  sendOwnedMessage mtrClusterMessages initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:@
presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMessagesClusterPresentMessagesRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "presentMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMessagesClusterCancelMessagesRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "cancelMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMessagesWithParams:@
readAttributeMessagesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMessagesWithParamsSelector = mkSelector "readAttributeMessagesWithParams:"

-- | @Selector@ for @readAttributeActiveMessageIDsWithParams:@
readAttributeActiveMessageIDsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveMessageIDsWithParamsSelector = mkSelector "readAttributeActiveMessageIDsWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterMessages)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterMessages)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterMessages)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

