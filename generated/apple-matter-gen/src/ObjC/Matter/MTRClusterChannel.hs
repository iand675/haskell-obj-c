{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Channel    This cluster provides an interface for controlling the current Channel on a device.
--
-- Generated bindings for @MTRClusterChannel@.
module ObjC.Matter.MTRClusterChannel
  ( MTRClusterChannel
  , IsMTRClusterChannel(..)
  , changeChannelWithParams_expectedValues_expectedValueInterval_completion
  , changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completion
  , skipChannelWithParams_expectedValues_expectedValueInterval_completion
  , getProgramGuideWithParams_expectedValues_expectedValueInterval_completion
  , getProgramGuideWithExpectedValues_expectedValueInterval_completion
  , recordProgramWithParams_expectedValues_expectedValueInterval_completion
  , cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeChannelListWithParams
  , readAttributeLineupWithParams
  , readAttributeCurrentChannelWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , changeChannelWithParams_expectedValues_expectedValueInterval_completionHandler
  , changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandler
  , skipChannelWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completionSelector
  , changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionSelector
  , changeChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , changeChannelWithParams_expectedValues_expectedValueInterval_completionSelector
  , getProgramGuideWithExpectedValues_expectedValueInterval_completionSelector
  , getProgramGuideWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeChannelListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentChannelWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLineupWithParamsSelector
  , recordProgramWithParams_expectedValues_expectedValueInterval_completionSelector
  , skipChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , skipChannelWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- changeChannelWithParams:expectedValues:expectedValueInterval:completion:@
changeChannelWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterChangeChannelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeChannelWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterChannel changeChannelWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRChannelClusterChangeChannelParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completion:@
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterChangeChannelByNumberParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterChannel changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRChannelClusterChangeChannelByNumberParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- skipChannelWithParams:expectedValues:expectedValueInterval:completion:@
skipChannelWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterSkipChannelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipChannelWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterChannel skipChannelWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRChannelClusterSkipChannelParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getProgramGuideWithParams:expectedValues:expectedValueInterval:completion:@
getProgramGuideWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterGetProgramGuideParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getProgramGuideWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterChannel getProgramGuideWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRChannelClusterGetProgramGuideParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getProgramGuideWithExpectedValues:expectedValueInterval:completion:@
getProgramGuideWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getProgramGuideWithExpectedValues_expectedValueInterval_completion mtrClusterChannel expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterChannel getProgramGuideWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- recordProgramWithParams:expectedValues:expectedValueInterval:completion:@
recordProgramWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterRecordProgramParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
recordProgramWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterChannel recordProgramWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRChannelClusterRecordProgramParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- cancelRecordProgramWithParams:expectedValues:expectedValueInterval:completion:@
cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterCancelRecordProgramParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterChannel cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRChannelClusterCancelRecordProgramParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeChannelListWithParams:@
readAttributeChannelListWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeChannelListWithParams mtrClusterChannel params =
  sendMessage mtrClusterChannel readAttributeChannelListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLineupWithParams:@
readAttributeLineupWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeLineupWithParams mtrClusterChannel params =
  sendMessage mtrClusterChannel readAttributeLineupWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentChannelWithParams:@
readAttributeCurrentChannelWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeCurrentChannelWithParams mtrClusterChannel params =
  sendMessage mtrClusterChannel readAttributeCurrentChannelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterChannel params =
  sendMessage mtrClusterChannel readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterChannel params =
  sendMessage mtrClusterChannel readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterChannel params =
  sendMessage mtrClusterChannel readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterChannel params =
  sendMessage mtrClusterChannel readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterChannel params =
  sendMessage mtrClusterChannel readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterChannel mtrClusterChannel => mtrClusterChannel -> IO (Id MTRClusterChannel)
init_ mtrClusterChannel =
  sendOwnedMessage mtrClusterChannel initSelector

-- | @+ new@
new :: IO (Id MTRClusterChannel)
new  =
  do
    cls' <- getRequiredClass "MTRClusterChannel"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterChannel mtrClusterChannel, IsMTRDevice device, IsNSObject queue) => mtrClusterChannel -> device -> CUShort -> queue -> IO (Id MTRClusterChannel)
initWithDevice_endpoint_queue mtrClusterChannel device endpoint queue =
  sendOwnedMessage mtrClusterChannel initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- changeChannelWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeChannelWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterChangeChannelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeChannelWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterChannel params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterChannel changeChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRChannelClusterChangeChannelParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterChangeChannelByNumberParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterChannel params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterChannel changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRChannelClusterChangeChannelByNumberParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- skipChannelWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipChannelWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterSkipChannelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipChannelWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterChannel params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterChannel skipChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRChannelClusterSkipChannelParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterChannel mtrClusterChannel, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterChannel -> device -> endpointID -> queue -> IO (Id MTRClusterChannel)
initWithDevice_endpointID_queue mtrClusterChannel device endpointID queue =
  sendOwnedMessage mtrClusterChannel initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeChannelWithParams:expectedValues:expectedValueInterval:completion:@
changeChannelWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRChannelClusterChangeChannelParams, Id NSArray, Id NSNumber, Ptr ()] ()
changeChannelWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "changeChannelWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completion:@
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRChannelClusterChangeChannelByNumberParams, Id NSArray, Id NSNumber, Ptr ()] ()
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @skipChannelWithParams:expectedValues:expectedValueInterval:completion:@
skipChannelWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRChannelClusterSkipChannelParams, Id NSArray, Id NSNumber, Ptr ()] ()
skipChannelWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "skipChannelWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getProgramGuideWithParams:expectedValues:expectedValueInterval:completion:@
getProgramGuideWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRChannelClusterGetProgramGuideParams, Id NSArray, Id NSNumber, Ptr ()] ()
getProgramGuideWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getProgramGuideWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getProgramGuideWithExpectedValues:expectedValueInterval:completion:@
getProgramGuideWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
getProgramGuideWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "getProgramGuideWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @recordProgramWithParams:expectedValues:expectedValueInterval:completion:@
recordProgramWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRChannelClusterRecordProgramParams, Id NSArray, Id NSNumber, Ptr ()] ()
recordProgramWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "recordProgramWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelRecordProgramWithParams:expectedValues:expectedValueInterval:completion:@
cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRChannelClusterCancelRecordProgramParams, Id NSArray, Id NSNumber, Ptr ()] ()
cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "cancelRecordProgramWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeChannelListWithParams:@
readAttributeChannelListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeChannelListWithParamsSelector = mkSelector "readAttributeChannelListWithParams:"

-- | @Selector@ for @readAttributeLineupWithParams:@
readAttributeLineupWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLineupWithParamsSelector = mkSelector "readAttributeLineupWithParams:"

-- | @Selector@ for @readAttributeCurrentChannelWithParams:@
readAttributeCurrentChannelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentChannelWithParamsSelector = mkSelector "readAttributeCurrentChannelWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterChannel)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterChannel)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterChannel)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @changeChannelWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRChannelClusterChangeChannelParams, Id NSArray, Id NSNumber, Ptr ()] ()
changeChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "changeChannelWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRChannelClusterChangeChannelByNumberParams, Id NSArray, Id NSNumber, Ptr ()] ()
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @skipChannelWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRChannelClusterSkipChannelParams, Id NSArray, Id NSNumber, Ptr ()] ()
skipChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "skipChannelWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterChannel)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

