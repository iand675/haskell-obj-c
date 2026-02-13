{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Content Launcher    This cluster provides an interface for launching content on a media player device such as a TV or Speaker.
--
-- Generated bindings for @MTRClusterContentLauncher@.
module ObjC.Matter.MTRClusterContentLauncher
  ( MTRClusterContentLauncher
  , IsMTRClusterContentLauncher(..)
  , launchContentWithParams_expectedValues_expectedValueInterval_completion
  , launchURLWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeAcceptHeaderWithParams
  , readAttributeSupportedStreamingProtocolsWithParams
  , writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval
  , writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , launchContentWithParams_expectedValues_expectedValueInterval_completionHandler
  , launchURLWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , launchContentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , launchContentWithParams_expectedValues_expectedValueInterval_completionSelector
  , launchURLWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , launchURLWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptHeaderWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSupportedStreamingProtocolsWithParamsSelector
  , writeAttributeSupportedStreamingProtocolsWithValue_expectedValueIntervalSelector
  , writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- launchContentWithParams:expectedValues:expectedValueInterval:completion:@
launchContentWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRContentLauncherClusterLaunchContentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchContentWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentLauncher params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentLauncher launchContentWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentLauncherClusterLaunchContentParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- launchURLWithParams:expectedValues:expectedValueInterval:completion:@
launchURLWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRContentLauncherClusterLaunchURLParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchURLWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentLauncher params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentLauncher launchURLWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentLauncherClusterLaunchURLParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeAcceptHeaderWithParams:@
readAttributeAcceptHeaderWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeAcceptHeaderWithParams mtrClusterContentLauncher params =
  sendMessage mtrClusterContentLauncher readAttributeAcceptHeaderWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportedStreamingProtocolsWithParams:@
readAttributeSupportedStreamingProtocolsWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeSupportedStreamingProtocolsWithParams mtrClusterContentLauncher params =
  sendMessage mtrClusterContentLauncher readAttributeSupportedStreamingProtocolsWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:@
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterContentLauncher -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval mtrClusterContentLauncher dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterContentLauncher writeAttributeSupportedStreamingProtocolsWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:params:@
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_params :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterContentLauncher -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_params mtrClusterContentLauncher dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterContentLauncher writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterContentLauncher params =
  sendMessage mtrClusterContentLauncher readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterContentLauncher params =
  sendMessage mtrClusterContentLauncher readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterContentLauncher params =
  sendMessage mtrClusterContentLauncher readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterContentLauncher params =
  sendMessage mtrClusterContentLauncher readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRReadParams params) => mtrClusterContentLauncher -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterContentLauncher params =
  sendMessage mtrClusterContentLauncher readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterContentLauncher mtrClusterContentLauncher => mtrClusterContentLauncher -> IO (Id MTRClusterContentLauncher)
init_ mtrClusterContentLauncher =
  sendOwnedMessage mtrClusterContentLauncher initSelector

-- | @+ new@
new :: IO (Id MTRClusterContentLauncher)
new  =
  do
    cls' <- getRequiredClass "MTRClusterContentLauncher"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRDevice device, IsNSObject queue) => mtrClusterContentLauncher -> device -> CUShort -> queue -> IO (Id MTRClusterContentLauncher)
initWithDevice_endpoint_queue mtrClusterContentLauncher device endpoint queue =
  sendOwnedMessage mtrClusterContentLauncher initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- launchContentWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchContentWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRContentLauncherClusterLaunchContentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchContentWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterContentLauncher params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterContentLauncher launchContentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRContentLauncherClusterLaunchContentParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- launchURLWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchURLWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRContentLauncherClusterLaunchURLParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchURLWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterContentLauncher params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterContentLauncher launchURLWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRContentLauncherClusterLaunchURLParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterContentLauncher mtrClusterContentLauncher, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterContentLauncher -> device -> endpointID -> queue -> IO (Id MTRClusterContentLauncher)
initWithDevice_endpointID_queue mtrClusterContentLauncher device endpointID queue =
  sendOwnedMessage mtrClusterContentLauncher initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @launchContentWithParams:expectedValues:expectedValueInterval:completion:@
launchContentWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentLauncherClusterLaunchContentParams, Id NSArray, Id NSNumber, Ptr ()] ()
launchContentWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "launchContentWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @launchURLWithParams:expectedValues:expectedValueInterval:completion:@
launchURLWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentLauncherClusterLaunchURLParams, Id NSArray, Id NSNumber, Ptr ()] ()
launchURLWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "launchURLWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeAcceptHeaderWithParams:@
readAttributeAcceptHeaderWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcceptHeaderWithParamsSelector = mkSelector "readAttributeAcceptHeaderWithParams:"

-- | @Selector@ for @readAttributeSupportedStreamingProtocolsWithParams:@
readAttributeSupportedStreamingProtocolsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedStreamingProtocolsWithParamsSelector = mkSelector "readAttributeSupportedStreamingProtocolsWithParams:"

-- | @Selector@ for @writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:@
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:params:@
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeSupportedStreamingProtocolsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSupportedStreamingProtocolsWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterContentLauncher)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterContentLauncher)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterContentLauncher)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @launchContentWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchContentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRContentLauncherClusterLaunchContentParams, Id NSArray, Id NSNumber, Ptr ()] ()
launchContentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "launchContentWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @launchURLWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchURLWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRContentLauncherClusterLaunchURLParams, Id NSArray, Id NSNumber, Ptr ()] ()
launchURLWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "launchURLWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterContentLauncher)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

