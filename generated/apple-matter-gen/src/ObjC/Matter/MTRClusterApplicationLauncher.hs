{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Application Launcher    This cluster provides an interface for launching content on a media player device such as a TV or Speaker.
--
-- Generated bindings for @MTRClusterApplicationLauncher@.
module ObjC.Matter.MTRClusterApplicationLauncher
  ( MTRClusterApplicationLauncher
  , IsMTRClusterApplicationLauncher(..)
  , launchAppWithParams_expectedValues_expectedValueInterval_completion
  , launchAppWithExpectedValues_expectedValueInterval_completion
  , stopAppWithParams_expectedValues_expectedValueInterval_completion
  , stopAppWithExpectedValues_expectedValueInterval_completion
  , hideAppWithParams_expectedValues_expectedValueInterval_completion
  , hideAppWithExpectedValues_expectedValueInterval_completion
  , readAttributeCatalogListWithParams
  , readAttributeCurrentAppWithParams
  , writeAttributeCurrentAppWithValue_expectedValueInterval
  , writeAttributeCurrentAppWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , launchAppWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopAppWithParams_expectedValues_expectedValueInterval_completionHandler
  , hideAppWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , hideAppWithExpectedValues_expectedValueInterval_completionSelector
  , hideAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , hideAppWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , launchAppWithExpectedValues_expectedValueInterval_completionSelector
  , launchAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , launchAppWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeCatalogListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentAppWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , stopAppWithExpectedValues_expectedValueInterval_completionSelector
  , stopAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopAppWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeCurrentAppWithValue_expectedValueIntervalSelector
  , writeAttributeCurrentAppWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- launchAppWithParams:expectedValues:expectedValueInterval:completion:@
launchAppWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterLaunchAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchAppWithParams_expectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterApplicationLauncher launchAppWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRApplicationLauncherClusterLaunchAppParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- launchAppWithExpectedValues:expectedValueInterval:completion:@
launchAppWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
launchAppWithExpectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterApplicationLauncher launchAppWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopAppWithParams:expectedValues:expectedValueInterval:completion:@
stopAppWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterStopAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopAppWithParams_expectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterApplicationLauncher stopAppWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRApplicationLauncherClusterStopAppParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopAppWithExpectedValues:expectedValueInterval:completion:@
stopAppWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopAppWithExpectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterApplicationLauncher stopAppWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- hideAppWithParams:expectedValues:expectedValueInterval:completion:@
hideAppWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterHideAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
hideAppWithParams_expectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterApplicationLauncher hideAppWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRApplicationLauncherClusterHideAppParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- hideAppWithExpectedValues:expectedValueInterval:completion:@
hideAppWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
hideAppWithExpectedValues_expectedValueInterval_completion mtrClusterApplicationLauncher expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterApplicationLauncher hideAppWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeCatalogListWithParams:@
readAttributeCatalogListWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeCatalogListWithParams mtrClusterApplicationLauncher params =
  sendMessage mtrClusterApplicationLauncher readAttributeCatalogListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentAppWithParams:@
readAttributeCurrentAppWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeCurrentAppWithParams mtrClusterApplicationLauncher params =
  sendMessage mtrClusterApplicationLauncher readAttributeCurrentAppWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeCurrentAppWithValue:expectedValueInterval:@
writeAttributeCurrentAppWithValue_expectedValueInterval :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeCurrentAppWithValue_expectedValueInterval mtrClusterApplicationLauncher dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterApplicationLauncher writeAttributeCurrentAppWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeCurrentAppWithValue:expectedValueInterval:params:@
writeAttributeCurrentAppWithValue_expectedValueInterval_params :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterApplicationLauncher -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeCurrentAppWithValue_expectedValueInterval_params mtrClusterApplicationLauncher dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterApplicationLauncher writeAttributeCurrentAppWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterApplicationLauncher params =
  sendMessage mtrClusterApplicationLauncher readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterApplicationLauncher params =
  sendMessage mtrClusterApplicationLauncher readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterApplicationLauncher params =
  sendMessage mtrClusterApplicationLauncher readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterApplicationLauncher params =
  sendMessage mtrClusterApplicationLauncher readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRReadParams params) => mtrClusterApplicationLauncher -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterApplicationLauncher params =
  sendMessage mtrClusterApplicationLauncher readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher => mtrClusterApplicationLauncher -> IO (Id MTRClusterApplicationLauncher)
init_ mtrClusterApplicationLauncher =
  sendOwnedMessage mtrClusterApplicationLauncher initSelector

-- | @+ new@
new :: IO (Id MTRClusterApplicationLauncher)
new  =
  do
    cls' <- getRequiredClass "MTRClusterApplicationLauncher"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRDevice device, IsNSObject queue) => mtrClusterApplicationLauncher -> device -> CUShort -> queue -> IO (Id MTRClusterApplicationLauncher)
initWithDevice_endpoint_queue mtrClusterApplicationLauncher device endpoint queue =
  sendOwnedMessage mtrClusterApplicationLauncher initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- launchAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchAppWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterLaunchAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
launchAppWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterApplicationLauncher params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterApplicationLauncher launchAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRApplicationLauncherClusterLaunchAppParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- stopAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopAppWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterStopAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopAppWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterApplicationLauncher params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterApplicationLauncher stopAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRApplicationLauncherClusterStopAppParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- hideAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
hideAppWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRApplicationLauncherClusterHideAppParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterApplicationLauncher -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
hideAppWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterApplicationLauncher params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterApplicationLauncher hideAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRApplicationLauncherClusterHideAppParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterApplicationLauncher mtrClusterApplicationLauncher, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterApplicationLauncher -> device -> endpointID -> queue -> IO (Id MTRClusterApplicationLauncher)
initWithDevice_endpointID_queue mtrClusterApplicationLauncher device endpointID queue =
  sendOwnedMessage mtrClusterApplicationLauncher initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @launchAppWithParams:expectedValues:expectedValueInterval:completion:@
launchAppWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRApplicationLauncherClusterLaunchAppParams, Id NSArray, Id NSNumber, Ptr ()] ()
launchAppWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "launchAppWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @launchAppWithExpectedValues:expectedValueInterval:completion:@
launchAppWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
launchAppWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "launchAppWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopAppWithParams:expectedValues:expectedValueInterval:completion:@
stopAppWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRApplicationLauncherClusterStopAppParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopAppWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopAppWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopAppWithExpectedValues:expectedValueInterval:completion:@
stopAppWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
stopAppWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopAppWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @hideAppWithParams:expectedValues:expectedValueInterval:completion:@
hideAppWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRApplicationLauncherClusterHideAppParams, Id NSArray, Id NSNumber, Ptr ()] ()
hideAppWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "hideAppWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @hideAppWithExpectedValues:expectedValueInterval:completion:@
hideAppWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
hideAppWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "hideAppWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCatalogListWithParams:@
readAttributeCatalogListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCatalogListWithParamsSelector = mkSelector "readAttributeCatalogListWithParams:"

-- | @Selector@ for @readAttributeCurrentAppWithParams:@
readAttributeCurrentAppWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentAppWithParamsSelector = mkSelector "readAttributeCurrentAppWithParams:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:expectedValueInterval:@
writeAttributeCurrentAppWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeCurrentAppWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeCurrentAppWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:expectedValueInterval:params:@
writeAttributeCurrentAppWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeCurrentAppWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeCurrentAppWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterApplicationLauncher)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterApplicationLauncher)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterApplicationLauncher)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @launchAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
launchAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRApplicationLauncherClusterLaunchAppParams, Id NSArray, Id NSNumber, Ptr ()] ()
launchAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "launchAppWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRApplicationLauncherClusterStopAppParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopAppWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @hideAppWithParams:expectedValues:expectedValueInterval:completionHandler:@
hideAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRApplicationLauncherClusterHideAppParams, Id NSArray, Id NSNumber, Ptr ()] ()
hideAppWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "hideAppWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterApplicationLauncher)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

