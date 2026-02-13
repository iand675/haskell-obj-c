{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster OTA Software Update Provider    Provides an interface for providing OTA software updates
--
-- Generated bindings for @MTRClusterOTASoftwareUpdateProvider@.
module ObjC.Matter.MTRClusterOTASoftwareUpdateProvider
  ( MTRClusterOTASoftwareUpdateProvider
  , IsMTRClusterOTASoftwareUpdateProvider(..)
  , queryImageWithParams_expectedValues_expectedValueInterval_completion
  , applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completion
  , notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completionSelector
  , queryImageWithParams_expectedValues_expectedValueInterval_completionSelector
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

-- | @- queryImageWithParams:expectedValues:expectedValueInterval:completion:@
queryImageWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOTASoftwareUpdateProvider mtrClusterOTASoftwareUpdateProvider, IsMTROTASoftwareUpdateProviderClusterQueryImageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOTASoftwareUpdateProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
queryImageWithParams_expectedValues_expectedValueInterval_completion mtrClusterOTASoftwareUpdateProvider params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOTASoftwareUpdateProvider queryImageWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROTASoftwareUpdateProviderClusterQueryImageParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- applyUpdateRequestWithParams:expectedValues:expectedValueInterval:completion:@
applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOTASoftwareUpdateProvider mtrClusterOTASoftwareUpdateProvider, IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOTASoftwareUpdateProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterOTASoftwareUpdateProvider params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOTASoftwareUpdateProvider applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- notifyUpdateAppliedWithParams:expectedValues:expectedValueInterval:completion:@
notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOTASoftwareUpdateProvider mtrClusterOTASoftwareUpdateProvider, IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOTASoftwareUpdateProvider -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completion mtrClusterOTASoftwareUpdateProvider params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOTASoftwareUpdateProvider notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOTASoftwareUpdateProvider mtrClusterOTASoftwareUpdateProvider, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateProvider -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOTASoftwareUpdateProvider params =
  sendMessage mtrClusterOTASoftwareUpdateProvider readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOTASoftwareUpdateProvider mtrClusterOTASoftwareUpdateProvider, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateProvider -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOTASoftwareUpdateProvider params =
  sendMessage mtrClusterOTASoftwareUpdateProvider readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOTASoftwareUpdateProvider mtrClusterOTASoftwareUpdateProvider, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateProvider -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOTASoftwareUpdateProvider params =
  sendMessage mtrClusterOTASoftwareUpdateProvider readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOTASoftwareUpdateProvider mtrClusterOTASoftwareUpdateProvider, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateProvider -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOTASoftwareUpdateProvider params =
  sendMessage mtrClusterOTASoftwareUpdateProvider readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOTASoftwareUpdateProvider mtrClusterOTASoftwareUpdateProvider, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateProvider -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOTASoftwareUpdateProvider params =
  sendMessage mtrClusterOTASoftwareUpdateProvider readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterOTASoftwareUpdateProvider mtrClusterOTASoftwareUpdateProvider => mtrClusterOTASoftwareUpdateProvider -> IO (Id MTRClusterOTASoftwareUpdateProvider)
init_ mtrClusterOTASoftwareUpdateProvider =
  sendOwnedMessage mtrClusterOTASoftwareUpdateProvider initSelector

-- | @+ new@
new :: IO (Id MTRClusterOTASoftwareUpdateProvider)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOTASoftwareUpdateProvider"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOTASoftwareUpdateProvider mtrClusterOTASoftwareUpdateProvider, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOTASoftwareUpdateProvider -> device -> endpointID -> queue -> IO (Id MTRClusterOTASoftwareUpdateProvider)
initWithDevice_endpointID_queue mtrClusterOTASoftwareUpdateProvider device endpointID queue =
  sendOwnedMessage mtrClusterOTASoftwareUpdateProvider initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @queryImageWithParams:expectedValues:expectedValueInterval:completion:@
queryImageWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROTASoftwareUpdateProviderClusterQueryImageParams, Id NSArray, Id NSNumber, Ptr ()] ()
queryImageWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "queryImageWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @applyUpdateRequestWithParams:expectedValues:expectedValueInterval:completion:@
applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
applyUpdateRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "applyUpdateRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @notifyUpdateAppliedWithParams:expectedValues:expectedValueInterval:completion:@
notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams, Id NSArray, Id NSNumber, Ptr ()] ()
notifyUpdateAppliedWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "notifyUpdateAppliedWithParams:expectedValues:expectedValueInterval:completion:"

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
initSelector :: Selector '[] (Id MTRClusterOTASoftwareUpdateProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterOTASoftwareUpdateProvider)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterOTASoftwareUpdateProvider)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

