{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Wi-Fi Network Management    Functionality to retrieve operational information about a managed Wi-Fi network.
--
-- Generated bindings for @MTRClusterWiFiNetworkManagement@.
module ObjC.Matter.MTRClusterWiFiNetworkManagement
  ( MTRClusterWiFiNetworkManagement
  , IsMTRClusterWiFiNetworkManagement(..)
  , networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completion
  , networkPassphraseRequestWithExpectedValues_expectedValueInterval_completion
  , readAttributeSSIDWithParams
  , readAttributePassphraseSurrogateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , networkPassphraseRequestWithExpectedValues_expectedValueInterval_completionSelector
  , networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributePassphraseSurrogateWithParamsSelector
  , readAttributeSSIDWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- networkPassphraseRequestWithParams:expectedValues:expectedValueInterval:completion:@
networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRWiFiNetworkManagementClusterNetworkPassphraseRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWiFiNetworkManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterWiFiNetworkManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWiFiNetworkManagement networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWiFiNetworkManagementClusterNetworkPassphraseRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- networkPassphraseRequestWithExpectedValues:expectedValueInterval:completion:@
networkPassphraseRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWiFiNetworkManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
networkPassphraseRequestWithExpectedValues_expectedValueInterval_completion mtrClusterWiFiNetworkManagement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterWiFiNetworkManagement networkPassphraseRequestWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSSIDWithParams:@
readAttributeSSIDWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeSSIDWithParams mtrClusterWiFiNetworkManagement params =
  sendMessage mtrClusterWiFiNetworkManagement readAttributeSSIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePassphraseSurrogateWithParams:@
readAttributePassphraseSurrogateWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributePassphraseSurrogateWithParams mtrClusterWiFiNetworkManagement params =
  sendMessage mtrClusterWiFiNetworkManagement readAttributePassphraseSurrogateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWiFiNetworkManagement params =
  sendMessage mtrClusterWiFiNetworkManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWiFiNetworkManagement params =
  sendMessage mtrClusterWiFiNetworkManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWiFiNetworkManagement params =
  sendMessage mtrClusterWiFiNetworkManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWiFiNetworkManagement params =
  sendMessage mtrClusterWiFiNetworkManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRReadParams params) => mtrClusterWiFiNetworkManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWiFiNetworkManagement params =
  sendMessage mtrClusterWiFiNetworkManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement => mtrClusterWiFiNetworkManagement -> IO (Id MTRClusterWiFiNetworkManagement)
init_ mtrClusterWiFiNetworkManagement =
  sendOwnedMessage mtrClusterWiFiNetworkManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterWiFiNetworkManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWiFiNetworkManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWiFiNetworkManagement mtrClusterWiFiNetworkManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWiFiNetworkManagement -> device -> endpointID -> queue -> IO (Id MTRClusterWiFiNetworkManagement)
initWithDevice_endpointID_queue mtrClusterWiFiNetworkManagement device endpointID queue =
  sendOwnedMessage mtrClusterWiFiNetworkManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkPassphraseRequestWithParams:expectedValues:expectedValueInterval:completion:@
networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWiFiNetworkManagementClusterNetworkPassphraseRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
networkPassphraseRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "networkPassphraseRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @networkPassphraseRequestWithExpectedValues:expectedValueInterval:completion:@
networkPassphraseRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
networkPassphraseRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "networkPassphraseRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSSIDWithParams:@
readAttributeSSIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSSIDWithParamsSelector = mkSelector "readAttributeSSIDWithParams:"

-- | @Selector@ for @readAttributePassphraseSurrogateWithParams:@
readAttributePassphraseSurrogateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePassphraseSurrogateWithParamsSelector = mkSelector "readAttributePassphraseSurrogateWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterWiFiNetworkManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterWiFiNetworkManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterWiFiNetworkManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

