{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Account Login    This cluster provides commands that facilitate user account login on a Content App or a node. For example, a Content App running on a Video Player device, which is represented as an endpoint (see [TV Architecture]), can use this cluster to help make the user account on the Content App match the user account on the Client.
--
-- Generated bindings for @MTRClusterAccountLogin@.
module ObjC.Matter.MTRClusterAccountLogin
  ( MTRClusterAccountLogin
  , IsMTRClusterAccountLogin(..)
  , getSetupPINWithParams_expectedValues_expectedValueInterval_completion
  , loginWithParams_expectedValues_expectedValueInterval_completion
  , logoutWithParams_expectedValues_expectedValueInterval_completion
  , logoutWithExpectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandler
  , loginWithParams_expectedValues_expectedValueInterval_completionHandler
  , logoutWithParams_expectedValues_expectedValueInterval_completionHandler
  , logoutWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getSetupPINWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , loginWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , loginWithParams_expectedValues_expectedValueInterval_completionSelector
  , logoutWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , logoutWithExpectedValues_expectedValueInterval_completionSelector
  , logoutWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , logoutWithParams_expectedValues_expectedValueInterval_completionSelector
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

-- | @- getSetupPINWithParams:expectedValues:expectedValueInterval:completion:@
getSetupPINWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterGetSetupPINParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getSetupPINWithParams_expectedValues_expectedValueInterval_completion mtrClusterAccountLogin params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterAccountLogin getSetupPINWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRAccountLoginClusterGetSetupPINParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- loginWithParams:expectedValues:expectedValueInterval:completion:@
loginWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterLoginParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
loginWithParams_expectedValues_expectedValueInterval_completion mtrClusterAccountLogin params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterAccountLogin loginWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRAccountLoginClusterLoginParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- logoutWithParams:expectedValues:expectedValueInterval:completion:@
logoutWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterLogoutParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
logoutWithParams_expectedValues_expectedValueInterval_completion mtrClusterAccountLogin params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterAccountLogin logoutWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRAccountLoginClusterLogoutParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- logoutWithExpectedValues:expectedValueInterval:completion:@
logoutWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
logoutWithExpectedValues_expectedValueInterval_completion mtrClusterAccountLogin expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterAccountLogin logoutWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRReadParams params) => mtrClusterAccountLogin -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAccountLogin params =
  sendMessage mtrClusterAccountLogin readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRReadParams params) => mtrClusterAccountLogin -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAccountLogin params =
  sendMessage mtrClusterAccountLogin readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRReadParams params) => mtrClusterAccountLogin -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAccountLogin params =
  sendMessage mtrClusterAccountLogin readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRReadParams params) => mtrClusterAccountLogin -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAccountLogin params =
  sendMessage mtrClusterAccountLogin readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRReadParams params) => mtrClusterAccountLogin -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAccountLogin params =
  sendMessage mtrClusterAccountLogin readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterAccountLogin mtrClusterAccountLogin => mtrClusterAccountLogin -> IO (Id MTRClusterAccountLogin)
init_ mtrClusterAccountLogin =
  sendOwnedMessage mtrClusterAccountLogin initSelector

-- | @+ new@
new :: IO (Id MTRClusterAccountLogin)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAccountLogin"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRDevice device, IsNSObject queue) => mtrClusterAccountLogin -> device -> CUShort -> queue -> IO (Id MTRClusterAccountLogin)
initWithDevice_endpoint_queue mtrClusterAccountLogin device endpoint queue =
  sendOwnedMessage mtrClusterAccountLogin initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- getSetupPINWithParams:expectedValues:expectedValueInterval:completionHandler:@
getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterGetSetupPINParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAccountLogin params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterAccountLogin getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRAccountLoginClusterGetSetupPINParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- loginWithParams:expectedValues:expectedValueInterval:completionHandler:@
loginWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterLoginParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
loginWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAccountLogin params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterAccountLogin loginWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRAccountLoginClusterLoginParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- logoutWithParams:expectedValues:expectedValueInterval:completionHandler:@
logoutWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRAccountLoginClusterLogoutParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
logoutWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAccountLogin params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterAccountLogin logoutWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRAccountLoginClusterLogoutParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- logoutWithExpectedValues:expectedValueInterval:completionHandler:@
logoutWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterAccountLogin -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
logoutWithExpectedValues_expectedValueInterval_completionHandler mtrClusterAccountLogin expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterAccountLogin logoutWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAccountLogin mtrClusterAccountLogin, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAccountLogin -> device -> endpointID -> queue -> IO (Id MTRClusterAccountLogin)
initWithDevice_endpointID_queue mtrClusterAccountLogin device endpointID queue =
  sendOwnedMessage mtrClusterAccountLogin initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getSetupPINWithParams:expectedValues:expectedValueInterval:completion:@
getSetupPINWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRAccountLoginClusterGetSetupPINParams, Id NSArray, Id NSNumber, Ptr ()] ()
getSetupPINWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getSetupPINWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @loginWithParams:expectedValues:expectedValueInterval:completion:@
loginWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRAccountLoginClusterLoginParams, Id NSArray, Id NSNumber, Ptr ()] ()
loginWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "loginWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @logoutWithParams:expectedValues:expectedValueInterval:completion:@
logoutWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRAccountLoginClusterLogoutParams, Id NSArray, Id NSNumber, Ptr ()] ()
logoutWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "logoutWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @logoutWithExpectedValues:expectedValueInterval:completion:@
logoutWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
logoutWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "logoutWithExpectedValues:expectedValueInterval:completion:"

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
initSelector :: Selector '[] (Id MTRClusterAccountLogin)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterAccountLogin)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterAccountLogin)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @getSetupPINWithParams:expectedValues:expectedValueInterval:completionHandler:@
getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRAccountLoginClusterGetSetupPINParams, Id NSArray, Id NSNumber, Ptr ()] ()
getSetupPINWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getSetupPINWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @loginWithParams:expectedValues:expectedValueInterval:completionHandler:@
loginWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRAccountLoginClusterLoginParams, Id NSArray, Id NSNumber, Ptr ()] ()
loginWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "loginWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @logoutWithParams:expectedValues:expectedValueInterval:completionHandler:@
logoutWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRAccountLoginClusterLogoutParams, Id NSArray, Id NSNumber, Ptr ()] ()
logoutWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "logoutWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @logoutWithExpectedValues:expectedValueInterval:completionHandler:@
logoutWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
logoutWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "logoutWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterAccountLogin)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

