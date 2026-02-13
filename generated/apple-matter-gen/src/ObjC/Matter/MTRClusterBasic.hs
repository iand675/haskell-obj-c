{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClusterBasic@.
module ObjC.Matter.MTRClusterBasic
  ( MTRClusterBasic
  , IsMTRClusterBasic(..)
  , initWithDevice_endpoint_queue
  , mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandler
  , mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpoint_queueSelector
  , mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBasic mtrClusterBasic, IsMTRDevice device, IsNSObject queue) => mtrClusterBasic -> device -> CUShort -> queue -> IO (Id MTRClusterBasic)
initWithDevice_endpoint_queue mtrClusterBasic device endpoint queue =
  sendOwnedMessage mtrClusterBasic initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- mfgSpecificPingWithParams:expectedValues:expectedValueInterval:completionHandler:@
mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterBasic mtrClusterBasic, IsMTRBasicClusterMfgSpecificPingParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBasic -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterBasic params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterBasic mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRBasicClusterMfgSpecificPingParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- mfgSpecificPingWithExpectedValues:expectedValueInterval:completionHandler:@
mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterBasic mtrClusterBasic, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterBasic -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandler mtrClusterBasic expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterBasic mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterBasic)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @mfgSpecificPingWithParams:expectedValues:expectedValueInterval:completionHandler:@
mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRBasicClusterMfgSpecificPingParams, Id NSArray, Id NSNumber, Ptr ()] ()
mfgSpecificPingWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "mfgSpecificPingWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @mfgSpecificPingWithExpectedValues:expectedValueInterval:completionHandler:@
mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
mfgSpecificPingWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "mfgSpecificPingWithExpectedValues:expectedValueInterval:completionHandler:"

