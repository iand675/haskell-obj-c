{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster On/Off    Attributes and commands for switching devices between 'On' and 'Off' states.
--
-- Generated bindings for @MTRClusterOnOff@.
module ObjC.Matter.MTRClusterOnOff
  ( MTRClusterOnOff
  , IsMTRClusterOnOff(..)
  , offWithParams_expectedValues_expectedValueInterval_completion
  , offWithExpectedValues_expectedValueInterval_completion
  , onWithParams_expectedValues_expectedValueInterval_completion
  , onWithExpectedValues_expectedValueInterval_completion
  , toggleWithParams_expectedValues_expectedValueInterval_completion
  , toggleWithExpectedValues_expectedValueInterval_completion
  , offWithEffectWithParams_expectedValues_expectedValueInterval_completion
  , onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completion
  , onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completion
  , onWithTimedOffWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeOnOffWithParams
  , readAttributeGlobalSceneControlWithParams
  , readAttributeOnTimeWithParams
  , writeAttributeOnTimeWithValue_expectedValueInterval
  , writeAttributeOnTimeWithValue_expectedValueInterval_params
  , readAttributeOffWaitTimeWithParams
  , writeAttributeOffWaitTimeWithValue_expectedValueInterval
  , writeAttributeOffWaitTimeWithValue_expectedValueInterval_params
  , readAttributeStartUpOnOffWithParams
  , writeAttributeStartUpOnOffWithValue_expectedValueInterval
  , writeAttributeStartUpOnOffWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , offWithParams_expectedValues_expectedValueInterval_completionHandler
  , offWithExpectedValues_expectedValueInterval_completionHandler
  , onWithParams_expectedValues_expectedValueInterval_completionHandler
  , onWithExpectedValues_expectedValueInterval_completionHandler
  , toggleWithParams_expectedValues_expectedValueInterval_completionHandler
  , toggleWithExpectedValues_expectedValueInterval_completionHandler
  , offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandler
  , onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandler
  , onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandler
  , onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , offWithEffectWithParams_expectedValues_expectedValueInterval_completionSelector
  , offWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , offWithExpectedValues_expectedValueInterval_completionSelector
  , offWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , offWithParams_expectedValues_expectedValueInterval_completionSelector
  , onWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , onWithExpectedValues_expectedValueInterval_completionSelector
  , onWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , onWithParams_expectedValues_expectedValueInterval_completionSelector
  , onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionSelector
  , onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeGlobalSceneControlWithParamsSelector
  , readAttributeOffWaitTimeWithParamsSelector
  , readAttributeOnOffWithParamsSelector
  , readAttributeOnTimeWithParamsSelector
  , readAttributeStartUpOnOffWithParamsSelector
  , toggleWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , toggleWithExpectedValues_expectedValueInterval_completionSelector
  , toggleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , toggleWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeOffWaitTimeWithValue_expectedValueIntervalSelector
  , writeAttributeOffWaitTimeWithValue_expectedValueInterval_paramsSelector
  , writeAttributeOnTimeWithValue_expectedValueIntervalSelector
  , writeAttributeOnTimeWithValue_expectedValueInterval_paramsSelector
  , writeAttributeStartUpOnOffWithValue_expectedValueIntervalSelector
  , writeAttributeStartUpOnOffWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- offWithParams:expectedValues:expectedValueInterval:completion:@
offWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOnOff offWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROnOffClusterOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- offWithExpectedValues:expectedValueInterval:completion:@
offWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithExpectedValues_expectedValueInterval_completion mtrClusterOnOff expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOnOff offWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- onWithParams:expectedValues:expectedValueInterval:completion:@
onWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOnOff onWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROnOffClusterOnParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- onWithExpectedValues:expectedValueInterval:completion:@
onWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithExpectedValues_expectedValueInterval_completion mtrClusterOnOff expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOnOff onWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- toggleWithParams:expectedValues:expectedValueInterval:completion:@
toggleWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterToggleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
toggleWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOnOff toggleWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROnOffClusterToggleParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- toggleWithExpectedValues:expectedValueInterval:completion:@
toggleWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
toggleWithExpectedValues_expectedValueInterval_completion mtrClusterOnOff expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOnOff toggleWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- offWithEffectWithParams:expectedValues:expectedValueInterval:completion:@
offWithEffectWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOffWithEffectParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithEffectWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOnOff offWithEffectWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROnOffClusterOffWithEffectParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completion:@
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnWithRecallGlobalSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOnOff onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROnOffClusterOnWithRecallGlobalSceneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completion:@
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completion mtrClusterOnOff expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOnOff onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- onWithTimedOffWithParams:expectedValues:expectedValueInterval:completion:@
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnWithTimedOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completion mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOnOff onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROnOffClusterOnWithTimedOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeOnOffWithParams:@
readAttributeOnOffWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeOnOffWithParams mtrClusterOnOff params =
  sendMessage mtrClusterOnOff readAttributeOnOffWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGlobalSceneControlWithParams:@
readAttributeGlobalSceneControlWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeGlobalSceneControlWithParams mtrClusterOnOff params =
  sendMessage mtrClusterOnOff readAttributeGlobalSceneControlWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOnTimeWithParams:@
readAttributeOnTimeWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeOnTimeWithParams mtrClusterOnOff params =
  sendMessage mtrClusterOnOff readAttributeOnTimeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOnTimeWithValue:expectedValueInterval:@
writeAttributeOnTimeWithValue_expectedValueInterval :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOnTimeWithValue_expectedValueInterval mtrClusterOnOff dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterOnOff writeAttributeOnTimeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOnTimeWithValue:expectedValueInterval:params:@
writeAttributeOnTimeWithValue_expectedValueInterval_params :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOnTimeWithValue_expectedValueInterval_params mtrClusterOnOff dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterOnOff writeAttributeOnTimeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeOffWaitTimeWithParams:@
readAttributeOffWaitTimeWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeOffWaitTimeWithParams mtrClusterOnOff params =
  sendMessage mtrClusterOnOff readAttributeOffWaitTimeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOffWaitTimeWithValue:expectedValueInterval:@
writeAttributeOffWaitTimeWithValue_expectedValueInterval :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOffWaitTimeWithValue_expectedValueInterval mtrClusterOnOff dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterOnOff writeAttributeOffWaitTimeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOffWaitTimeWithValue:expectedValueInterval:params:@
writeAttributeOffWaitTimeWithValue_expectedValueInterval_params :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOffWaitTimeWithValue_expectedValueInterval_params mtrClusterOnOff dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterOnOff writeAttributeOffWaitTimeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeStartUpOnOffWithParams:@
readAttributeStartUpOnOffWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeStartUpOnOffWithParams mtrClusterOnOff params =
  sendMessage mtrClusterOnOff readAttributeStartUpOnOffWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeStartUpOnOffWithValue:expectedValueInterval:@
writeAttributeStartUpOnOffWithValue_expectedValueInterval :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeStartUpOnOffWithValue_expectedValueInterval mtrClusterOnOff dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterOnOff writeAttributeStartUpOnOffWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeStartUpOnOffWithValue:expectedValueInterval:params:@
writeAttributeStartUpOnOffWithValue_expectedValueInterval_params :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOnOff -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeStartUpOnOffWithValue_expectedValueInterval_params mtrClusterOnOff dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterOnOff writeAttributeStartUpOnOffWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOnOff params =
  sendMessage mtrClusterOnOff readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOnOff params =
  sendMessage mtrClusterOnOff readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOnOff params =
  sendMessage mtrClusterOnOff readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOnOff params =
  sendMessage mtrClusterOnOff readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRReadParams params) => mtrClusterOnOff -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOnOff params =
  sendMessage mtrClusterOnOff readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterOnOff mtrClusterOnOff => mtrClusterOnOff -> IO (Id MTRClusterOnOff)
init_ mtrClusterOnOff =
  sendOwnedMessage mtrClusterOnOff initSelector

-- | @+ new@
new :: IO (Id MTRClusterOnOff)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOnOff"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRDevice device, IsNSObject queue) => mtrClusterOnOff -> device -> CUShort -> queue -> IO (Id MTRClusterOnOff)
initWithDevice_endpoint_queue mtrClusterOnOff device endpoint queue =
  sendOwnedMessage mtrClusterOnOff initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- offWithParams:expectedValues:expectedValueInterval:completionHandler:@
offWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOnOff offWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROnOffClusterOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- offWithExpectedValues:expectedValueInterval:completionHandler:@
offWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithExpectedValues_expectedValueInterval_completionHandler mtrClusterOnOff expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOnOff offWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- onWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOnOff onWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROnOffClusterOnParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- onWithExpectedValues:expectedValueInterval:completionHandler:@
onWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithExpectedValues_expectedValueInterval_completionHandler mtrClusterOnOff expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOnOff onWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- toggleWithParams:expectedValues:expectedValueInterval:completionHandler:@
toggleWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterToggleParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
toggleWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOnOff toggleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROnOffClusterToggleParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- toggleWithExpectedValues:expectedValueInterval:completionHandler:@
toggleWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
toggleWithExpectedValues_expectedValueInterval_completionHandler mtrClusterOnOff expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOnOff toggleWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- offWithEffectWithParams:expectedValues:expectedValueInterval:completionHandler:@
offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOffWithEffectParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOnOff offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROnOffClusterOffWithEffectParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnWithRecallGlobalSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOnOff onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROnOffClusterOnWithRecallGlobalSceneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completionHandler:@
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandler mtrClusterOnOff expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOnOff onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- onWithTimedOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTROnOffClusterOnWithTimedOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOnOff params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterOnOff onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTROnOffClusterOnWithTimedOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOnOff mtrClusterOnOff, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOnOff -> device -> endpointID -> queue -> IO (Id MTRClusterOnOff)
initWithDevice_endpointID_queue mtrClusterOnOff device endpointID queue =
  sendOwnedMessage mtrClusterOnOff initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offWithParams:expectedValues:expectedValueInterval:completion:@
offWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROnOffClusterOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
offWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "offWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @offWithExpectedValues:expectedValueInterval:completion:@
offWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
offWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "offWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @onWithParams:expectedValues:expectedValueInterval:completion:@
onWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROnOffClusterOnParams, Id NSArray, Id NSNumber, Ptr ()] ()
onWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "onWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @onWithExpectedValues:expectedValueInterval:completion:@
onWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
onWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "onWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @toggleWithParams:expectedValues:expectedValueInterval:completion:@
toggleWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROnOffClusterToggleParams, Id NSArray, Id NSNumber, Ptr ()] ()
toggleWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "toggleWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @toggleWithExpectedValues:expectedValueInterval:completion:@
toggleWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
toggleWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "toggleWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @offWithEffectWithParams:expectedValues:expectedValueInterval:completion:@
offWithEffectWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROnOffClusterOffWithEffectParams, Id NSArray, Id NSNumber, Ptr ()] ()
offWithEffectWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "offWithEffectWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completion:@
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROnOffClusterOnWithRecallGlobalSceneParams, Id NSArray, Id NSNumber, Ptr ()] ()
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completion:@
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @onWithTimedOffWithParams:expectedValues:expectedValueInterval:completion:@
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROnOffClusterOnWithTimedOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "onWithTimedOffWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeOnOffWithParams:@
readAttributeOnOffWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOnOffWithParamsSelector = mkSelector "readAttributeOnOffWithParams:"

-- | @Selector@ for @readAttributeGlobalSceneControlWithParams:@
readAttributeGlobalSceneControlWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGlobalSceneControlWithParamsSelector = mkSelector "readAttributeGlobalSceneControlWithParams:"

-- | @Selector@ for @readAttributeOnTimeWithParams:@
readAttributeOnTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOnTimeWithParamsSelector = mkSelector "readAttributeOnTimeWithParams:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:expectedValueInterval:@
writeAttributeOnTimeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOnTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOnTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOnTimeWithValue:expectedValueInterval:params:@
writeAttributeOnTimeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOnTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOnTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOffWaitTimeWithParams:@
readAttributeOffWaitTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOffWaitTimeWithParamsSelector = mkSelector "readAttributeOffWaitTimeWithParams:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:expectedValueInterval:@
writeAttributeOffWaitTimeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOffWaitTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOffWaitTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOffWaitTimeWithValue:expectedValueInterval:params:@
writeAttributeOffWaitTimeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOffWaitTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOffWaitTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeStartUpOnOffWithParams:@
readAttributeStartUpOnOffWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStartUpOnOffWithParamsSelector = mkSelector "readAttributeStartUpOnOffWithParams:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:expectedValueInterval:@
writeAttributeStartUpOnOffWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeStartUpOnOffWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeStartUpOnOffWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeStartUpOnOffWithValue:expectedValueInterval:params:@
writeAttributeStartUpOnOffWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeStartUpOnOffWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeStartUpOnOffWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterOnOff)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterOnOff)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterOnOff)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @offWithParams:expectedValues:expectedValueInterval:completionHandler:@
offWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROnOffClusterOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
offWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "offWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @offWithExpectedValues:expectedValueInterval:completionHandler:@
offWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
offWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "offWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @onWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROnOffClusterOnParams, Id NSArray, Id NSNumber, Ptr ()] ()
onWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "onWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @onWithExpectedValues:expectedValueInterval:completionHandler:@
onWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
onWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "onWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @toggleWithParams:expectedValues:expectedValueInterval:completionHandler:@
toggleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROnOffClusterToggleParams, Id NSArray, Id NSNumber, Ptr ()] ()
toggleWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "toggleWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @toggleWithExpectedValues:expectedValueInterval:completionHandler:@
toggleWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
toggleWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "toggleWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @offWithEffectWithParams:expectedValues:expectedValueInterval:completionHandler:@
offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROnOffClusterOffWithEffectParams, Id NSArray, Id NSNumber, Ptr ()] ()
offWithEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "offWithEffectWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROnOffClusterOnWithRecallGlobalSceneParams, Id NSArray, Id NSNumber, Ptr ()] ()
onWithRecallGlobalSceneWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "onWithRecallGlobalSceneWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completionHandler:@
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
onWithRecallGlobalSceneWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "onWithRecallGlobalSceneWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @onWithTimedOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTROnOffClusterOnWithTimedOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
onWithTimedOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "onWithTimedOffWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterOnOff)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

