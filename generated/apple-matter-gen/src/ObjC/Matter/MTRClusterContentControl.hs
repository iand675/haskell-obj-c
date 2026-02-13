{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Content Control    This cluster is used for managing the content control (including "parental control") settings on a media device such as a TV, or Set-top Box.
--
-- Generated bindings for @MTRClusterContentControl@.
module ObjC.Matter.MTRClusterContentControl
  ( MTRClusterContentControl
  , IsMTRClusterContentControl(..)
  , updatePINWithParams_expectedValues_expectedValueInterval_completion
  , resetPINWithParams_expectedValues_expectedValueInterval_completion
  , resetPINWithExpectedValues_expectedValueInterval_completion
  , enableWithParams_expectedValues_expectedValueInterval_completion
  , enableWithExpectedValues_expectedValueInterval_completion
  , disableWithParams_expectedValues_expectedValueInterval_completion
  , disableWithExpectedValues_expectedValueInterval_completion
  , addBonusTimeWithParams_expectedValues_expectedValueInterval_completion
  , addBonusTimeWithExpectedValues_expectedValueInterval_completion
  , setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completion
  , blockUnratedContentWithParams_expectedValues_expectedValueInterval_completion
  , blockUnratedContentWithExpectedValues_expectedValueInterval_completion
  , unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completion
  , unblockUnratedContentWithExpectedValues_expectedValueInterval_completion
  , setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completion
  , setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeEnabledWithParams
  , readAttributeOnDemandRatingsWithParams
  , readAttributeOnDemandRatingThresholdWithParams
  , readAttributeScheduledContentRatingsWithParams
  , readAttributeScheduledContentRatingThresholdWithParams
  , readAttributeScreenDailyTimeWithParams
  , readAttributeRemainingScreenTimeWithParams
  , readAttributeBlockUnratedWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addBonusTimeWithExpectedValues_expectedValueInterval_completionSelector
  , addBonusTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , blockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector
  , blockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector
  , disableWithExpectedValues_expectedValueInterval_completionSelector
  , disableWithParams_expectedValues_expectedValueInterval_completionSelector
  , enableWithExpectedValues_expectedValueInterval_completionSelector
  , enableWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeBlockUnratedWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeEnabledWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeOnDemandRatingThresholdWithParamsSelector
  , readAttributeOnDemandRatingsWithParamsSelector
  , readAttributeRemainingScreenTimeWithParamsSelector
  , readAttributeScheduledContentRatingThresholdWithParamsSelector
  , readAttributeScheduledContentRatingsWithParamsSelector
  , readAttributeScreenDailyTimeWithParamsSelector
  , resetPINWithExpectedValues_expectedValueInterval_completionSelector
  , resetPINWithParams_expectedValues_expectedValueInterval_completionSelector
  , setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector
  , setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector
  , setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , unblockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector
  , unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector
  , updatePINWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- updatePINWithParams:expectedValues:expectedValueInterval:completion:@
updatePINWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterUpdatePINParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updatePINWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl updatePINWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentControlClusterUpdatePINParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resetPINWithParams:expectedValues:expectedValueInterval:completion:@
resetPINWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterResetPINParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetPINWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl resetPINWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentControlClusterResetPINParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resetPINWithExpectedValues:expectedValueInterval:completion:@
resetPINWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetPINWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl resetPINWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- enableWithParams:expectedValues:expectedValueInterval:completion:@
enableWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterEnableParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl enableWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentControlClusterEnableParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- enableWithExpectedValues:expectedValueInterval:completion:@
enableWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
enableWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl enableWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- disableWithParams:expectedValues:expectedValueInterval:completion:@
disableWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterDisableParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
disableWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl disableWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentControlClusterDisableParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- disableWithExpectedValues:expectedValueInterval:completion:@
disableWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
disableWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl disableWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- addBonusTimeWithParams:expectedValues:expectedValueInterval:completion:@
addBonusTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterAddBonusTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addBonusTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl addBonusTimeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentControlClusterAddBonusTimeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- addBonusTimeWithExpectedValues:expectedValueInterval:completion:@
addBonusTimeWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
addBonusTimeWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl addBonusTimeWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- setScreenDailyTimeWithParams:expectedValues:expectedValueInterval:completion:@
setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterSetScreenDailyTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentControlClusterSetScreenDailyTimeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- blockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:@
blockUnratedContentWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterBlockUnratedContentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
blockUnratedContentWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl blockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentControlClusterBlockUnratedContentParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- blockUnratedContentWithExpectedValues:expectedValueInterval:completion:@
blockUnratedContentWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
blockUnratedContentWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl blockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- unblockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:@
unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterUnblockUnratedContentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentControlClusterUnblockUnratedContentParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- unblockUnratedContentWithExpectedValues:expectedValueInterval:completion:@
unblockUnratedContentWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
unblockUnratedContentWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl unblockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- setOnDemandRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:@
setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterSetOnDemandRatingThresholdParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentControlClusterSetOnDemandRatingThresholdParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setScheduledContentRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:@
setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterSetScheduledContentRatingThresholdParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterContentControl setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRContentControlClusterSetScheduledContentRatingThresholdParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeEnabledWithParams:@
readAttributeEnabledWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeEnabledWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeEnabledWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOnDemandRatingsWithParams:@
readAttributeOnDemandRatingsWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeOnDemandRatingsWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeOnDemandRatingsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOnDemandRatingThresholdWithParams:@
readAttributeOnDemandRatingThresholdWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeOnDemandRatingThresholdWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeOnDemandRatingThresholdWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeScheduledContentRatingsWithParams:@
readAttributeScheduledContentRatingsWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeScheduledContentRatingsWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeScheduledContentRatingsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeScheduledContentRatingThresholdWithParams:@
readAttributeScheduledContentRatingThresholdWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeScheduledContentRatingThresholdWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeScheduledContentRatingThresholdWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeScreenDailyTimeWithParams:@
readAttributeScreenDailyTimeWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeScreenDailyTimeWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeScreenDailyTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRemainingScreenTimeWithParams:@
readAttributeRemainingScreenTimeWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeRemainingScreenTimeWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeRemainingScreenTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBlockUnratedWithParams:@
readAttributeBlockUnratedWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeBlockUnratedWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeBlockUnratedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterContentControl params =
  sendMessage mtrClusterContentControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterContentControl mtrClusterContentControl => mtrClusterContentControl -> IO (Id MTRClusterContentControl)
init_ mtrClusterContentControl =
  sendOwnedMessage mtrClusterContentControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterContentControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterContentControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterContentControl -> device -> endpointID -> queue -> IO (Id MTRClusterContentControl)
initWithDevice_endpointID_queue mtrClusterContentControl device endpointID queue =
  sendOwnedMessage mtrClusterContentControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updatePINWithParams:expectedValues:expectedValueInterval:completion:@
updatePINWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentControlClusterUpdatePINParams, Id NSArray, Id NSNumber, Ptr ()] ()
updatePINWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updatePINWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetPINWithParams:expectedValues:expectedValueInterval:completion:@
resetPINWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentControlClusterResetPINParams, Id NSArray, Id NSNumber, Ptr ()] ()
resetPINWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetPINWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetPINWithExpectedValues:expectedValueInterval:completion:@
resetPINWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
resetPINWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetPINWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableWithParams:expectedValues:expectedValueInterval:completion:@
enableWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentControlClusterEnableParams, Id NSArray, Id NSNumber, Ptr ()] ()
enableWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enableWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableWithExpectedValues:expectedValueInterval:completion:@
enableWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
enableWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "enableWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @disableWithParams:expectedValues:expectedValueInterval:completion:@
disableWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentControlClusterDisableParams, Id NSArray, Id NSNumber, Ptr ()] ()
disableWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "disableWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @disableWithExpectedValues:expectedValueInterval:completion:@
disableWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
disableWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "disableWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addBonusTimeWithParams:expectedValues:expectedValueInterval:completion:@
addBonusTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentControlClusterAddBonusTimeParams, Id NSArray, Id NSNumber, Ptr ()] ()
addBonusTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addBonusTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addBonusTimeWithExpectedValues:expectedValueInterval:completion:@
addBonusTimeWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
addBonusTimeWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "addBonusTimeWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setScreenDailyTimeWithParams:expectedValues:expectedValueInterval:completion:@
setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentControlClusterSetScreenDailyTimeParams, Id NSArray, Id NSNumber, Ptr ()] ()
setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setScreenDailyTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @blockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:@
blockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentControlClusterBlockUnratedContentParams, Id NSArray, Id NSNumber, Ptr ()] ()
blockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "blockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @blockUnratedContentWithExpectedValues:expectedValueInterval:completion:@
blockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
blockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "blockUnratedContentWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unblockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:@
unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentControlClusterUnblockUnratedContentParams, Id NSArray, Id NSNumber, Ptr ()] ()
unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "unblockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unblockUnratedContentWithExpectedValues:expectedValueInterval:completion:@
unblockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
unblockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "unblockUnratedContentWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setOnDemandRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:@
setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentControlClusterSetOnDemandRatingThresholdParams, Id NSArray, Id NSNumber, Ptr ()] ()
setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setOnDemandRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setScheduledContentRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:@
setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRContentControlClusterSetScheduledContentRatingThresholdParams, Id NSArray, Id NSNumber, Ptr ()] ()
setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setScheduledContentRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeEnabledWithParams:@
readAttributeEnabledWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEnabledWithParamsSelector = mkSelector "readAttributeEnabledWithParams:"

-- | @Selector@ for @readAttributeOnDemandRatingsWithParams:@
readAttributeOnDemandRatingsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOnDemandRatingsWithParamsSelector = mkSelector "readAttributeOnDemandRatingsWithParams:"

-- | @Selector@ for @readAttributeOnDemandRatingThresholdWithParams:@
readAttributeOnDemandRatingThresholdWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOnDemandRatingThresholdWithParamsSelector = mkSelector "readAttributeOnDemandRatingThresholdWithParams:"

-- | @Selector@ for @readAttributeScheduledContentRatingsWithParams:@
readAttributeScheduledContentRatingsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeScheduledContentRatingsWithParamsSelector = mkSelector "readAttributeScheduledContentRatingsWithParams:"

-- | @Selector@ for @readAttributeScheduledContentRatingThresholdWithParams:@
readAttributeScheduledContentRatingThresholdWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeScheduledContentRatingThresholdWithParamsSelector = mkSelector "readAttributeScheduledContentRatingThresholdWithParams:"

-- | @Selector@ for @readAttributeScreenDailyTimeWithParams:@
readAttributeScreenDailyTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeScreenDailyTimeWithParamsSelector = mkSelector "readAttributeScreenDailyTimeWithParams:"

-- | @Selector@ for @readAttributeRemainingScreenTimeWithParams:@
readAttributeRemainingScreenTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRemainingScreenTimeWithParamsSelector = mkSelector "readAttributeRemainingScreenTimeWithParams:"

-- | @Selector@ for @readAttributeBlockUnratedWithParams:@
readAttributeBlockUnratedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBlockUnratedWithParamsSelector = mkSelector "readAttributeBlockUnratedWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterContentControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterContentControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterContentControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

