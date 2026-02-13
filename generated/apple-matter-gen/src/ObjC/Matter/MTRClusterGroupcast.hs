{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Groupcast    The Groupcast cluster manages the content of the node-wide multicast Group membership that is part of the underlying interaction layer.
--
-- Generated bindings for @MTRClusterGroupcast@.
module ObjC.Matter.MTRClusterGroupcast
  ( MTRClusterGroupcast
  , IsMTRClusterGroupcast(..)
  , joinGroupWithParams_expectedValues_expectedValueInterval_completion
  , leaveGroupWithParams_expectedValues_expectedValueInterval_completion
  , updateGroupKeyWithParams_expectedValues_expectedValueInterval_completion
  , expireGracePeriodWithParams_expectedValues_expectedValueInterval_completion
  , configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMembershipWithParams
  , readAttributeMaxMembershipCountWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completionSelector
  , expireGracePeriodWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , joinGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , leaveGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMaxMembershipCountWithParamsSelector
  , readAttributeMembershipWithParamsSelector
  , updateGroupKeyWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- joinGroupWithParams:expectedValues:expectedValueInterval:completion:@
joinGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRGroupcastClusterJoinGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupcast -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
joinGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupcast params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGroupcast joinGroupWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGroupcastClusterJoinGroupParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- leaveGroupWithParams:expectedValues:expectedValueInterval:completion:@
leaveGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRGroupcastClusterLeaveGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupcast -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
leaveGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupcast params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGroupcast leaveGroupWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGroupcastClusterLeaveGroupParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- updateGroupKeyWithParams:expectedValues:expectedValueInterval:completion:@
updateGroupKeyWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRGroupcastClusterUpdateGroupKeyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupcast -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateGroupKeyWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupcast params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGroupcast updateGroupKeyWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGroupcastClusterUpdateGroupKeyParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- expireGracePeriodWithParams:expectedValues:expectedValueInterval:completion:@
expireGracePeriodWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRGroupcastClusterExpireGracePeriodParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupcast -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
expireGracePeriodWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupcast params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGroupcast expireGracePeriodWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGroupcastClusterExpireGracePeriodParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- configureAuxiliaryACLWithParams:expectedValues:expectedValueInterval:completion:@
configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRGroupcastClusterConfigureAuxiliaryACLParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupcast -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupcast params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGroupcast configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGroupcastClusterConfigureAuxiliaryACLParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeMembershipWithParams:@
readAttributeMembershipWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeMembershipWithParams mtrClusterGroupcast params =
  sendMessage mtrClusterGroupcast readAttributeMembershipWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxMembershipCountWithParams:@
readAttributeMaxMembershipCountWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeMaxMembershipCountWithParams mtrClusterGroupcast params =
  sendMessage mtrClusterGroupcast readAttributeMaxMembershipCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterGroupcast params =
  sendMessage mtrClusterGroupcast readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterGroupcast params =
  sendMessage mtrClusterGroupcast readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterGroupcast params =
  sendMessage mtrClusterGroupcast readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterGroupcast params =
  sendMessage mtrClusterGroupcast readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRReadParams params) => mtrClusterGroupcast -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterGroupcast params =
  sendMessage mtrClusterGroupcast readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterGroupcast mtrClusterGroupcast => mtrClusterGroupcast -> IO (Id MTRClusterGroupcast)
init_ mtrClusterGroupcast =
  sendOwnedMessage mtrClusterGroupcast initSelector

-- | @+ new@
new :: IO (Id MTRClusterGroupcast)
new  =
  do
    cls' <- getRequiredClass "MTRClusterGroupcast"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterGroupcast mtrClusterGroupcast, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterGroupcast -> device -> endpointID -> queue -> IO (Id MTRClusterGroupcast)
initWithDevice_endpointID_queue mtrClusterGroupcast device endpointID queue =
  sendOwnedMessage mtrClusterGroupcast initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @joinGroupWithParams:expectedValues:expectedValueInterval:completion:@
joinGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGroupcastClusterJoinGroupParams, Id NSArray, Id NSNumber, Ptr ()] ()
joinGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "joinGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @leaveGroupWithParams:expectedValues:expectedValueInterval:completion:@
leaveGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGroupcastClusterLeaveGroupParams, Id NSArray, Id NSNumber, Ptr ()] ()
leaveGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "leaveGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateGroupKeyWithParams:expectedValues:expectedValueInterval:completion:@
updateGroupKeyWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGroupcastClusterUpdateGroupKeyParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateGroupKeyWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateGroupKeyWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @expireGracePeriodWithParams:expectedValues:expectedValueInterval:completion:@
expireGracePeriodWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGroupcastClusterExpireGracePeriodParams, Id NSArray, Id NSNumber, Ptr ()] ()
expireGracePeriodWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "expireGracePeriodWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @configureAuxiliaryACLWithParams:expectedValues:expectedValueInterval:completion:@
configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGroupcastClusterConfigureAuxiliaryACLParams, Id NSArray, Id NSNumber, Ptr ()] ()
configureAuxiliaryACLWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "configureAuxiliaryACLWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMembershipWithParams:@
readAttributeMembershipWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMembershipWithParamsSelector = mkSelector "readAttributeMembershipWithParams:"

-- | @Selector@ for @readAttributeMaxMembershipCountWithParams:@
readAttributeMaxMembershipCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxMembershipCountWithParamsSelector = mkSelector "readAttributeMaxMembershipCountWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterGroupcast)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterGroupcast)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterGroupcast)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

