{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSProgress@.
module ObjC.Foundation.NSProgress
  ( NSProgress
  , IsNSProgress(..)
  , currentProgress
  , progressWithTotalUnitCount
  , discreteProgressWithTotalUnitCount
  , progressWithTotalUnitCount_parent_pendingUnitCount
  , initWithParent_userInfo
  , becomeCurrentWithPendingUnitCount
  , performAsCurrentWithPendingUnitCount_usingBlock
  , resignCurrent
  , addChild_withPendingUnitCount
  , setUserInfoObject_forKey
  , cancel
  , pause
  , resume
  , publish
  , unpublish
  , addSubscriberForFileURL_withPublishingHandler
  , removeSubscriber
  , totalUnitCount
  , setTotalUnitCount
  , completedUnitCount
  , setCompletedUnitCount
  , localizedDescription
  , setLocalizedDescription
  , localizedAdditionalDescription
  , setLocalizedAdditionalDescription
  , cancellable
  , setCancellable
  , pausable
  , setPausable
  , cancelled
  , paused
  , cancellationHandler
  , setCancellationHandler
  , pausingHandler
  , setPausingHandler
  , resumingHandler
  , setResumingHandler
  , indeterminate
  , fractionCompleted
  , finished
  , userInfo
  , kind
  , setKind
  , estimatedTimeRemaining
  , setEstimatedTimeRemaining
  , throughput
  , setThroughput
  , fileOperationKind
  , setFileOperationKind
  , fileURL
  , setFileURL
  , fileTotalCount
  , setFileTotalCount
  , fileCompletedCount
  , setFileCompletedCount
  , old
  , addChild_withPendingUnitCountSelector
  , addSubscriberForFileURL_withPublishingHandlerSelector
  , becomeCurrentWithPendingUnitCountSelector
  , cancelSelector
  , cancellableSelector
  , cancellationHandlerSelector
  , cancelledSelector
  , completedUnitCountSelector
  , currentProgressSelector
  , discreteProgressWithTotalUnitCountSelector
  , estimatedTimeRemainingSelector
  , fileCompletedCountSelector
  , fileOperationKindSelector
  , fileTotalCountSelector
  , fileURLSelector
  , finishedSelector
  , fractionCompletedSelector
  , indeterminateSelector
  , initWithParent_userInfoSelector
  , kindSelector
  , localizedAdditionalDescriptionSelector
  , localizedDescriptionSelector
  , oldSelector
  , pausableSelector
  , pauseSelector
  , pausedSelector
  , pausingHandlerSelector
  , performAsCurrentWithPendingUnitCount_usingBlockSelector
  , progressWithTotalUnitCountSelector
  , progressWithTotalUnitCount_parent_pendingUnitCountSelector
  , publishSelector
  , removeSubscriberSelector
  , resignCurrentSelector
  , resumeSelector
  , resumingHandlerSelector
  , setCancellableSelector
  , setCancellationHandlerSelector
  , setCompletedUnitCountSelector
  , setEstimatedTimeRemainingSelector
  , setFileCompletedCountSelector
  , setFileOperationKindSelector
  , setFileTotalCountSelector
  , setFileURLSelector
  , setKindSelector
  , setLocalizedAdditionalDescriptionSelector
  , setLocalizedDescriptionSelector
  , setPausableSelector
  , setPausingHandlerSelector
  , setResumingHandlerSelector
  , setThroughputSelector
  , setTotalUnitCountSelector
  , setUserInfoObject_forKeySelector
  , throughputSelector
  , totalUnitCountSelector
  , unpublishSelector
  , userInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ currentProgress@
currentProgress :: IO (Id NSProgress)
currentProgress  =
  do
    cls' <- getRequiredClass "NSProgress"
    sendClassMessage cls' currentProgressSelector

-- | @+ progressWithTotalUnitCount:@
progressWithTotalUnitCount :: CLong -> IO (Id NSProgress)
progressWithTotalUnitCount unitCount =
  do
    cls' <- getRequiredClass "NSProgress"
    sendClassMessage cls' progressWithTotalUnitCountSelector unitCount

-- | @+ discreteProgressWithTotalUnitCount:@
discreteProgressWithTotalUnitCount :: CLong -> IO (Id NSProgress)
discreteProgressWithTotalUnitCount unitCount =
  do
    cls' <- getRequiredClass "NSProgress"
    sendClassMessage cls' discreteProgressWithTotalUnitCountSelector unitCount

-- | @+ progressWithTotalUnitCount:parent:pendingUnitCount:@
progressWithTotalUnitCount_parent_pendingUnitCount :: IsNSProgress parent => CLong -> parent -> CLong -> IO (Id NSProgress)
progressWithTotalUnitCount_parent_pendingUnitCount unitCount parent portionOfParentTotalUnitCount =
  do
    cls' <- getRequiredClass "NSProgress"
    sendClassMessage cls' progressWithTotalUnitCount_parent_pendingUnitCountSelector unitCount (toNSProgress parent) portionOfParentTotalUnitCount

-- | @- initWithParent:userInfo:@
initWithParent_userInfo :: (IsNSProgress nsProgress, IsNSProgress parentProgressOrNil, IsNSDictionary userInfoOrNil) => nsProgress -> parentProgressOrNil -> userInfoOrNil -> IO (Id NSProgress)
initWithParent_userInfo nsProgress parentProgressOrNil userInfoOrNil =
  sendOwnedMessage nsProgress initWithParent_userInfoSelector (toNSProgress parentProgressOrNil) (toNSDictionary userInfoOrNil)

-- | @- becomeCurrentWithPendingUnitCount:@
becomeCurrentWithPendingUnitCount :: IsNSProgress nsProgress => nsProgress -> CLong -> IO ()
becomeCurrentWithPendingUnitCount nsProgress unitCount =
  sendMessage nsProgress becomeCurrentWithPendingUnitCountSelector unitCount

-- | @- performAsCurrentWithPendingUnitCount:usingBlock:@
performAsCurrentWithPendingUnitCount_usingBlock :: IsNSProgress nsProgress => nsProgress -> CLong -> Ptr () -> IO ()
performAsCurrentWithPendingUnitCount_usingBlock nsProgress unitCount work =
  sendMessage nsProgress performAsCurrentWithPendingUnitCount_usingBlockSelector unitCount work

-- | @- resignCurrent@
resignCurrent :: IsNSProgress nsProgress => nsProgress -> IO ()
resignCurrent nsProgress =
  sendMessage nsProgress resignCurrentSelector

-- | @- addChild:withPendingUnitCount:@
addChild_withPendingUnitCount :: (IsNSProgress nsProgress, IsNSProgress child) => nsProgress -> child -> CLong -> IO ()
addChild_withPendingUnitCount nsProgress child inUnitCount =
  sendMessage nsProgress addChild_withPendingUnitCountSelector (toNSProgress child) inUnitCount

-- | @- setUserInfoObject:forKey:@
setUserInfoObject_forKey :: (IsNSProgress nsProgress, IsNSString key) => nsProgress -> RawId -> key -> IO ()
setUserInfoObject_forKey nsProgress objectOrNil key =
  sendMessage nsProgress setUserInfoObject_forKeySelector objectOrNil (toNSString key)

-- | @- cancel@
cancel :: IsNSProgress nsProgress => nsProgress -> IO ()
cancel nsProgress =
  sendMessage nsProgress cancelSelector

-- | @- pause@
pause :: IsNSProgress nsProgress => nsProgress -> IO ()
pause nsProgress =
  sendMessage nsProgress pauseSelector

-- | @- resume@
resume :: IsNSProgress nsProgress => nsProgress -> IO ()
resume nsProgress =
  sendMessage nsProgress resumeSelector

-- | @- publish@
publish :: IsNSProgress nsProgress => nsProgress -> IO ()
publish nsProgress =
  sendMessage nsProgress publishSelector

-- | @- unpublish@
unpublish :: IsNSProgress nsProgress => nsProgress -> IO ()
unpublish nsProgress =
  sendMessage nsProgress unpublishSelector

-- | @+ addSubscriberForFileURL:withPublishingHandler:@
addSubscriberForFileURL_withPublishingHandler :: IsNSURL url => url -> Ptr () -> IO RawId
addSubscriberForFileURL_withPublishingHandler url publishingHandler =
  do
    cls' <- getRequiredClass "NSProgress"
    sendClassMessage cls' addSubscriberForFileURL_withPublishingHandlerSelector (toNSURL url) publishingHandler

-- | @+ removeSubscriber:@
removeSubscriber :: RawId -> IO ()
removeSubscriber subscriber =
  do
    cls' <- getRequiredClass "NSProgress"
    sendClassMessage cls' removeSubscriberSelector subscriber

-- | @- totalUnitCount@
totalUnitCount :: IsNSProgress nsProgress => nsProgress -> IO CLong
totalUnitCount nsProgress =
  sendMessage nsProgress totalUnitCountSelector

-- | @- setTotalUnitCount:@
setTotalUnitCount :: IsNSProgress nsProgress => nsProgress -> CLong -> IO ()
setTotalUnitCount nsProgress value =
  sendMessage nsProgress setTotalUnitCountSelector value

-- | @- completedUnitCount@
completedUnitCount :: IsNSProgress nsProgress => nsProgress -> IO CLong
completedUnitCount nsProgress =
  sendMessage nsProgress completedUnitCountSelector

-- | @- setCompletedUnitCount:@
setCompletedUnitCount :: IsNSProgress nsProgress => nsProgress -> CLong -> IO ()
setCompletedUnitCount nsProgress value =
  sendMessage nsProgress setCompletedUnitCountSelector value

-- | @- localizedDescription@
localizedDescription :: IsNSProgress nsProgress => nsProgress -> IO (Id NSString)
localizedDescription nsProgress =
  sendMessage nsProgress localizedDescriptionSelector

-- | @- setLocalizedDescription:@
setLocalizedDescription :: (IsNSProgress nsProgress, IsNSString value) => nsProgress -> value -> IO ()
setLocalizedDescription nsProgress value =
  sendMessage nsProgress setLocalizedDescriptionSelector (toNSString value)

-- | @- localizedAdditionalDescription@
localizedAdditionalDescription :: IsNSProgress nsProgress => nsProgress -> IO (Id NSString)
localizedAdditionalDescription nsProgress =
  sendMessage nsProgress localizedAdditionalDescriptionSelector

-- | @- setLocalizedAdditionalDescription:@
setLocalizedAdditionalDescription :: (IsNSProgress nsProgress, IsNSString value) => nsProgress -> value -> IO ()
setLocalizedAdditionalDescription nsProgress value =
  sendMessage nsProgress setLocalizedAdditionalDescriptionSelector (toNSString value)

-- | @- cancellable@
cancellable :: IsNSProgress nsProgress => nsProgress -> IO Bool
cancellable nsProgress =
  sendMessage nsProgress cancellableSelector

-- | @- setCancellable:@
setCancellable :: IsNSProgress nsProgress => nsProgress -> Bool -> IO ()
setCancellable nsProgress value =
  sendMessage nsProgress setCancellableSelector value

-- | @- pausable@
pausable :: IsNSProgress nsProgress => nsProgress -> IO Bool
pausable nsProgress =
  sendMessage nsProgress pausableSelector

-- | @- setPausable:@
setPausable :: IsNSProgress nsProgress => nsProgress -> Bool -> IO ()
setPausable nsProgress value =
  sendMessage nsProgress setPausableSelector value

-- | @- cancelled@
cancelled :: IsNSProgress nsProgress => nsProgress -> IO Bool
cancelled nsProgress =
  sendMessage nsProgress cancelledSelector

-- | @- paused@
paused :: IsNSProgress nsProgress => nsProgress -> IO Bool
paused nsProgress =
  sendMessage nsProgress pausedSelector

-- | @- cancellationHandler@
cancellationHandler :: IsNSProgress nsProgress => nsProgress -> IO (Ptr ())
cancellationHandler nsProgress =
  sendMessage nsProgress cancellationHandlerSelector

-- | @- setCancellationHandler:@
setCancellationHandler :: IsNSProgress nsProgress => nsProgress -> Ptr () -> IO ()
setCancellationHandler nsProgress value =
  sendMessage nsProgress setCancellationHandlerSelector value

-- | @- pausingHandler@
pausingHandler :: IsNSProgress nsProgress => nsProgress -> IO (Ptr ())
pausingHandler nsProgress =
  sendMessage nsProgress pausingHandlerSelector

-- | @- setPausingHandler:@
setPausingHandler :: IsNSProgress nsProgress => nsProgress -> Ptr () -> IO ()
setPausingHandler nsProgress value =
  sendMessage nsProgress setPausingHandlerSelector value

-- | @- resumingHandler@
resumingHandler :: IsNSProgress nsProgress => nsProgress -> IO (Ptr ())
resumingHandler nsProgress =
  sendMessage nsProgress resumingHandlerSelector

-- | @- setResumingHandler:@
setResumingHandler :: IsNSProgress nsProgress => nsProgress -> Ptr () -> IO ()
setResumingHandler nsProgress value =
  sendMessage nsProgress setResumingHandlerSelector value

-- | @- indeterminate@
indeterminate :: IsNSProgress nsProgress => nsProgress -> IO Bool
indeterminate nsProgress =
  sendMessage nsProgress indeterminateSelector

-- | @- fractionCompleted@
fractionCompleted :: IsNSProgress nsProgress => nsProgress -> IO CDouble
fractionCompleted nsProgress =
  sendMessage nsProgress fractionCompletedSelector

-- | @- finished@
finished :: IsNSProgress nsProgress => nsProgress -> IO Bool
finished nsProgress =
  sendMessage nsProgress finishedSelector

-- | @- userInfo@
userInfo :: IsNSProgress nsProgress => nsProgress -> IO (Id NSDictionary)
userInfo nsProgress =
  sendMessage nsProgress userInfoSelector

-- | @- kind@
kind :: IsNSProgress nsProgress => nsProgress -> IO (Id NSString)
kind nsProgress =
  sendMessage nsProgress kindSelector

-- | @- setKind:@
setKind :: (IsNSProgress nsProgress, IsNSString value) => nsProgress -> value -> IO ()
setKind nsProgress value =
  sendMessage nsProgress setKindSelector (toNSString value)

-- | @- estimatedTimeRemaining@
estimatedTimeRemaining :: IsNSProgress nsProgress => nsProgress -> IO (Id NSNumber)
estimatedTimeRemaining nsProgress =
  sendMessage nsProgress estimatedTimeRemainingSelector

-- | @- setEstimatedTimeRemaining:@
setEstimatedTimeRemaining :: (IsNSProgress nsProgress, IsNSNumber value) => nsProgress -> value -> IO ()
setEstimatedTimeRemaining nsProgress value =
  sendMessage nsProgress setEstimatedTimeRemainingSelector (toNSNumber value)

-- | @- throughput@
throughput :: IsNSProgress nsProgress => nsProgress -> IO (Id NSNumber)
throughput nsProgress =
  sendMessage nsProgress throughputSelector

-- | @- setThroughput:@
setThroughput :: (IsNSProgress nsProgress, IsNSNumber value) => nsProgress -> value -> IO ()
setThroughput nsProgress value =
  sendMessage nsProgress setThroughputSelector (toNSNumber value)

-- | @- fileOperationKind@
fileOperationKind :: IsNSProgress nsProgress => nsProgress -> IO (Id NSString)
fileOperationKind nsProgress =
  sendMessage nsProgress fileOperationKindSelector

-- | @- setFileOperationKind:@
setFileOperationKind :: (IsNSProgress nsProgress, IsNSString value) => nsProgress -> value -> IO ()
setFileOperationKind nsProgress value =
  sendMessage nsProgress setFileOperationKindSelector (toNSString value)

-- | @- fileURL@
fileURL :: IsNSProgress nsProgress => nsProgress -> IO (Id NSURL)
fileURL nsProgress =
  sendMessage nsProgress fileURLSelector

-- | @- setFileURL:@
setFileURL :: (IsNSProgress nsProgress, IsNSURL value) => nsProgress -> value -> IO ()
setFileURL nsProgress value =
  sendMessage nsProgress setFileURLSelector (toNSURL value)

-- | @- fileTotalCount@
fileTotalCount :: IsNSProgress nsProgress => nsProgress -> IO (Id NSNumber)
fileTotalCount nsProgress =
  sendMessage nsProgress fileTotalCountSelector

-- | @- setFileTotalCount:@
setFileTotalCount :: (IsNSProgress nsProgress, IsNSNumber value) => nsProgress -> value -> IO ()
setFileTotalCount nsProgress value =
  sendMessage nsProgress setFileTotalCountSelector (toNSNumber value)

-- | @- fileCompletedCount@
fileCompletedCount :: IsNSProgress nsProgress => nsProgress -> IO (Id NSNumber)
fileCompletedCount nsProgress =
  sendMessage nsProgress fileCompletedCountSelector

-- | @- setFileCompletedCount:@
setFileCompletedCount :: (IsNSProgress nsProgress, IsNSNumber value) => nsProgress -> value -> IO ()
setFileCompletedCount nsProgress value =
  sendMessage nsProgress setFileCompletedCountSelector (toNSNumber value)

-- | @- old@
old :: IsNSProgress nsProgress => nsProgress -> IO Bool
old nsProgress =
  sendMessage nsProgress oldSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentProgress@
currentProgressSelector :: Selector '[] (Id NSProgress)
currentProgressSelector = mkSelector "currentProgress"

-- | @Selector@ for @progressWithTotalUnitCount:@
progressWithTotalUnitCountSelector :: Selector '[CLong] (Id NSProgress)
progressWithTotalUnitCountSelector = mkSelector "progressWithTotalUnitCount:"

-- | @Selector@ for @discreteProgressWithTotalUnitCount:@
discreteProgressWithTotalUnitCountSelector :: Selector '[CLong] (Id NSProgress)
discreteProgressWithTotalUnitCountSelector = mkSelector "discreteProgressWithTotalUnitCount:"

-- | @Selector@ for @progressWithTotalUnitCount:parent:pendingUnitCount:@
progressWithTotalUnitCount_parent_pendingUnitCountSelector :: Selector '[CLong, Id NSProgress, CLong] (Id NSProgress)
progressWithTotalUnitCount_parent_pendingUnitCountSelector = mkSelector "progressWithTotalUnitCount:parent:pendingUnitCount:"

-- | @Selector@ for @initWithParent:userInfo:@
initWithParent_userInfoSelector :: Selector '[Id NSProgress, Id NSDictionary] (Id NSProgress)
initWithParent_userInfoSelector = mkSelector "initWithParent:userInfo:"

-- | @Selector@ for @becomeCurrentWithPendingUnitCount:@
becomeCurrentWithPendingUnitCountSelector :: Selector '[CLong] ()
becomeCurrentWithPendingUnitCountSelector = mkSelector "becomeCurrentWithPendingUnitCount:"

-- | @Selector@ for @performAsCurrentWithPendingUnitCount:usingBlock:@
performAsCurrentWithPendingUnitCount_usingBlockSelector :: Selector '[CLong, Ptr ()] ()
performAsCurrentWithPendingUnitCount_usingBlockSelector = mkSelector "performAsCurrentWithPendingUnitCount:usingBlock:"

-- | @Selector@ for @resignCurrent@
resignCurrentSelector :: Selector '[] ()
resignCurrentSelector = mkSelector "resignCurrent"

-- | @Selector@ for @addChild:withPendingUnitCount:@
addChild_withPendingUnitCountSelector :: Selector '[Id NSProgress, CLong] ()
addChild_withPendingUnitCountSelector = mkSelector "addChild:withPendingUnitCount:"

-- | @Selector@ for @setUserInfoObject:forKey:@
setUserInfoObject_forKeySelector :: Selector '[RawId, Id NSString] ()
setUserInfoObject_forKeySelector = mkSelector "setUserInfoObject:forKey:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] ()
pauseSelector = mkSelector "pause"

-- | @Selector@ for @resume@
resumeSelector :: Selector '[] ()
resumeSelector = mkSelector "resume"

-- | @Selector@ for @publish@
publishSelector :: Selector '[] ()
publishSelector = mkSelector "publish"

-- | @Selector@ for @unpublish@
unpublishSelector :: Selector '[] ()
unpublishSelector = mkSelector "unpublish"

-- | @Selector@ for @addSubscriberForFileURL:withPublishingHandler:@
addSubscriberForFileURL_withPublishingHandlerSelector :: Selector '[Id NSURL, Ptr ()] RawId
addSubscriberForFileURL_withPublishingHandlerSelector = mkSelector "addSubscriberForFileURL:withPublishingHandler:"

-- | @Selector@ for @removeSubscriber:@
removeSubscriberSelector :: Selector '[RawId] ()
removeSubscriberSelector = mkSelector "removeSubscriber:"

-- | @Selector@ for @totalUnitCount@
totalUnitCountSelector :: Selector '[] CLong
totalUnitCountSelector = mkSelector "totalUnitCount"

-- | @Selector@ for @setTotalUnitCount:@
setTotalUnitCountSelector :: Selector '[CLong] ()
setTotalUnitCountSelector = mkSelector "setTotalUnitCount:"

-- | @Selector@ for @completedUnitCount@
completedUnitCountSelector :: Selector '[] CLong
completedUnitCountSelector = mkSelector "completedUnitCount"

-- | @Selector@ for @setCompletedUnitCount:@
setCompletedUnitCountSelector :: Selector '[CLong] ()
setCompletedUnitCountSelector = mkSelector "setCompletedUnitCount:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector '[Id NSString] ()
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @localizedAdditionalDescription@
localizedAdditionalDescriptionSelector :: Selector '[] (Id NSString)
localizedAdditionalDescriptionSelector = mkSelector "localizedAdditionalDescription"

-- | @Selector@ for @setLocalizedAdditionalDescription:@
setLocalizedAdditionalDescriptionSelector :: Selector '[Id NSString] ()
setLocalizedAdditionalDescriptionSelector = mkSelector "setLocalizedAdditionalDescription:"

-- | @Selector@ for @cancellable@
cancellableSelector :: Selector '[] Bool
cancellableSelector = mkSelector "cancellable"

-- | @Selector@ for @setCancellable:@
setCancellableSelector :: Selector '[Bool] ()
setCancellableSelector = mkSelector "setCancellable:"

-- | @Selector@ for @pausable@
pausableSelector :: Selector '[] Bool
pausableSelector = mkSelector "pausable"

-- | @Selector@ for @setPausable:@
setPausableSelector :: Selector '[Bool] ()
setPausableSelector = mkSelector "setPausable:"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector '[] Bool
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @cancellationHandler@
cancellationHandlerSelector :: Selector '[] (Ptr ())
cancellationHandlerSelector = mkSelector "cancellationHandler"

-- | @Selector@ for @setCancellationHandler:@
setCancellationHandlerSelector :: Selector '[Ptr ()] ()
setCancellationHandlerSelector = mkSelector "setCancellationHandler:"

-- | @Selector@ for @pausingHandler@
pausingHandlerSelector :: Selector '[] (Ptr ())
pausingHandlerSelector = mkSelector "pausingHandler"

-- | @Selector@ for @setPausingHandler:@
setPausingHandlerSelector :: Selector '[Ptr ()] ()
setPausingHandlerSelector = mkSelector "setPausingHandler:"

-- | @Selector@ for @resumingHandler@
resumingHandlerSelector :: Selector '[] (Ptr ())
resumingHandlerSelector = mkSelector "resumingHandler"

-- | @Selector@ for @setResumingHandler:@
setResumingHandlerSelector :: Selector '[Ptr ()] ()
setResumingHandlerSelector = mkSelector "setResumingHandler:"

-- | @Selector@ for @indeterminate@
indeterminateSelector :: Selector '[] Bool
indeterminateSelector = mkSelector "indeterminate"

-- | @Selector@ for @fractionCompleted@
fractionCompletedSelector :: Selector '[] CDouble
fractionCompletedSelector = mkSelector "fractionCompleted"

-- | @Selector@ for @finished@
finishedSelector :: Selector '[] Bool
finishedSelector = mkSelector "finished"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @kind@
kindSelector :: Selector '[] (Id NSString)
kindSelector = mkSelector "kind"

-- | @Selector@ for @setKind:@
setKindSelector :: Selector '[Id NSString] ()
setKindSelector = mkSelector "setKind:"

-- | @Selector@ for @estimatedTimeRemaining@
estimatedTimeRemainingSelector :: Selector '[] (Id NSNumber)
estimatedTimeRemainingSelector = mkSelector "estimatedTimeRemaining"

-- | @Selector@ for @setEstimatedTimeRemaining:@
setEstimatedTimeRemainingSelector :: Selector '[Id NSNumber] ()
setEstimatedTimeRemainingSelector = mkSelector "setEstimatedTimeRemaining:"

-- | @Selector@ for @throughput@
throughputSelector :: Selector '[] (Id NSNumber)
throughputSelector = mkSelector "throughput"

-- | @Selector@ for @setThroughput:@
setThroughputSelector :: Selector '[Id NSNumber] ()
setThroughputSelector = mkSelector "setThroughput:"

-- | @Selector@ for @fileOperationKind@
fileOperationKindSelector :: Selector '[] (Id NSString)
fileOperationKindSelector = mkSelector "fileOperationKind"

-- | @Selector@ for @setFileOperationKind:@
setFileOperationKindSelector :: Selector '[Id NSString] ()
setFileOperationKindSelector = mkSelector "setFileOperationKind:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector '[] (Id NSURL)
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @setFileURL:@
setFileURLSelector :: Selector '[Id NSURL] ()
setFileURLSelector = mkSelector "setFileURL:"

-- | @Selector@ for @fileTotalCount@
fileTotalCountSelector :: Selector '[] (Id NSNumber)
fileTotalCountSelector = mkSelector "fileTotalCount"

-- | @Selector@ for @setFileTotalCount:@
setFileTotalCountSelector :: Selector '[Id NSNumber] ()
setFileTotalCountSelector = mkSelector "setFileTotalCount:"

-- | @Selector@ for @fileCompletedCount@
fileCompletedCountSelector :: Selector '[] (Id NSNumber)
fileCompletedCountSelector = mkSelector "fileCompletedCount"

-- | @Selector@ for @setFileCompletedCount:@
setFileCompletedCountSelector :: Selector '[Id NSNumber] ()
setFileCompletedCountSelector = mkSelector "setFileCompletedCount:"

-- | @Selector@ for @old@
oldSelector :: Selector '[] Bool
oldSelector = mkSelector "old"

