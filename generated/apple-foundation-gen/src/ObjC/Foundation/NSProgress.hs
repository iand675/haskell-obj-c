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
  , currentProgressSelector
  , progressWithTotalUnitCountSelector
  , discreteProgressWithTotalUnitCountSelector
  , progressWithTotalUnitCount_parent_pendingUnitCountSelector
  , initWithParent_userInfoSelector
  , becomeCurrentWithPendingUnitCountSelector
  , performAsCurrentWithPendingUnitCount_usingBlockSelector
  , resignCurrentSelector
  , addChild_withPendingUnitCountSelector
  , setUserInfoObject_forKeySelector
  , cancelSelector
  , pauseSelector
  , resumeSelector
  , publishSelector
  , unpublishSelector
  , addSubscriberForFileURL_withPublishingHandlerSelector
  , removeSubscriberSelector
  , totalUnitCountSelector
  , setTotalUnitCountSelector
  , completedUnitCountSelector
  , setCompletedUnitCountSelector
  , localizedDescriptionSelector
  , setLocalizedDescriptionSelector
  , localizedAdditionalDescriptionSelector
  , setLocalizedAdditionalDescriptionSelector
  , cancellableSelector
  , setCancellableSelector
  , pausableSelector
  , setPausableSelector
  , cancelledSelector
  , pausedSelector
  , cancellationHandlerSelector
  , setCancellationHandlerSelector
  , pausingHandlerSelector
  , setPausingHandlerSelector
  , resumingHandlerSelector
  , setResumingHandlerSelector
  , indeterminateSelector
  , fractionCompletedSelector
  , finishedSelector
  , userInfoSelector
  , kindSelector
  , setKindSelector
  , estimatedTimeRemainingSelector
  , setEstimatedTimeRemainingSelector
  , throughputSelector
  , setThroughputSelector
  , fileOperationKindSelector
  , setFileOperationKindSelector
  , fileURLSelector
  , setFileURLSelector
  , fileTotalCountSelector
  , setFileTotalCountSelector
  , fileCompletedCountSelector
  , setFileCompletedCountSelector
  , oldSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ currentProgress@
currentProgress :: IO (Id NSProgress)
currentProgress  =
  do
    cls' <- getRequiredClass "NSProgress"
    sendClassMsg cls' (mkSelector "currentProgress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ progressWithTotalUnitCount:@
progressWithTotalUnitCount :: CLong -> IO (Id NSProgress)
progressWithTotalUnitCount unitCount =
  do
    cls' <- getRequiredClass "NSProgress"
    sendClassMsg cls' (mkSelector "progressWithTotalUnitCount:") (retPtr retVoid) [argCLong unitCount] >>= retainedObject . castPtr

-- | @+ discreteProgressWithTotalUnitCount:@
discreteProgressWithTotalUnitCount :: CLong -> IO (Id NSProgress)
discreteProgressWithTotalUnitCount unitCount =
  do
    cls' <- getRequiredClass "NSProgress"
    sendClassMsg cls' (mkSelector "discreteProgressWithTotalUnitCount:") (retPtr retVoid) [argCLong unitCount] >>= retainedObject . castPtr

-- | @+ progressWithTotalUnitCount:parent:pendingUnitCount:@
progressWithTotalUnitCount_parent_pendingUnitCount :: IsNSProgress parent => CLong -> parent -> CLong -> IO (Id NSProgress)
progressWithTotalUnitCount_parent_pendingUnitCount unitCount parent portionOfParentTotalUnitCount =
  do
    cls' <- getRequiredClass "NSProgress"
    withObjCPtr parent $ \raw_parent ->
      sendClassMsg cls' (mkSelector "progressWithTotalUnitCount:parent:pendingUnitCount:") (retPtr retVoid) [argCLong unitCount, argPtr (castPtr raw_parent :: Ptr ()), argCLong portionOfParentTotalUnitCount] >>= retainedObject . castPtr

-- | @- initWithParent:userInfo:@
initWithParent_userInfo :: (IsNSProgress nsProgress, IsNSProgress parentProgressOrNil, IsNSDictionary userInfoOrNil) => nsProgress -> parentProgressOrNil -> userInfoOrNil -> IO (Id NSProgress)
initWithParent_userInfo nsProgress  parentProgressOrNil userInfoOrNil =
  withObjCPtr parentProgressOrNil $ \raw_parentProgressOrNil ->
    withObjCPtr userInfoOrNil $ \raw_userInfoOrNil ->
        sendMsg nsProgress (mkSelector "initWithParent:userInfo:") (retPtr retVoid) [argPtr (castPtr raw_parentProgressOrNil :: Ptr ()), argPtr (castPtr raw_userInfoOrNil :: Ptr ())] >>= ownedObject . castPtr

-- | @- becomeCurrentWithPendingUnitCount:@
becomeCurrentWithPendingUnitCount :: IsNSProgress nsProgress => nsProgress -> CLong -> IO ()
becomeCurrentWithPendingUnitCount nsProgress  unitCount =
    sendMsg nsProgress (mkSelector "becomeCurrentWithPendingUnitCount:") retVoid [argCLong unitCount]

-- | @- performAsCurrentWithPendingUnitCount:usingBlock:@
performAsCurrentWithPendingUnitCount_usingBlock :: IsNSProgress nsProgress => nsProgress -> CLong -> Ptr () -> IO ()
performAsCurrentWithPendingUnitCount_usingBlock nsProgress  unitCount work =
    sendMsg nsProgress (mkSelector "performAsCurrentWithPendingUnitCount:usingBlock:") retVoid [argCLong unitCount, argPtr (castPtr work :: Ptr ())]

-- | @- resignCurrent@
resignCurrent :: IsNSProgress nsProgress => nsProgress -> IO ()
resignCurrent nsProgress  =
    sendMsg nsProgress (mkSelector "resignCurrent") retVoid []

-- | @- addChild:withPendingUnitCount:@
addChild_withPendingUnitCount :: (IsNSProgress nsProgress, IsNSProgress child) => nsProgress -> child -> CLong -> IO ()
addChild_withPendingUnitCount nsProgress  child inUnitCount =
  withObjCPtr child $ \raw_child ->
      sendMsg nsProgress (mkSelector "addChild:withPendingUnitCount:") retVoid [argPtr (castPtr raw_child :: Ptr ()), argCLong inUnitCount]

-- | @- setUserInfoObject:forKey:@
setUserInfoObject_forKey :: (IsNSProgress nsProgress, IsNSString key) => nsProgress -> RawId -> key -> IO ()
setUserInfoObject_forKey nsProgress  objectOrNil key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsProgress (mkSelector "setUserInfoObject:forKey:") retVoid [argPtr (castPtr (unRawId objectOrNil) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- cancel@
cancel :: IsNSProgress nsProgress => nsProgress -> IO ()
cancel nsProgress  =
    sendMsg nsProgress (mkSelector "cancel") retVoid []

-- | @- pause@
pause :: IsNSProgress nsProgress => nsProgress -> IO ()
pause nsProgress  =
    sendMsg nsProgress (mkSelector "pause") retVoid []

-- | @- resume@
resume :: IsNSProgress nsProgress => nsProgress -> IO ()
resume nsProgress  =
    sendMsg nsProgress (mkSelector "resume") retVoid []

-- | @- publish@
publish :: IsNSProgress nsProgress => nsProgress -> IO ()
publish nsProgress  =
    sendMsg nsProgress (mkSelector "publish") retVoid []

-- | @- unpublish@
unpublish :: IsNSProgress nsProgress => nsProgress -> IO ()
unpublish nsProgress  =
    sendMsg nsProgress (mkSelector "unpublish") retVoid []

-- | @+ addSubscriberForFileURL:withPublishingHandler:@
addSubscriberForFileURL_withPublishingHandler :: IsNSURL url => url -> Ptr () -> IO RawId
addSubscriberForFileURL_withPublishingHandler url publishingHandler =
  do
    cls' <- getRequiredClass "NSProgress"
    withObjCPtr url $ \raw_url ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "addSubscriberForFileURL:withPublishingHandler:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr publishingHandler :: Ptr ())]

-- | @+ removeSubscriber:@
removeSubscriber :: RawId -> IO ()
removeSubscriber subscriber =
  do
    cls' <- getRequiredClass "NSProgress"
    sendClassMsg cls' (mkSelector "removeSubscriber:") retVoid [argPtr (castPtr (unRawId subscriber) :: Ptr ())]

-- | @- totalUnitCount@
totalUnitCount :: IsNSProgress nsProgress => nsProgress -> IO CLong
totalUnitCount nsProgress  =
    sendMsg nsProgress (mkSelector "totalUnitCount") retCLong []

-- | @- setTotalUnitCount:@
setTotalUnitCount :: IsNSProgress nsProgress => nsProgress -> CLong -> IO ()
setTotalUnitCount nsProgress  value =
    sendMsg nsProgress (mkSelector "setTotalUnitCount:") retVoid [argCLong value]

-- | @- completedUnitCount@
completedUnitCount :: IsNSProgress nsProgress => nsProgress -> IO CLong
completedUnitCount nsProgress  =
    sendMsg nsProgress (mkSelector "completedUnitCount") retCLong []

-- | @- setCompletedUnitCount:@
setCompletedUnitCount :: IsNSProgress nsProgress => nsProgress -> CLong -> IO ()
setCompletedUnitCount nsProgress  value =
    sendMsg nsProgress (mkSelector "setCompletedUnitCount:") retVoid [argCLong value]

-- | @- localizedDescription@
localizedDescription :: IsNSProgress nsProgress => nsProgress -> IO (Id NSString)
localizedDescription nsProgress  =
    sendMsg nsProgress (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalizedDescription:@
setLocalizedDescription :: (IsNSProgress nsProgress, IsNSString value) => nsProgress -> value -> IO ()
setLocalizedDescription nsProgress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsProgress (mkSelector "setLocalizedDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localizedAdditionalDescription@
localizedAdditionalDescription :: IsNSProgress nsProgress => nsProgress -> IO (Id NSString)
localizedAdditionalDescription nsProgress  =
    sendMsg nsProgress (mkSelector "localizedAdditionalDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalizedAdditionalDescription:@
setLocalizedAdditionalDescription :: (IsNSProgress nsProgress, IsNSString value) => nsProgress -> value -> IO ()
setLocalizedAdditionalDescription nsProgress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsProgress (mkSelector "setLocalizedAdditionalDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cancellable@
cancellable :: IsNSProgress nsProgress => nsProgress -> IO Bool
cancellable nsProgress  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgress (mkSelector "cancellable") retCULong []

-- | @- setCancellable:@
setCancellable :: IsNSProgress nsProgress => nsProgress -> Bool -> IO ()
setCancellable nsProgress  value =
    sendMsg nsProgress (mkSelector "setCancellable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- pausable@
pausable :: IsNSProgress nsProgress => nsProgress -> IO Bool
pausable nsProgress  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgress (mkSelector "pausable") retCULong []

-- | @- setPausable:@
setPausable :: IsNSProgress nsProgress => nsProgress -> Bool -> IO ()
setPausable nsProgress  value =
    sendMsg nsProgress (mkSelector "setPausable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- cancelled@
cancelled :: IsNSProgress nsProgress => nsProgress -> IO Bool
cancelled nsProgress  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgress (mkSelector "cancelled") retCULong []

-- | @- paused@
paused :: IsNSProgress nsProgress => nsProgress -> IO Bool
paused nsProgress  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgress (mkSelector "paused") retCULong []

-- | @- cancellationHandler@
cancellationHandler :: IsNSProgress nsProgress => nsProgress -> IO (Ptr ())
cancellationHandler nsProgress  =
    fmap castPtr $ sendMsg nsProgress (mkSelector "cancellationHandler") (retPtr retVoid) []

-- | @- setCancellationHandler:@
setCancellationHandler :: IsNSProgress nsProgress => nsProgress -> Ptr () -> IO ()
setCancellationHandler nsProgress  value =
    sendMsg nsProgress (mkSelector "setCancellationHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- pausingHandler@
pausingHandler :: IsNSProgress nsProgress => nsProgress -> IO (Ptr ())
pausingHandler nsProgress  =
    fmap castPtr $ sendMsg nsProgress (mkSelector "pausingHandler") (retPtr retVoid) []

-- | @- setPausingHandler:@
setPausingHandler :: IsNSProgress nsProgress => nsProgress -> Ptr () -> IO ()
setPausingHandler nsProgress  value =
    sendMsg nsProgress (mkSelector "setPausingHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- resumingHandler@
resumingHandler :: IsNSProgress nsProgress => nsProgress -> IO (Ptr ())
resumingHandler nsProgress  =
    fmap castPtr $ sendMsg nsProgress (mkSelector "resumingHandler") (retPtr retVoid) []

-- | @- setResumingHandler:@
setResumingHandler :: IsNSProgress nsProgress => nsProgress -> Ptr () -> IO ()
setResumingHandler nsProgress  value =
    sendMsg nsProgress (mkSelector "setResumingHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- indeterminate@
indeterminate :: IsNSProgress nsProgress => nsProgress -> IO Bool
indeterminate nsProgress  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgress (mkSelector "indeterminate") retCULong []

-- | @- fractionCompleted@
fractionCompleted :: IsNSProgress nsProgress => nsProgress -> IO CDouble
fractionCompleted nsProgress  =
    sendMsg nsProgress (mkSelector "fractionCompleted") retCDouble []

-- | @- finished@
finished :: IsNSProgress nsProgress => nsProgress -> IO Bool
finished nsProgress  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgress (mkSelector "finished") retCULong []

-- | @- userInfo@
userInfo :: IsNSProgress nsProgress => nsProgress -> IO (Id NSDictionary)
userInfo nsProgress  =
    sendMsg nsProgress (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- kind@
kind :: IsNSProgress nsProgress => nsProgress -> IO (Id NSString)
kind nsProgress  =
    sendMsg nsProgress (mkSelector "kind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKind:@
setKind :: (IsNSProgress nsProgress, IsNSString value) => nsProgress -> value -> IO ()
setKind nsProgress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsProgress (mkSelector "setKind:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- estimatedTimeRemaining@
estimatedTimeRemaining :: IsNSProgress nsProgress => nsProgress -> IO (Id NSNumber)
estimatedTimeRemaining nsProgress  =
    sendMsg nsProgress (mkSelector "estimatedTimeRemaining") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEstimatedTimeRemaining:@
setEstimatedTimeRemaining :: (IsNSProgress nsProgress, IsNSNumber value) => nsProgress -> value -> IO ()
setEstimatedTimeRemaining nsProgress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsProgress (mkSelector "setEstimatedTimeRemaining:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- throughput@
throughput :: IsNSProgress nsProgress => nsProgress -> IO (Id NSNumber)
throughput nsProgress  =
    sendMsg nsProgress (mkSelector "throughput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setThroughput:@
setThroughput :: (IsNSProgress nsProgress, IsNSNumber value) => nsProgress -> value -> IO ()
setThroughput nsProgress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsProgress (mkSelector "setThroughput:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fileOperationKind@
fileOperationKind :: IsNSProgress nsProgress => nsProgress -> IO (Id NSString)
fileOperationKind nsProgress  =
    sendMsg nsProgress (mkSelector "fileOperationKind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileOperationKind:@
setFileOperationKind :: (IsNSProgress nsProgress, IsNSString value) => nsProgress -> value -> IO ()
setFileOperationKind nsProgress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsProgress (mkSelector "setFileOperationKind:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fileURL@
fileURL :: IsNSProgress nsProgress => nsProgress -> IO (Id NSURL)
fileURL nsProgress  =
    sendMsg nsProgress (mkSelector "fileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileURL:@
setFileURL :: (IsNSProgress nsProgress, IsNSURL value) => nsProgress -> value -> IO ()
setFileURL nsProgress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsProgress (mkSelector "setFileURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fileTotalCount@
fileTotalCount :: IsNSProgress nsProgress => nsProgress -> IO (Id NSNumber)
fileTotalCount nsProgress  =
    sendMsg nsProgress (mkSelector "fileTotalCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileTotalCount:@
setFileTotalCount :: (IsNSProgress nsProgress, IsNSNumber value) => nsProgress -> value -> IO ()
setFileTotalCount nsProgress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsProgress (mkSelector "setFileTotalCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fileCompletedCount@
fileCompletedCount :: IsNSProgress nsProgress => nsProgress -> IO (Id NSNumber)
fileCompletedCount nsProgress  =
    sendMsg nsProgress (mkSelector "fileCompletedCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileCompletedCount:@
setFileCompletedCount :: (IsNSProgress nsProgress, IsNSNumber value) => nsProgress -> value -> IO ()
setFileCompletedCount nsProgress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsProgress (mkSelector "setFileCompletedCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- old@
old :: IsNSProgress nsProgress => nsProgress -> IO Bool
old nsProgress  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProgress (mkSelector "old") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentProgress@
currentProgressSelector :: Selector
currentProgressSelector = mkSelector "currentProgress"

-- | @Selector@ for @progressWithTotalUnitCount:@
progressWithTotalUnitCountSelector :: Selector
progressWithTotalUnitCountSelector = mkSelector "progressWithTotalUnitCount:"

-- | @Selector@ for @discreteProgressWithTotalUnitCount:@
discreteProgressWithTotalUnitCountSelector :: Selector
discreteProgressWithTotalUnitCountSelector = mkSelector "discreteProgressWithTotalUnitCount:"

-- | @Selector@ for @progressWithTotalUnitCount:parent:pendingUnitCount:@
progressWithTotalUnitCount_parent_pendingUnitCountSelector :: Selector
progressWithTotalUnitCount_parent_pendingUnitCountSelector = mkSelector "progressWithTotalUnitCount:parent:pendingUnitCount:"

-- | @Selector@ for @initWithParent:userInfo:@
initWithParent_userInfoSelector :: Selector
initWithParent_userInfoSelector = mkSelector "initWithParent:userInfo:"

-- | @Selector@ for @becomeCurrentWithPendingUnitCount:@
becomeCurrentWithPendingUnitCountSelector :: Selector
becomeCurrentWithPendingUnitCountSelector = mkSelector "becomeCurrentWithPendingUnitCount:"

-- | @Selector@ for @performAsCurrentWithPendingUnitCount:usingBlock:@
performAsCurrentWithPendingUnitCount_usingBlockSelector :: Selector
performAsCurrentWithPendingUnitCount_usingBlockSelector = mkSelector "performAsCurrentWithPendingUnitCount:usingBlock:"

-- | @Selector@ for @resignCurrent@
resignCurrentSelector :: Selector
resignCurrentSelector = mkSelector "resignCurrent"

-- | @Selector@ for @addChild:withPendingUnitCount:@
addChild_withPendingUnitCountSelector :: Selector
addChild_withPendingUnitCountSelector = mkSelector "addChild:withPendingUnitCount:"

-- | @Selector@ for @setUserInfoObject:forKey:@
setUserInfoObject_forKeySelector :: Selector
setUserInfoObject_forKeySelector = mkSelector "setUserInfoObject:forKey:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @publish@
publishSelector :: Selector
publishSelector = mkSelector "publish"

-- | @Selector@ for @unpublish@
unpublishSelector :: Selector
unpublishSelector = mkSelector "unpublish"

-- | @Selector@ for @addSubscriberForFileURL:withPublishingHandler:@
addSubscriberForFileURL_withPublishingHandlerSelector :: Selector
addSubscriberForFileURL_withPublishingHandlerSelector = mkSelector "addSubscriberForFileURL:withPublishingHandler:"

-- | @Selector@ for @removeSubscriber:@
removeSubscriberSelector :: Selector
removeSubscriberSelector = mkSelector "removeSubscriber:"

-- | @Selector@ for @totalUnitCount@
totalUnitCountSelector :: Selector
totalUnitCountSelector = mkSelector "totalUnitCount"

-- | @Selector@ for @setTotalUnitCount:@
setTotalUnitCountSelector :: Selector
setTotalUnitCountSelector = mkSelector "setTotalUnitCount:"

-- | @Selector@ for @completedUnitCount@
completedUnitCountSelector :: Selector
completedUnitCountSelector = mkSelector "completedUnitCount"

-- | @Selector@ for @setCompletedUnitCount:@
setCompletedUnitCountSelector :: Selector
setCompletedUnitCountSelector = mkSelector "setCompletedUnitCount:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @localizedAdditionalDescription@
localizedAdditionalDescriptionSelector :: Selector
localizedAdditionalDescriptionSelector = mkSelector "localizedAdditionalDescription"

-- | @Selector@ for @setLocalizedAdditionalDescription:@
setLocalizedAdditionalDescriptionSelector :: Selector
setLocalizedAdditionalDescriptionSelector = mkSelector "setLocalizedAdditionalDescription:"

-- | @Selector@ for @cancellable@
cancellableSelector :: Selector
cancellableSelector = mkSelector "cancellable"

-- | @Selector@ for @setCancellable:@
setCancellableSelector :: Selector
setCancellableSelector = mkSelector "setCancellable:"

-- | @Selector@ for @pausable@
pausableSelector :: Selector
pausableSelector = mkSelector "pausable"

-- | @Selector@ for @setPausable:@
setPausableSelector :: Selector
setPausableSelector = mkSelector "setPausable:"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @cancellationHandler@
cancellationHandlerSelector :: Selector
cancellationHandlerSelector = mkSelector "cancellationHandler"

-- | @Selector@ for @setCancellationHandler:@
setCancellationHandlerSelector :: Selector
setCancellationHandlerSelector = mkSelector "setCancellationHandler:"

-- | @Selector@ for @pausingHandler@
pausingHandlerSelector :: Selector
pausingHandlerSelector = mkSelector "pausingHandler"

-- | @Selector@ for @setPausingHandler:@
setPausingHandlerSelector :: Selector
setPausingHandlerSelector = mkSelector "setPausingHandler:"

-- | @Selector@ for @resumingHandler@
resumingHandlerSelector :: Selector
resumingHandlerSelector = mkSelector "resumingHandler"

-- | @Selector@ for @setResumingHandler:@
setResumingHandlerSelector :: Selector
setResumingHandlerSelector = mkSelector "setResumingHandler:"

-- | @Selector@ for @indeterminate@
indeterminateSelector :: Selector
indeterminateSelector = mkSelector "indeterminate"

-- | @Selector@ for @fractionCompleted@
fractionCompletedSelector :: Selector
fractionCompletedSelector = mkSelector "fractionCompleted"

-- | @Selector@ for @finished@
finishedSelector :: Selector
finishedSelector = mkSelector "finished"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @kind@
kindSelector :: Selector
kindSelector = mkSelector "kind"

-- | @Selector@ for @setKind:@
setKindSelector :: Selector
setKindSelector = mkSelector "setKind:"

-- | @Selector@ for @estimatedTimeRemaining@
estimatedTimeRemainingSelector :: Selector
estimatedTimeRemainingSelector = mkSelector "estimatedTimeRemaining"

-- | @Selector@ for @setEstimatedTimeRemaining:@
setEstimatedTimeRemainingSelector :: Selector
setEstimatedTimeRemainingSelector = mkSelector "setEstimatedTimeRemaining:"

-- | @Selector@ for @throughput@
throughputSelector :: Selector
throughputSelector = mkSelector "throughput"

-- | @Selector@ for @setThroughput:@
setThroughputSelector :: Selector
setThroughputSelector = mkSelector "setThroughput:"

-- | @Selector@ for @fileOperationKind@
fileOperationKindSelector :: Selector
fileOperationKindSelector = mkSelector "fileOperationKind"

-- | @Selector@ for @setFileOperationKind:@
setFileOperationKindSelector :: Selector
setFileOperationKindSelector = mkSelector "setFileOperationKind:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @setFileURL:@
setFileURLSelector :: Selector
setFileURLSelector = mkSelector "setFileURL:"

-- | @Selector@ for @fileTotalCount@
fileTotalCountSelector :: Selector
fileTotalCountSelector = mkSelector "fileTotalCount"

-- | @Selector@ for @setFileTotalCount:@
setFileTotalCountSelector :: Selector
setFileTotalCountSelector = mkSelector "setFileTotalCount:"

-- | @Selector@ for @fileCompletedCount@
fileCompletedCountSelector :: Selector
fileCompletedCountSelector = mkSelector "fileCompletedCount"

-- | @Selector@ for @setFileCompletedCount:@
setFileCompletedCountSelector :: Selector
setFileCompletedCountSelector = mkSelector "setFileCompletedCount:"

-- | @Selector@ for @old@
oldSelector :: Selector
oldSelector = mkSelector "old"

