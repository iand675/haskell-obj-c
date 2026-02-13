{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to begin a workload immediately, or shortly after submission, which is allowed to continue running even if the app is backgrounded.
--
-- Generated bindings for @BGContinuedProcessingTaskRequest@.
module ObjC.BackgroundTasks.BGContinuedProcessingTaskRequest
  ( BGContinuedProcessingTaskRequest
  , IsBGContinuedProcessingTaskRequest(..)
  , initWithIdentifier_title_subtitle
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , strategy
  , setStrategy
  , requiredResources
  , setRequiredResources
  , initWithIdentifier_title_subtitleSelector
  , requiredResourcesSelector
  , setRequiredResourcesSelector
  , setStrategySelector
  , setSubtitleSelector
  , setTitleSelector
  , strategySelector
  , subtitleSelector
  , titleSelector

  -- * Enum types
  , BGContinuedProcessingTaskRequestResources(BGContinuedProcessingTaskRequestResources)
  , pattern BGContinuedProcessingTaskRequestResourcesDefault
  , pattern BGContinuedProcessingTaskRequestResourcesGPU
  , BGContinuedProcessingTaskRequestSubmissionStrategy(BGContinuedProcessingTaskRequestSubmissionStrategy)
  , pattern BGContinuedProcessingTaskRequestSubmissionStrategyFail
  , pattern BGContinuedProcessingTaskRequestSubmissionStrategyQueue

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.BackgroundTasks.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates an instance on behalf of the currently foregrounded app.
--
-- Apps and their extensions should use this method to initialize any tasks due to the underlying association to the currently foregrounded app. Please note that ``BGTaskRequest/earliestBeginDate`` will be outright ignored by the scheduler in favor of @NSDate.now@.
--
-- The identifier ought to use wildcard notation, where the prefix of the identifier must at least contain the bundle ID of the submitting application, followed by optional semantic context, and finally ending with @.*@. An example: `<MainBundle>.<SemanticContext>.*@ which would transform to @com.foo.MyApplication.continuedProcessingTask.*`. Thus, a submitted identifier would be of the form @com.foo.MyApplication.continuedProcessingTask.HD830D@.
--
-- - Parameters:   - identifier: The task identifier.   - title: The localized title displayed to the user before the task begins running.   - subtitle: The localized subtitle displayed to the user before the task begins running. - Warning: Successful creation of this object does not guarantee successful submission to the scheduler.
--
-- ObjC selector: @- initWithIdentifier:title:subtitle:@
initWithIdentifier_title_subtitle :: (IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest, IsNSString identifier, IsNSString title, IsNSString subtitle) => bgContinuedProcessingTaskRequest -> identifier -> title -> subtitle -> IO (Id BGContinuedProcessingTaskRequest)
initWithIdentifier_title_subtitle bgContinuedProcessingTaskRequest identifier title subtitle =
  sendOwnedMessage bgContinuedProcessingTaskRequest initWithIdentifier_title_subtitleSelector (toNSString identifier) (toNSString title) (toNSString subtitle)

-- | The localized title displayed to the user.
--
-- ObjC selector: @- title@
title :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> IO (Id NSString)
title bgContinuedProcessingTaskRequest =
  sendMessage bgContinuedProcessingTaskRequest titleSelector

-- | The localized title displayed to the user.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest, IsNSString value) => bgContinuedProcessingTaskRequest -> value -> IO ()
setTitle bgContinuedProcessingTaskRequest value =
  sendMessage bgContinuedProcessingTaskRequest setTitleSelector (toNSString value)

-- | The localized subtitle displayed to the user.
--
-- ObjC selector: @- subtitle@
subtitle :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> IO (Id NSString)
subtitle bgContinuedProcessingTaskRequest =
  sendMessage bgContinuedProcessingTaskRequest subtitleSelector

-- | The localized subtitle displayed to the user.
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest, IsNSString value) => bgContinuedProcessingTaskRequest -> value -> IO ()
setSubtitle bgContinuedProcessingTaskRequest value =
  sendMessage bgContinuedProcessingTaskRequest setSubtitleSelector (toNSString value)

-- | The submission strategy for the scheduler to abide by.
--
-- Defaults to ``BGContinuedProcessingTaskRequestSubmissionStrategy/BGContinuedProcessingTaskRequestSubmissionStrategyQueue``.
--
-- ObjC selector: @- strategy@
strategy :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> IO BGContinuedProcessingTaskRequestSubmissionStrategy
strategy bgContinuedProcessingTaskRequest =
  sendMessage bgContinuedProcessingTaskRequest strategySelector

-- | The submission strategy for the scheduler to abide by.
--
-- Defaults to ``BGContinuedProcessingTaskRequestSubmissionStrategy/BGContinuedProcessingTaskRequestSubmissionStrategyQueue``.
--
-- ObjC selector: @- setStrategy:@
setStrategy :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> BGContinuedProcessingTaskRequestSubmissionStrategy -> IO ()
setStrategy bgContinuedProcessingTaskRequest value =
  sendMessage bgContinuedProcessingTaskRequest setStrategySelector value

-- | Inform the scheduler that the task will be requesting additional system resources.
--
-- Defaults to ``BGContinuedProcessingTaskRequestResources/BGContinuedProcessingTaskRequestResourcesDefault``.
--
-- ObjC selector: @- requiredResources@
requiredResources :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> IO BGContinuedProcessingTaskRequestResources
requiredResources bgContinuedProcessingTaskRequest =
  sendMessage bgContinuedProcessingTaskRequest requiredResourcesSelector

-- | Inform the scheduler that the task will be requesting additional system resources.
--
-- Defaults to ``BGContinuedProcessingTaskRequestResources/BGContinuedProcessingTaskRequestResourcesDefault``.
--
-- ObjC selector: @- setRequiredResources:@
setRequiredResources :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> BGContinuedProcessingTaskRequestResources -> IO ()
setRequiredResources bgContinuedProcessingTaskRequest value =
  sendMessage bgContinuedProcessingTaskRequest setRequiredResourcesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:title:subtitle:@
initWithIdentifier_title_subtitleSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id BGContinuedProcessingTaskRequest)
initWithIdentifier_title_subtitleSelector = mkSelector "initWithIdentifier:title:subtitle:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @strategy@
strategySelector :: Selector '[] BGContinuedProcessingTaskRequestSubmissionStrategy
strategySelector = mkSelector "strategy"

-- | @Selector@ for @setStrategy:@
setStrategySelector :: Selector '[BGContinuedProcessingTaskRequestSubmissionStrategy] ()
setStrategySelector = mkSelector "setStrategy:"

-- | @Selector@ for @requiredResources@
requiredResourcesSelector :: Selector '[] BGContinuedProcessingTaskRequestResources
requiredResourcesSelector = mkSelector "requiredResources"

-- | @Selector@ for @setRequiredResources:@
setRequiredResourcesSelector :: Selector '[BGContinuedProcessingTaskRequestResources] ()
setRequiredResourcesSelector = mkSelector "setRequiredResources:"

