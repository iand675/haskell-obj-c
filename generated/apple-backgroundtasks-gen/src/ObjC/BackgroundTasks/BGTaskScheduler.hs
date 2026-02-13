{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class for scheduling task requests that launch your app in the background.
--
-- Background tasks give your app a way to run code while the app is suspended. To learn how to register, schedule, and run a background task, see <doc://com.apple.documentation/documentation/uikit/app_and_environment/scenes/preparing_your_ui_to_run_in_the_background/using_background_tasks_to_update_your_app>.
--
-- Generated bindings for @BGTaskScheduler@.
module ObjC.BackgroundTasks.BGTaskScheduler
  ( BGTaskScheduler
  , IsBGTaskScheduler(..)
  , init_
  , new
  , registerForTaskWithIdentifier_usingQueue_launchHandler
  , submitTaskRequest_error
  , cancelTaskRequestWithIdentifier
  , cancelAllTaskRequests
  , sharedScheduler
  , supportedResources
  , cancelAllTaskRequestsSelector
  , cancelTaskRequestWithIdentifierSelector
  , initSelector
  , newSelector
  , registerForTaskWithIdentifier_usingQueue_launchHandlerSelector
  , sharedSchedulerSelector
  , submitTaskRequest_errorSelector
  , supportedResourcesSelector

  -- * Enum types
  , BGContinuedProcessingTaskRequestResources(BGContinuedProcessingTaskRequestResources)
  , pattern BGContinuedProcessingTaskRequestResourcesDefault
  , pattern BGContinuedProcessingTaskRequestResourcesGPU

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

-- | @- init@
init_ :: IsBGTaskScheduler bgTaskScheduler => bgTaskScheduler -> IO (Id BGTaskScheduler)
init_ bgTaskScheduler =
  sendOwnedMessage bgTaskScheduler initSelector

-- | @+ new@
new :: IO (Id BGTaskScheduler)
new  =
  do
    cls' <- getRequiredClass "BGTaskScheduler"
    sendOwnedClassMessage cls' newSelector

-- | Register a launch handler for the task with the associated identifier that’s executed on the specified queue.
--
-- Every identifier in the <doc://com.apple.documentation/documentation/bundleresources/information_property_list/bgtaskschedulerpermittedidentifiers> requires a handler. Registration of all launch handlers must be complete before the end of <doc://com.apple.documentation/documentation/uikit/uiapplicationdelegate/1623053-applicationdidfinishlaunching>.
--
-- You must register launch handlers before your application finishes launching (``BGContinuedProcessingTask`` registrations are exempt from this requirement). Attempting to register a handler after launch or multiple handlers for the same identifier is an error. Although you may submit task requests from some extensions, only the host app will be launched to handle background work.
--
-- - Parameters:    - identifier: The identifier for the task that will be handled by the provided launch handler.    - queue: A queue for executing the task. Pass @nil@ to use a default background queue.    - launchHandler: The system runs the block of code for the launch handler when it launches the app in the background. The block takes a single parameter, a ``BGTask`` object used for assigning an expiration handler and for setting a completion status. The block has no return value. Assign an expiration handler to the task's expirationHandler property and call setTaskCompletedWithSuccess: when the background work is complete. - Returns: Returns <doc://com.apple.documentation/documentation/objectivec/yes> if the launch handler was registered. Returns <doc://com.apple.documentation/documentation/objectivec/no> if the identifier isn't included in the <doc://com.apple.documentation/documentation/bundleresources/information_property_list/bgtaskschedulerpermittedidentifiers> @Info.plist@. - Important: Register each task identifier only once. The system kills the app on the second registration of the same task identifier.
--
-- ObjC selector: @- registerForTaskWithIdentifier:usingQueue:launchHandler:@
registerForTaskWithIdentifier_usingQueue_launchHandler :: (IsBGTaskScheduler bgTaskScheduler, IsNSString identifier, IsNSObject queue) => bgTaskScheduler -> identifier -> queue -> Ptr () -> IO Bool
registerForTaskWithIdentifier_usingQueue_launchHandler bgTaskScheduler identifier queue launchHandler =
  sendMessage bgTaskScheduler registerForTaskWithIdentifier_usingQueue_launchHandlerSelector (toNSString identifier) (toNSObject queue) launchHandler

-- | Submit a previously registered background task for execution.
--
-- Submitting a task request for an unexecuted task that’s already in the queue replaces the previous task request.
--
-- There can be a total of 1 refresh task and 10 processing tasks scheduled at any time. Trying to schedule more tasks returns ``BGTaskSchedulerErrorCode/BGTaskSchedulerErrorCodeTooManyPendingTaskRequests``.
--
-- - Parameters:   - taskRequest: The task request object representing the parameters of the background task to be scheduled.   - error: If an error occurs, upon return contains an error object that indicates why the request was rejected - Returns: @YES@ if the request was successfully submitted; @NO@ if there was an error
--
-- ObjC selector: @- submitTaskRequest:error:@
submitTaskRequest_error :: (IsBGTaskScheduler bgTaskScheduler, IsBGTaskRequest taskRequest, IsNSError error_) => bgTaskScheduler -> taskRequest -> error_ -> IO Bool
submitTaskRequest_error bgTaskScheduler taskRequest error_ =
  sendMessage bgTaskScheduler submitTaskRequest_errorSelector (toBGTaskRequest taskRequest) (toNSError error_)

-- | Cancel a previously scheduled task request.
--
-- - Parameters:   - identifier: The identifier of the previously submitted task request to cancel.
--
-- ObjC selector: @- cancelTaskRequestWithIdentifier:@
cancelTaskRequestWithIdentifier :: (IsBGTaskScheduler bgTaskScheduler, IsNSString identifier) => bgTaskScheduler -> identifier -> IO ()
cancelTaskRequestWithIdentifier bgTaskScheduler identifier =
  sendMessage bgTaskScheduler cancelTaskRequestWithIdentifierSelector (toNSString identifier)

-- | Cancel all previously submitted task requests.
--
-- ObjC selector: @- cancelAllTaskRequests@
cancelAllTaskRequests :: IsBGTaskScheduler bgTaskScheduler => bgTaskScheduler -> IO ()
cancelAllTaskRequests bgTaskScheduler =
  sendMessage bgTaskScheduler cancelAllTaskRequestsSelector

-- | The shared background task scheduler instance.
--
-- ObjC selector: @+ sharedScheduler@
sharedScheduler :: IO (Id BGTaskScheduler)
sharedScheduler  =
  do
    cls' <- getRequiredClass "BGTaskScheduler"
    sendClassMessage cls' sharedSchedulerSelector

-- | A bitfield of the resources the device supports for ``BackgroundTasks/BGContinuedProcessingTaskRequest`` instances.
--
-- ObjC selector: @+ supportedResources@
supportedResources :: IO BGContinuedProcessingTaskRequestResources
supportedResources  =
  do
    cls' <- getRequiredClass "BGTaskScheduler"
    sendClassMessage cls' supportedResourcesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BGTaskScheduler)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BGTaskScheduler)
newSelector = mkSelector "new"

-- | @Selector@ for @registerForTaskWithIdentifier:usingQueue:launchHandler:@
registerForTaskWithIdentifier_usingQueue_launchHandlerSelector :: Selector '[Id NSString, Id NSObject, Ptr ()] Bool
registerForTaskWithIdentifier_usingQueue_launchHandlerSelector = mkSelector "registerForTaskWithIdentifier:usingQueue:launchHandler:"

-- | @Selector@ for @submitTaskRequest:error:@
submitTaskRequest_errorSelector :: Selector '[Id BGTaskRequest, Id NSError] Bool
submitTaskRequest_errorSelector = mkSelector "submitTaskRequest:error:"

-- | @Selector@ for @cancelTaskRequestWithIdentifier:@
cancelTaskRequestWithIdentifierSelector :: Selector '[Id NSString] ()
cancelTaskRequestWithIdentifierSelector = mkSelector "cancelTaskRequestWithIdentifier:"

-- | @Selector@ for @cancelAllTaskRequests@
cancelAllTaskRequestsSelector :: Selector '[] ()
cancelAllTaskRequestsSelector = mkSelector "cancelAllTaskRequests"

-- | @Selector@ for @sharedScheduler@
sharedSchedulerSelector :: Selector '[] (Id BGTaskScheduler)
sharedSchedulerSelector = mkSelector "sharedScheduler"

-- | @Selector@ for @supportedResources@
supportedResourcesSelector :: Selector '[] BGContinuedProcessingTaskRequestResources
supportedResourcesSelector = mkSelector "supportedResources"

