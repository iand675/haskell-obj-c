{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract class representing a task that’s run while the app is in the background.
--
-- Generated bindings for @BGTask@.
module ObjC.BackgroundTasks.BGTask
  ( BGTask
  , IsBGTask(..)
  , init_
  , new
  , setTaskCompletedWithSuccess
  , identifier
  , expirationHandler
  , setExpirationHandler
  , expirationHandlerSelector
  , identifierSelector
  , initSelector
  , newSelector
  , setExpirationHandlerSelector
  , setTaskCompletedWithSuccessSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBGTask bgTask => bgTask -> IO (Id BGTask)
init_ bgTask =
  sendOwnedMessage bgTask initSelector

-- | @+ new@
new :: IO (Id BGTask)
new  =
  do
    cls' <- getRequiredClass "BGTask"
    sendOwnedClassMessage cls' newSelector

-- | Inform the background task scheduler that the task is complete.
--
-- Call this method as soon as the background work associated with this task is complete. The system provides your app with a limited amount of time to finish the task. If you do not call setTaskCompletedWithSuccess: on the task, the system continues to run in the background until all the available time is consumed, wasting battery power. The system suspends the app as soon as all background tasks are complete.
--
-- - Parameters:     - success: A @Boolean@ indicating if the task completed successfully or not. If the task was unsuccessful, you     may request the system to try again later by submitting a new task request to the scheduler before calling this     method.
--
-- - Important: If you don’t set an expiration handler, the system will mark your task as complete and unsuccessful instead of sending a warning. - Warning: Not calling ``BGTask/setTaskCompletedWithSuccess:`` before the time for the task expires may result in the system killing your app.
--
-- ObjC selector: @- setTaskCompletedWithSuccess:@
setTaskCompletedWithSuccess :: IsBGTask bgTask => bgTask -> Bool -> IO ()
setTaskCompletedWithSuccess bgTask success =
  sendMessage bgTask setTaskCompletedWithSuccessSelector success

-- | The string identifier of the task.
--
-- The identifier is the same as the one used to register the launch handler in ``BGTaskScheduler/registerForTaskWithIdentifier:usingQueue:launchHandler:``.
--
-- ObjC selector: @- identifier@
identifier :: IsBGTask bgTask => bgTask -> IO (Id NSString)
identifier bgTask =
  sendMessage bgTask identifierSelector

-- | A handler called shortly before the task’s background time expires.
--
-- There is a limit to how long your app has to perform its background work, and your work may need to be interrupted if system conditions change. Assign a handler to this property to cancel any ongoing tasks, perform any needed cleanup, and then call setTaskCompletedWithSuccess: to signal completion to the system and allow your app to be suspended. This property is cleared after it is called by the system or when ``BGTask/setTaskCompletedWithSuccess:`` is called. This is to mitigate the impact of a retain cycle created by referencing the BGTask instance inside this block.
--
-- The handler may be called before the background process uses the full amount of its allocated time.
--
-- - Parameters:     - expirationHandler: The expiration handler takes no arguments and has no return value. Use the handler to     cancel any ongoing work and to do any required cleanup in as short a time as possible.
--
-- - Note: The manager sets the value @expirationHandler@ to @nil@ after the handler completes. - Warning: Not setting an expiration handler results in the system marking your task as complete and unsuccessful instead of sending a warning.
--
-- ObjC selector: @- expirationHandler@
expirationHandler :: IsBGTask bgTask => bgTask -> IO (Ptr ())
expirationHandler bgTask =
  sendMessage bgTask expirationHandlerSelector

-- | A handler called shortly before the task’s background time expires.
--
-- There is a limit to how long your app has to perform its background work, and your work may need to be interrupted if system conditions change. Assign a handler to this property to cancel any ongoing tasks, perform any needed cleanup, and then call setTaskCompletedWithSuccess: to signal completion to the system and allow your app to be suspended. This property is cleared after it is called by the system or when ``BGTask/setTaskCompletedWithSuccess:`` is called. This is to mitigate the impact of a retain cycle created by referencing the BGTask instance inside this block.
--
-- The handler may be called before the background process uses the full amount of its allocated time.
--
-- - Parameters:     - expirationHandler: The expiration handler takes no arguments and has no return value. Use the handler to     cancel any ongoing work and to do any required cleanup in as short a time as possible.
--
-- - Note: The manager sets the value @expirationHandler@ to @nil@ after the handler completes. - Warning: Not setting an expiration handler results in the system marking your task as complete and unsuccessful instead of sending a warning.
--
-- ObjC selector: @- setExpirationHandler:@
setExpirationHandler :: IsBGTask bgTask => bgTask -> Ptr () -> IO ()
setExpirationHandler bgTask value =
  sendMessage bgTask setExpirationHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BGTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BGTask)
newSelector = mkSelector "new"

-- | @Selector@ for @setTaskCompletedWithSuccess:@
setTaskCompletedWithSuccessSelector :: Selector '[Bool] ()
setTaskCompletedWithSuccessSelector = mkSelector "setTaskCompletedWithSuccess:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @expirationHandler@
expirationHandlerSelector :: Selector '[] (Ptr ())
expirationHandlerSelector = mkSelector "expirationHandler"

-- | @Selector@ for @setExpirationHandler:@
setExpirationHandlerSelector :: Selector '[Ptr ()] ()
setExpirationHandlerSelector = mkSelector "setExpirationHandler:"

