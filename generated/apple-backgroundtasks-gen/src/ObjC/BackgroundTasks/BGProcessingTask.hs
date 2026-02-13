{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A time-consuming processing task that runs while the app is in the background.
--
-- Use processing tasks for long data updates, processing data, and app maintenance. Although processing tasks can run for minutes, the system can interrupt the process. Add an expiration handler by setting ``BGTask/expirationHandler`` for any required cleanup.
--
-- Executing processing tasks requires setting the @processing@ <doc://com.apple.documentation/documentation/bundleresources/information_property_list/uibackgroundmodes> capability. For information on setting this capability, see ``BGTaskScheduler``.
--
-- Processing tasks run only when the device is idle. The system terminates any background processing tasks running when the user starts using the device. Background refresh tasks are not affected.
--
-- Generated bindings for @BGProcessingTask@.
module ObjC.BackgroundTasks.BGProcessingTask
  ( BGProcessingTask
  , IsBGProcessingTask(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

