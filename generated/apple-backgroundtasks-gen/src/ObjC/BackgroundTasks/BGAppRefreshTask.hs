{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object representing a short task typically used to refresh content that’s run while the app is in the background.
--
-- Use app refresh tasks for updating your app with small bits of information, such as the latest stock values.
--
-- Executing app refresh tasks requires setting the @fetch@ <doc://com.apple.documentation/documentation/bundleresources/information_property_list/uibackgroundmodes> capability. For information on setting this capability, see ``BGTaskScheduler``.
--
-- Generated bindings for @BGAppRefreshTask@.
module ObjC.BackgroundTasks.BGAppRefreshTask
  ( BGAppRefreshTask
  , IsBGAppRefreshTask(..)


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

