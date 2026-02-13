{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Mice are available to an application that links to GameController.framework. There are 2 ways to access mice paired to the system. Adopt both to ensure the best user experience:
--
-- 1: Querying for the current array of mice using [GCMouse mice] 2: Registering for Connection/Disconnection notifications from NSNotificationCenter.
--
-- Generated bindings for @GCMouse@.
module ObjC.GameController.GCMouse
  ( GCMouse
  , IsGCMouse(..)
  , mice
  , mouseInput
  , current
  , currentSelector
  , miceSelector
  , mouseInputSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ mice@
mice :: IO (Id NSArray)
mice  =
  do
    cls' <- getRequiredClass "GCMouse"
    sendClassMessage cls' miceSelector

-- | Unlike GCController GCMouse supports only one input profile Profile contains mouse buttons, scroll wheel and  pointer delta.
--
-- ObjC selector: @- mouseInput@
mouseInput :: IsGCMouse gcMouse => gcMouse -> IO (Id GCMouseInput)
mouseInput gcMouse =
  sendMessage gcMouse mouseInputSelector

-- | The most recently used mouse device. If a user actuates a mouse input, that mouse will become the current one.
--
-- See: GCMouseDidBecomeCurrentNotification
--
-- See: GCMouseDidStopBeingCurrentNotification
--
-- ObjC selector: @+ current@
current :: IO (Id GCMouse)
current  =
  do
    cls' <- getRequiredClass "GCMouse"
    sendClassMessage cls' currentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mice@
miceSelector :: Selector '[] (Id NSArray)
miceSelector = mkSelector "mice"

-- | @Selector@ for @mouseInput@
mouseInputSelector :: Selector '[] (Id GCMouseInput)
mouseInputSelector = mkSelector "mouseInput"

-- | @Selector@ for @current@
currentSelector :: Selector '[] (Id GCMouse)
currentSelector = mkSelector "current"

