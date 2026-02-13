{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCControllerLiveInput@.
module ObjC.GameController.GCControllerLiveInput
  ( GCControllerLiveInput
  , IsGCControllerLiveInput(..)
  , capture
  , nextInputState
  , unmappedInput
  , captureSelector
  , nextInputStateSelector
  , unmappedInputSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- capture@
capture :: IsGCControllerLiveInput gcControllerLiveInput => gcControllerLiveInput -> IO (Id GCControllerInputState)
capture gcControllerLiveInput =
  sendMessage gcControllerLiveInput captureSelector

-- | @- nextInputState@
nextInputState :: IsGCControllerLiveInput gcControllerLiveInput => gcControllerLiveInput -> IO (Id GCControllerInputState)
nextInputState gcControllerLiveInput =
  sendMessage gcControllerLiveInput nextInputStateSelector

-- | Get a view of the controller's input without any system-level control  remapping applied.
--
-- Developers should avoid implementing their own control remapping  functionality and to instead direct users to the system game controller  settings to remap controls.  If you choose to implement your own  control remapping functionality, or if your app streams controller input  to a remote device that implements control remapping functionality, you  should access controller physical input through this interface.
--
-- ObjC selector: @- unmappedInput@
unmappedInput :: IsGCControllerLiveInput gcControllerLiveInput => gcControllerLiveInput -> IO (Id GCControllerLiveInput)
unmappedInput gcControllerLiveInput =
  sendMessage gcControllerLiveInput unmappedInputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @capture@
captureSelector :: Selector '[] (Id GCControllerInputState)
captureSelector = mkSelector "capture"

-- | @Selector@ for @nextInputState@
nextInputStateSelector :: Selector '[] (Id GCControllerInputState)
nextInputStateSelector = mkSelector "nextInputState"

-- | @Selector@ for @unmappedInput@
unmappedInputSelector :: Selector '[] (Id GCControllerLiveInput)
unmappedInputSelector = mkSelector "unmappedInput"

