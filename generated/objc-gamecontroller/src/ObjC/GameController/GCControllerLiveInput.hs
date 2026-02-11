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

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- capture@
capture :: IsGCControllerLiveInput gcControllerLiveInput => gcControllerLiveInput -> IO (Id GCControllerInputState)
capture gcControllerLiveInput  =
  sendMsg gcControllerLiveInput (mkSelector "capture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextInputState@
nextInputState :: IsGCControllerLiveInput gcControllerLiveInput => gcControllerLiveInput -> IO (Id GCControllerInputState)
nextInputState gcControllerLiveInput  =
  sendMsg gcControllerLiveInput (mkSelector "nextInputState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get a view of the controller's input without any system-level control  remapping applied.
--
-- Developers should avoid implementing their own control remapping  functionality and to instead direct users to the system game controller  settings to remap controls.  If you choose to implement your own  control remapping functionality, or if your app streams controller input  to a remote device that implements control remapping functionality, you  should access controller physical input through this interface.
--
-- ObjC selector: @- unmappedInput@
unmappedInput :: IsGCControllerLiveInput gcControllerLiveInput => gcControllerLiveInput -> IO (Id GCControllerLiveInput)
unmappedInput gcControllerLiveInput  =
  sendMsg gcControllerLiveInput (mkSelector "unmappedInput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @capture@
captureSelector :: Selector
captureSelector = mkSelector "capture"

-- | @Selector@ for @nextInputState@
nextInputStateSelector :: Selector
nextInputStateSelector = mkSelector "nextInputState"

-- | @Selector@ for @unmappedInput@
unmappedInputSelector :: Selector
unmappedInputSelector = mkSelector "unmappedInput"

