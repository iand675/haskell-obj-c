{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCGamepad@.
module ObjC.GameController.GCGamepad
  ( GCGamepad
  , IsGCGamepad(..)
  , saveSnapshot
  , controller
  , valueChangedHandler
  , setValueChangedHandler
  , dpad
  , buttonA
  , buttonB
  , buttonX
  , buttonY
  , leftShoulder
  , rightShoulder
  , buttonASelector
  , buttonBSelector
  , buttonXSelector
  , buttonYSelector
  , controllerSelector
  , dpadSelector
  , leftShoulderSelector
  , rightShoulderSelector
  , saveSnapshotSelector
  , setValueChangedHandlerSelector
  , valueChangedHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Polls the state vector of the controller and saves it to a snapshot. The snapshot is stored in a device independent format that can be serialized and used at a later date. This is useful for features such as quality assurance, save game or replay functionality among many.
--
-- If your application is heavily multithreaded this may also be useful to guarantee atomicity of input handling as a snapshot will not change based on user input once it is taken.
--
-- ObjC selector: @- saveSnapshot@
saveSnapshot :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCGamepadSnapshot)
saveSnapshot gcGamepad =
  sendMessage gcGamepad saveSnapshotSelector

-- | A profile keeps a reference to the controller that this profile is mapping input from.
--
-- ObjC selector: @- controller@
controller :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCController)
controller gcGamepad =
  sendMessage gcGamepad controllerSelector

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCGamepad gcGamepad => gcGamepad -> IO (Ptr ())
valueChangedHandler gcGamepad =
  sendMessage gcGamepad valueChangedHandlerSelector

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCGamepad gcGamepad => gcGamepad -> Ptr () -> IO ()
setValueChangedHandler gcGamepad value =
  sendMessage gcGamepad setValueChangedHandlerSelector value

-- | Required to be analog in the Standard profile. All the elements of this directional input are thus analog.
--
-- ObjC selector: @- dpad@
dpad :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerDirectionPad)
dpad gcGamepad =
  sendMessage gcGamepad dpadSelector

-- | All face buttons are required to be analog in the Standard profile. These must be arranged in the diamond pattern given below:
--
-- Y  / \\ X   B  \\ /   A
--
-- ObjC selector: @- buttonA@
buttonA :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
buttonA gcGamepad =
  sendMessage gcGamepad buttonASelector

-- | @- buttonB@
buttonB :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
buttonB gcGamepad =
  sendMessage gcGamepad buttonBSelector

-- | @- buttonX@
buttonX :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
buttonX gcGamepad =
  sendMessage gcGamepad buttonXSelector

-- | @- buttonY@
buttonY :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
buttonY gcGamepad =
  sendMessage gcGamepad buttonYSelector

-- | Shoulder buttons are required to be analog inputs.
--
-- ObjC selector: @- leftShoulder@
leftShoulder :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
leftShoulder gcGamepad =
  sendMessage gcGamepad leftShoulderSelector

-- | Shoulder buttons are required to be analog inputs.
--
-- ObjC selector: @- rightShoulder@
rightShoulder :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
rightShoulder gcGamepad =
  sendMessage gcGamepad rightShoulderSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @saveSnapshot@
saveSnapshotSelector :: Selector '[] (Id GCGamepadSnapshot)
saveSnapshotSelector = mkSelector "saveSnapshot"

-- | @Selector@ for @controller@
controllerSelector :: Selector '[] (Id GCController)
controllerSelector = mkSelector "controller"

-- | @Selector@ for @valueChangedHandler@
valueChangedHandlerSelector :: Selector '[] (Ptr ())
valueChangedHandlerSelector = mkSelector "valueChangedHandler"

-- | @Selector@ for @setValueChangedHandler:@
setValueChangedHandlerSelector :: Selector '[Ptr ()] ()
setValueChangedHandlerSelector = mkSelector "setValueChangedHandler:"

-- | @Selector@ for @dpad@
dpadSelector :: Selector '[] (Id GCControllerDirectionPad)
dpadSelector = mkSelector "dpad"

-- | @Selector@ for @buttonA@
buttonASelector :: Selector '[] (Id GCControllerButtonInput)
buttonASelector = mkSelector "buttonA"

-- | @Selector@ for @buttonB@
buttonBSelector :: Selector '[] (Id GCControllerButtonInput)
buttonBSelector = mkSelector "buttonB"

-- | @Selector@ for @buttonX@
buttonXSelector :: Selector '[] (Id GCControllerButtonInput)
buttonXSelector = mkSelector "buttonX"

-- | @Selector@ for @buttonY@
buttonYSelector :: Selector '[] (Id GCControllerButtonInput)
buttonYSelector = mkSelector "buttonY"

-- | @Selector@ for @leftShoulder@
leftShoulderSelector :: Selector '[] (Id GCControllerButtonInput)
leftShoulderSelector = mkSelector "leftShoulder"

-- | @Selector@ for @rightShoulder@
rightShoulderSelector :: Selector '[] (Id GCControllerButtonInput)
rightShoulderSelector = mkSelector "rightShoulder"

