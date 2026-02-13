{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCExtendedGamepad@.
module ObjC.GameController.GCExtendedGamepad
  ( GCExtendedGamepad
  , IsGCExtendedGamepad(..)
  , saveSnapshot
  , setStateFromExtendedGamepad
  , controller
  , valueChangedHandler
  , setValueChangedHandler
  , dpad
  , buttonA
  , buttonB
  , buttonX
  , buttonY
  , buttonMenu
  , buttonOptions
  , buttonHome
  , leftThumbstick
  , rightThumbstick
  , leftShoulder
  , rightShoulder
  , leftTrigger
  , rightTrigger
  , leftThumbstickButton
  , rightThumbstickButton
  , buttonASelector
  , buttonBSelector
  , buttonHomeSelector
  , buttonMenuSelector
  , buttonOptionsSelector
  , buttonXSelector
  , buttonYSelector
  , controllerSelector
  , dpadSelector
  , leftShoulderSelector
  , leftThumbstickButtonSelector
  , leftThumbstickSelector
  , leftTriggerSelector
  , rightShoulderSelector
  , rightThumbstickButtonSelector
  , rightThumbstickSelector
  , rightTriggerSelector
  , saveSnapshotSelector
  , setStateFromExtendedGamepadSelector
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
saveSnapshot :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCExtendedGamepadSnapshot)
saveSnapshot gcExtendedGamepad =
  sendMessage gcExtendedGamepad saveSnapshotSelector

-- | Sets the state vector of the extended gamepad to a copy of the input extended gamepad's state vector.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: GCController.snapshot
--
-- ObjC selector: @- setStateFromExtendedGamepad:@
setStateFromExtendedGamepad :: (IsGCExtendedGamepad gcExtendedGamepad, IsGCExtendedGamepad extendedGamepad) => gcExtendedGamepad -> extendedGamepad -> IO ()
setStateFromExtendedGamepad gcExtendedGamepad extendedGamepad =
  sendMessage gcExtendedGamepad setStateFromExtendedGamepadSelector (toGCExtendedGamepad extendedGamepad)

-- | A profile keeps a reference to the controller that this profile is mapping input from.
--
-- ObjC selector: @- controller@
controller :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCController)
controller gcExtendedGamepad =
  sendMessage gcExtendedGamepad controllerSelector

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Ptr ())
valueChangedHandler gcExtendedGamepad =
  sendMessage gcExtendedGamepad valueChangedHandlerSelector

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> Ptr () -> IO ()
setValueChangedHandler gcExtendedGamepad value =
  sendMessage gcExtendedGamepad setValueChangedHandlerSelector value

-- | Required to be analog in the Extended profile. All the elements of this directional input are thus analog.
--
-- ObjC selector: @- dpad@
dpad :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerDirectionPad)
dpad gcExtendedGamepad =
  sendMessage gcExtendedGamepad dpadSelector

-- | All face buttons are required to be analog in the Extended profile. These must be arranged in the diamond pattern given below:
--
-- Y  / \\ X   B  \\ /   A
--
-- ObjC selector: @- buttonA@
buttonA :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonA gcExtendedGamepad =
  sendMessage gcExtendedGamepad buttonASelector

-- | @- buttonB@
buttonB :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonB gcExtendedGamepad =
  sendMessage gcExtendedGamepad buttonBSelector

-- | @- buttonX@
buttonX :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonX gcExtendedGamepad =
  sendMessage gcExtendedGamepad buttonXSelector

-- | @- buttonY@
buttonY :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonY gcExtendedGamepad =
  sendMessage gcExtendedGamepad buttonYSelector

-- | Button menu is the primary menu button, and should be used to enter the main menu and pause the game.
--
-- ObjC selector: @- buttonMenu@
buttonMenu :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonMenu gcExtendedGamepad =
  sendMessage gcExtendedGamepad buttonMenuSelector

-- | Button options is the secondary menu button. It should be used to enter a secondary menu, such as graphics and sound configuration, and pause the game.
--
-- ObjC selector: @- buttonOptions@
buttonOptions :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonOptions gcExtendedGamepad =
  sendMessage gcExtendedGamepad buttonOptionsSelector

-- | Button home is a special menu button. If the system does not consume button home events, they will be passed to your application and should be used to enter a secondary menu, and pause the game.
--
-- ObjC selector: @- buttonHome@
buttonHome :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonHome gcExtendedGamepad =
  sendMessage gcExtendedGamepad buttonHomeSelector

-- | A thumbstick is a 2-axis control that is physically required to be analog. All the elements of this directional input are thus analog.
--
-- ObjC selector: @- leftThumbstick@
leftThumbstick :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerDirectionPad)
leftThumbstick gcExtendedGamepad =
  sendMessage gcExtendedGamepad leftThumbstickSelector

-- | A thumbstick is a 2-axis control that is physically required to be analog. All the elements of this directional input are thus analog.
--
-- ObjC selector: @- rightThumbstick@
rightThumbstick :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerDirectionPad)
rightThumbstick gcExtendedGamepad =
  sendMessage gcExtendedGamepad rightThumbstickSelector

-- | Shoulder buttons are required to be analog inputs.
--
-- ObjC selector: @- leftShoulder@
leftShoulder :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
leftShoulder gcExtendedGamepad =
  sendMessage gcExtendedGamepad leftShoulderSelector

-- | Shoulder buttons are required to be analog inputs.
--
-- ObjC selector: @- rightShoulder@
rightShoulder :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
rightShoulder gcExtendedGamepad =
  sendMessage gcExtendedGamepad rightShoulderSelector

-- | Triggers are required to be analog inputs. Common uses would be acceleration and decelleration in a driving game for example.
--
-- ObjC selector: @- leftTrigger@
leftTrigger :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
leftTrigger gcExtendedGamepad =
  sendMessage gcExtendedGamepad leftTriggerSelector

-- | @- rightTrigger@
rightTrigger :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
rightTrigger gcExtendedGamepad =
  sendMessage gcExtendedGamepad rightTriggerSelector

-- | A thumbstick may also have a clickable component, which is treated as a non-analog button.
--
-- ObjC selector: @- leftThumbstickButton@
leftThumbstickButton :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
leftThumbstickButton gcExtendedGamepad =
  sendMessage gcExtendedGamepad leftThumbstickButtonSelector

-- | @- rightThumbstickButton@
rightThumbstickButton :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
rightThumbstickButton gcExtendedGamepad =
  sendMessage gcExtendedGamepad rightThumbstickButtonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @saveSnapshot@
saveSnapshotSelector :: Selector '[] (Id GCExtendedGamepadSnapshot)
saveSnapshotSelector = mkSelector "saveSnapshot"

-- | @Selector@ for @setStateFromExtendedGamepad:@
setStateFromExtendedGamepadSelector :: Selector '[Id GCExtendedGamepad] ()
setStateFromExtendedGamepadSelector = mkSelector "setStateFromExtendedGamepad:"

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

-- | @Selector@ for @buttonMenu@
buttonMenuSelector :: Selector '[] (Id GCControllerButtonInput)
buttonMenuSelector = mkSelector "buttonMenu"

-- | @Selector@ for @buttonOptions@
buttonOptionsSelector :: Selector '[] (Id GCControllerButtonInput)
buttonOptionsSelector = mkSelector "buttonOptions"

-- | @Selector@ for @buttonHome@
buttonHomeSelector :: Selector '[] (Id GCControllerButtonInput)
buttonHomeSelector = mkSelector "buttonHome"

-- | @Selector@ for @leftThumbstick@
leftThumbstickSelector :: Selector '[] (Id GCControllerDirectionPad)
leftThumbstickSelector = mkSelector "leftThumbstick"

-- | @Selector@ for @rightThumbstick@
rightThumbstickSelector :: Selector '[] (Id GCControllerDirectionPad)
rightThumbstickSelector = mkSelector "rightThumbstick"

-- | @Selector@ for @leftShoulder@
leftShoulderSelector :: Selector '[] (Id GCControllerButtonInput)
leftShoulderSelector = mkSelector "leftShoulder"

-- | @Selector@ for @rightShoulder@
rightShoulderSelector :: Selector '[] (Id GCControllerButtonInput)
rightShoulderSelector = mkSelector "rightShoulder"

-- | @Selector@ for @leftTrigger@
leftTriggerSelector :: Selector '[] (Id GCControllerButtonInput)
leftTriggerSelector = mkSelector "leftTrigger"

-- | @Selector@ for @rightTrigger@
rightTriggerSelector :: Selector '[] (Id GCControllerButtonInput)
rightTriggerSelector = mkSelector "rightTrigger"

-- | @Selector@ for @leftThumbstickButton@
leftThumbstickButtonSelector :: Selector '[] (Id GCControllerButtonInput)
leftThumbstickButtonSelector = mkSelector "leftThumbstickButton"

-- | @Selector@ for @rightThumbstickButton@
rightThumbstickButtonSelector :: Selector '[] (Id GCControllerButtonInput)
rightThumbstickButtonSelector = mkSelector "rightThumbstickButton"

