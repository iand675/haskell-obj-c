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
  , saveSnapshotSelector
  , setStateFromExtendedGamepadSelector
  , controllerSelector
  , valueChangedHandlerSelector
  , setValueChangedHandlerSelector
  , dpadSelector
  , buttonASelector
  , buttonBSelector
  , buttonXSelector
  , buttonYSelector
  , buttonMenuSelector
  , buttonOptionsSelector
  , buttonHomeSelector
  , leftThumbstickSelector
  , rightThumbstickSelector
  , leftShoulderSelector
  , rightShoulderSelector
  , leftTriggerSelector
  , rightTriggerSelector
  , leftThumbstickButtonSelector
  , rightThumbstickButtonSelector


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

-- | Polls the state vector of the controller and saves it to a snapshot. The snapshot is stored in a device independent format that can be serialized and used at a later date. This is useful for features such as quality assurance, save game or replay functionality among many.
--
-- If your application is heavily multithreaded this may also be useful to guarantee atomicity of input handling as a snapshot will not change based on user input once it is taken.
--
-- ObjC selector: @- saveSnapshot@
saveSnapshot :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCExtendedGamepadSnapshot)
saveSnapshot gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "saveSnapshot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the state vector of the extended gamepad to a copy of the input extended gamepad's state vector.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: GCController.snapshot
--
-- ObjC selector: @- setStateFromExtendedGamepad:@
setStateFromExtendedGamepad :: (IsGCExtendedGamepad gcExtendedGamepad, IsGCExtendedGamepad extendedGamepad) => gcExtendedGamepad -> extendedGamepad -> IO ()
setStateFromExtendedGamepad gcExtendedGamepad  extendedGamepad =
withObjCPtr extendedGamepad $ \raw_extendedGamepad ->
    sendMsg gcExtendedGamepad (mkSelector "setStateFromExtendedGamepad:") retVoid [argPtr (castPtr raw_extendedGamepad :: Ptr ())]

-- | A profile keeps a reference to the controller that this profile is mapping input from.
--
-- ObjC selector: @- controller@
controller :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCController)
controller gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "controller") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Ptr ())
valueChangedHandler gcExtendedGamepad  =
  fmap castPtr $ sendMsg gcExtendedGamepad (mkSelector "valueChangedHandler") (retPtr retVoid) []

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> Ptr () -> IO ()
setValueChangedHandler gcExtendedGamepad  value =
  sendMsg gcExtendedGamepad (mkSelector "setValueChangedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Required to be analog in the Extended profile. All the elements of this directional input are thus analog.
--
-- ObjC selector: @- dpad@
dpad :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerDirectionPad)
dpad gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "dpad") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All face buttons are required to be analog in the Extended profile. These must be arranged in the diamond pattern given below:
--
-- Y  / \\ X   B  \\ /   A
--
-- ObjC selector: @- buttonA@
buttonA :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonA gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "buttonA") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- buttonB@
buttonB :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonB gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "buttonB") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- buttonX@
buttonX :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonX gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "buttonX") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- buttonY@
buttonY :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonY gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "buttonY") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Button menu is the primary menu button, and should be used to enter the main menu and pause the game.
--
-- ObjC selector: @- buttonMenu@
buttonMenu :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonMenu gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "buttonMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Button options is the secondary menu button. It should be used to enter a secondary menu, such as graphics and sound configuration, and pause the game.
--
-- ObjC selector: @- buttonOptions@
buttonOptions :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonOptions gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "buttonOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Button home is a special menu button. If the system does not consume button home events, they will be passed to your application and should be used to enter a secondary menu, and pause the game.
--
-- ObjC selector: @- buttonHome@
buttonHome :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
buttonHome gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "buttonHome") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A thumbstick is a 2-axis control that is physically required to be analog. All the elements of this directional input are thus analog.
--
-- ObjC selector: @- leftThumbstick@
leftThumbstick :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerDirectionPad)
leftThumbstick gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "leftThumbstick") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A thumbstick is a 2-axis control that is physically required to be analog. All the elements of this directional input are thus analog.
--
-- ObjC selector: @- rightThumbstick@
rightThumbstick :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerDirectionPad)
rightThumbstick gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "rightThumbstick") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Shoulder buttons are required to be analog inputs.
--
-- ObjC selector: @- leftShoulder@
leftShoulder :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
leftShoulder gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "leftShoulder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Shoulder buttons are required to be analog inputs.
--
-- ObjC selector: @- rightShoulder@
rightShoulder :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
rightShoulder gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "rightShoulder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Triggers are required to be analog inputs. Common uses would be acceleration and decelleration in a driving game for example.
--
-- ObjC selector: @- leftTrigger@
leftTrigger :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
leftTrigger gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "leftTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rightTrigger@
rightTrigger :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
rightTrigger gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "rightTrigger") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A thumbstick may also have a clickable component, which is treated as a non-analog button.
--
-- ObjC selector: @- leftThumbstickButton@
leftThumbstickButton :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
leftThumbstickButton gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "leftThumbstickButton") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rightThumbstickButton@
rightThumbstickButton :: IsGCExtendedGamepad gcExtendedGamepad => gcExtendedGamepad -> IO (Id GCControllerButtonInput)
rightThumbstickButton gcExtendedGamepad  =
  sendMsg gcExtendedGamepad (mkSelector "rightThumbstickButton") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @saveSnapshot@
saveSnapshotSelector :: Selector
saveSnapshotSelector = mkSelector "saveSnapshot"

-- | @Selector@ for @setStateFromExtendedGamepad:@
setStateFromExtendedGamepadSelector :: Selector
setStateFromExtendedGamepadSelector = mkSelector "setStateFromExtendedGamepad:"

-- | @Selector@ for @controller@
controllerSelector :: Selector
controllerSelector = mkSelector "controller"

-- | @Selector@ for @valueChangedHandler@
valueChangedHandlerSelector :: Selector
valueChangedHandlerSelector = mkSelector "valueChangedHandler"

-- | @Selector@ for @setValueChangedHandler:@
setValueChangedHandlerSelector :: Selector
setValueChangedHandlerSelector = mkSelector "setValueChangedHandler:"

-- | @Selector@ for @dpad@
dpadSelector :: Selector
dpadSelector = mkSelector "dpad"

-- | @Selector@ for @buttonA@
buttonASelector :: Selector
buttonASelector = mkSelector "buttonA"

-- | @Selector@ for @buttonB@
buttonBSelector :: Selector
buttonBSelector = mkSelector "buttonB"

-- | @Selector@ for @buttonX@
buttonXSelector :: Selector
buttonXSelector = mkSelector "buttonX"

-- | @Selector@ for @buttonY@
buttonYSelector :: Selector
buttonYSelector = mkSelector "buttonY"

-- | @Selector@ for @buttonMenu@
buttonMenuSelector :: Selector
buttonMenuSelector = mkSelector "buttonMenu"

-- | @Selector@ for @buttonOptions@
buttonOptionsSelector :: Selector
buttonOptionsSelector = mkSelector "buttonOptions"

-- | @Selector@ for @buttonHome@
buttonHomeSelector :: Selector
buttonHomeSelector = mkSelector "buttonHome"

-- | @Selector@ for @leftThumbstick@
leftThumbstickSelector :: Selector
leftThumbstickSelector = mkSelector "leftThumbstick"

-- | @Selector@ for @rightThumbstick@
rightThumbstickSelector :: Selector
rightThumbstickSelector = mkSelector "rightThumbstick"

-- | @Selector@ for @leftShoulder@
leftShoulderSelector :: Selector
leftShoulderSelector = mkSelector "leftShoulder"

-- | @Selector@ for @rightShoulder@
rightShoulderSelector :: Selector
rightShoulderSelector = mkSelector "rightShoulder"

-- | @Selector@ for @leftTrigger@
leftTriggerSelector :: Selector
leftTriggerSelector = mkSelector "leftTrigger"

-- | @Selector@ for @rightTrigger@
rightTriggerSelector :: Selector
rightTriggerSelector = mkSelector "rightTrigger"

-- | @Selector@ for @leftThumbstickButton@
leftThumbstickButtonSelector :: Selector
leftThumbstickButtonSelector = mkSelector "leftThumbstickButton"

-- | @Selector@ for @rightThumbstickButton@
rightThumbstickButtonSelector :: Selector
rightThumbstickButtonSelector = mkSelector "rightThumbstickButton"

