{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCMicroGamepad@.
module ObjC.GameController.GCMicroGamepad
  ( GCMicroGamepad
  , IsGCMicroGamepad(..)
  , saveSnapshot
  , setStateFromMicroGamepad
  , controller
  , valueChangedHandler
  , setValueChangedHandler
  , dpad
  , buttonA
  , buttonX
  , buttonMenu
  , reportsAbsoluteDpadValues
  , setReportsAbsoluteDpadValues
  , allowsRotation
  , setAllowsRotation
  , allowsRotationSelector
  , buttonASelector
  , buttonMenuSelector
  , buttonXSelector
  , controllerSelector
  , dpadSelector
  , reportsAbsoluteDpadValuesSelector
  , saveSnapshotSelector
  , setAllowsRotationSelector
  , setReportsAbsoluteDpadValuesSelector
  , setStateFromMicroGamepadSelector
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
-- See: GCMicroGamepadSnapshot
--
-- ObjC selector: @- saveSnapshot@
saveSnapshot :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> IO (Id GCMicroGamepadSnapshot)
saveSnapshot gcMicroGamepad =
  sendMessage gcMicroGamepad saveSnapshotSelector

-- | Sets the state vector of the micro gamepad to a copy of the input micro gamepad's state vector.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: GCController.snapshot
--
-- ObjC selector: @- setStateFromMicroGamepad:@
setStateFromMicroGamepad :: (IsGCMicroGamepad gcMicroGamepad, IsGCMicroGamepad microGamepad) => gcMicroGamepad -> microGamepad -> IO ()
setStateFromMicroGamepad gcMicroGamepad microGamepad =
  sendMessage gcMicroGamepad setStateFromMicroGamepadSelector (toGCMicroGamepad microGamepad)

-- | A profile keeps a reference to the controller that this profile is mapping input from.
--
-- ObjC selector: @- controller@
controller :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> IO (Id GCController)
controller gcMicroGamepad =
  sendMessage gcMicroGamepad controllerSelector

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> IO (Ptr ())
valueChangedHandler gcMicroGamepad =
  sendMessage gcMicroGamepad valueChangedHandlerSelector

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> Ptr () -> IO ()
setValueChangedHandler gcMicroGamepad value =
  sendMessage gcMicroGamepad setValueChangedHandlerSelector value

-- | Optionally analog in the Micro profile. All the elements of this directional input are either analog or digital.
--
-- ObjC selector: @- dpad@
dpad :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> IO (Id GCControllerDirectionPad)
dpad gcMicroGamepad =
  sendMessage gcMicroGamepad dpadSelector

-- | The Micro profile has two buttons that are optionally analog in the Micro profile. Button A is the primary action button, it indicates affirmative action and should be used to advance in menus or perform the primary action in gameplay.
--
-- ObjC selector: @- buttonA@
buttonA :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> IO (Id GCControllerButtonInput)
buttonA gcMicroGamepad =
  sendMessage gcMicroGamepad buttonASelector

-- | Button X is the secondary action button, it indicates an alternate affirmative action and should be used to perform a secondary action. If there is no secondary action it should be used as equivalent to buttonA.
--
-- Unlike on other profiles there is no negative button on this profile. Instead the menu button should be used to present menu content or to retreat in a menu flow.
--
-- See: buttonA
--
-- ObjC selector: @- buttonX@
buttonX :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> IO (Id GCControllerButtonInput)
buttonX gcMicroGamepad =
  sendMessage gcMicroGamepad buttonXSelector

-- | Button menu is the primary menu button, and should be used to enter the main menu and pause the game.
--
-- ObjC selector: @- buttonMenu@
buttonMenu :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> IO (Id GCControllerButtonInput)
buttonMenu gcMicroGamepad =
  sendMessage gcMicroGamepad buttonMenuSelector

-- | The Micro profile can use the raw position values of the touchpad on the remote as D-pad values, or it can create a virtual dpad centered around the first contact point with the surface.
--
-- If NO; a smaller sliding window is created around the initial touch point and subsequent movement is relative to that center. Movement outside the window will slide the window with it to re-center it. This is great for surfaces where there is no clear sense of a middle and drift over time is an issue.
--
-- If YES; the absolute values are used and any drift will have to managed manually either through user traning or by a developer using the dpad.
--
-- The default value for this property is NO, meaning a sliding window is used for the dpad.
--
-- ObjC selector: @- reportsAbsoluteDpadValues@
reportsAbsoluteDpadValues :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> IO Bool
reportsAbsoluteDpadValues gcMicroGamepad =
  sendMessage gcMicroGamepad reportsAbsoluteDpadValuesSelector

-- | The Micro profile can use the raw position values of the touchpad on the remote as D-pad values, or it can create a virtual dpad centered around the first contact point with the surface.
--
-- If NO; a smaller sliding window is created around the initial touch point and subsequent movement is relative to that center. Movement outside the window will slide the window with it to re-center it. This is great for surfaces where there is no clear sense of a middle and drift over time is an issue.
--
-- If YES; the absolute values are used and any drift will have to managed manually either through user traning or by a developer using the dpad.
--
-- The default value for this property is NO, meaning a sliding window is used for the dpad.
--
-- ObjC selector: @- setReportsAbsoluteDpadValues:@
setReportsAbsoluteDpadValues :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> Bool -> IO ()
setReportsAbsoluteDpadValues gcMicroGamepad value =
  sendMessage gcMicroGamepad setReportsAbsoluteDpadValuesSelector value

-- | Allows the Micro profile to monitor the orientation of the controller, if the controller is positioned in landscape orientation, D-pad input values will be transposed 90 degrees to match the new orientation.
--
-- The default value for this property is NO.
--
-- ObjC selector: @- allowsRotation@
allowsRotation :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> IO Bool
allowsRotation gcMicroGamepad =
  sendMessage gcMicroGamepad allowsRotationSelector

-- | Allows the Micro profile to monitor the orientation of the controller, if the controller is positioned in landscape orientation, D-pad input values will be transposed 90 degrees to match the new orientation.
--
-- The default value for this property is NO.
--
-- ObjC selector: @- setAllowsRotation:@
setAllowsRotation :: IsGCMicroGamepad gcMicroGamepad => gcMicroGamepad -> Bool -> IO ()
setAllowsRotation gcMicroGamepad value =
  sendMessage gcMicroGamepad setAllowsRotationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @saveSnapshot@
saveSnapshotSelector :: Selector '[] (Id GCMicroGamepadSnapshot)
saveSnapshotSelector = mkSelector "saveSnapshot"

-- | @Selector@ for @setStateFromMicroGamepad:@
setStateFromMicroGamepadSelector :: Selector '[Id GCMicroGamepad] ()
setStateFromMicroGamepadSelector = mkSelector "setStateFromMicroGamepad:"

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

-- | @Selector@ for @buttonX@
buttonXSelector :: Selector '[] (Id GCControllerButtonInput)
buttonXSelector = mkSelector "buttonX"

-- | @Selector@ for @buttonMenu@
buttonMenuSelector :: Selector '[] (Id GCControllerButtonInput)
buttonMenuSelector = mkSelector "buttonMenu"

-- | @Selector@ for @reportsAbsoluteDpadValues@
reportsAbsoluteDpadValuesSelector :: Selector '[] Bool
reportsAbsoluteDpadValuesSelector = mkSelector "reportsAbsoluteDpadValues"

-- | @Selector@ for @setReportsAbsoluteDpadValues:@
setReportsAbsoluteDpadValuesSelector :: Selector '[Bool] ()
setReportsAbsoluteDpadValuesSelector = mkSelector "setReportsAbsoluteDpadValues:"

-- | @Selector@ for @allowsRotation@
allowsRotationSelector :: Selector '[] Bool
allowsRotationSelector = mkSelector "allowsRotation"

-- | @Selector@ for @setAllowsRotation:@
setAllowsRotationSelector :: Selector '[Bool] ()
setAllowsRotationSelector = mkSelector "setAllowsRotation:"

