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
  , saveSnapshotSelector
  , controllerSelector
  , valueChangedHandlerSelector
  , setValueChangedHandlerSelector
  , dpadSelector
  , buttonASelector
  , buttonBSelector
  , buttonXSelector
  , buttonYSelector
  , leftShoulderSelector
  , rightShoulderSelector


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
saveSnapshot :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCGamepadSnapshot)
saveSnapshot gcGamepad  =
  sendMsg gcGamepad (mkSelector "saveSnapshot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A profile keeps a reference to the controller that this profile is mapping input from.
--
-- ObjC selector: @- controller@
controller :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCController)
controller gcGamepad  =
  sendMsg gcGamepad (mkSelector "controller") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCGamepad gcGamepad => gcGamepad -> IO (Ptr ())
valueChangedHandler gcGamepad  =
  fmap castPtr $ sendMsg gcGamepad (mkSelector "valueChangedHandler") (retPtr retVoid) []

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCGamepad gcGamepad => gcGamepad -> Ptr () -> IO ()
setValueChangedHandler gcGamepad  value =
  sendMsg gcGamepad (mkSelector "setValueChangedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Required to be analog in the Standard profile. All the elements of this directional input are thus analog.
--
-- ObjC selector: @- dpad@
dpad :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerDirectionPad)
dpad gcGamepad  =
  sendMsg gcGamepad (mkSelector "dpad") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All face buttons are required to be analog in the Standard profile. These must be arranged in the diamond pattern given below:
--
-- Y  / \\ X   B  \\ /   A
--
-- ObjC selector: @- buttonA@
buttonA :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
buttonA gcGamepad  =
  sendMsg gcGamepad (mkSelector "buttonA") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- buttonB@
buttonB :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
buttonB gcGamepad  =
  sendMsg gcGamepad (mkSelector "buttonB") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- buttonX@
buttonX :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
buttonX gcGamepad  =
  sendMsg gcGamepad (mkSelector "buttonX") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- buttonY@
buttonY :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
buttonY gcGamepad  =
  sendMsg gcGamepad (mkSelector "buttonY") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Shoulder buttons are required to be analog inputs.
--
-- ObjC selector: @- leftShoulder@
leftShoulder :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
leftShoulder gcGamepad  =
  sendMsg gcGamepad (mkSelector "leftShoulder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Shoulder buttons are required to be analog inputs.
--
-- ObjC selector: @- rightShoulder@
rightShoulder :: IsGCGamepad gcGamepad => gcGamepad -> IO (Id GCControllerButtonInput)
rightShoulder gcGamepad  =
  sendMsg gcGamepad (mkSelector "rightShoulder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @saveSnapshot@
saveSnapshotSelector :: Selector
saveSnapshotSelector = mkSelector "saveSnapshot"

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

-- | @Selector@ for @leftShoulder@
leftShoulderSelector :: Selector
leftShoulderSelector = mkSelector "leftShoulder"

-- | @Selector@ for @rightShoulder@
rightShoulderSelector :: Selector
rightShoulderSelector = mkSelector "rightShoulder"

