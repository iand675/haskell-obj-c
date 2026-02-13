{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCControllerButtonInput@.
module ObjC.GameController.GCControllerButtonInput
  ( GCControllerButtonInput
  , IsGCControllerButtonInput(..)
  , setValue
  , valueChangedHandler
  , setValueChangedHandler
  , pressedChangedHandler
  , setPressedChangedHandler
  , touchedChangedHandler
  , setTouchedChangedHandler
  , value
  , pressed
  , touched
  , pressedChangedHandlerSelector
  , pressedSelector
  , setPressedChangedHandlerSelector
  , setTouchedChangedHandlerSelector
  , setValueChangedHandlerSelector
  , setValueSelector
  , touchedChangedHandlerSelector
  , touchedSelector
  , valueChangedHandlerSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Sets the normalized value for the button input. Will update the pressed state of the button.
--
-- @value@ — the value to set the input to.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: value
--
-- ObjC selector: @- setValue:@
setValue :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> CFloat -> IO ()
setValue gcControllerButtonInput value =
  sendMessage gcControllerButtonInput setValueSelector value

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> IO (Ptr ())
valueChangedHandler gcControllerButtonInput =
  sendMessage gcControllerButtonInput valueChangedHandlerSelector

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> Ptr () -> IO ()
setValueChangedHandler gcControllerButtonInput value =
  sendMessage gcControllerButtonInput setValueChangedHandlerSelector value

-- | Set this block if you want to be notified when only the pressed state on this button changes. This will get called less often than the valueChangedHandler with the additional feature of the pressed state being different to the last time it was called.
--
-- ObjC selector: @- pressedChangedHandler@
pressedChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> IO (Ptr ())
pressedChangedHandler gcControllerButtonInput =
  sendMessage gcControllerButtonInput pressedChangedHandlerSelector

-- | Set this block if you want to be notified when only the pressed state on this button changes. This will get called less often than the valueChangedHandler with the additional feature of the pressed state being different to the last time it was called.
--
-- ObjC selector: @- setPressedChangedHandler:@
setPressedChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> Ptr () -> IO ()
setPressedChangedHandler gcControllerButtonInput value =
  sendMessage gcControllerButtonInput setPressedChangedHandlerSelector value

-- | @- touchedChangedHandler@
touchedChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> IO (Ptr ())
touchedChangedHandler gcControllerButtonInput =
  sendMessage gcControllerButtonInput touchedChangedHandlerSelector

-- | @- setTouchedChangedHandler:@
setTouchedChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> Ptr () -> IO ()
setTouchedChangedHandler gcControllerButtonInput value =
  sendMessage gcControllerButtonInput setTouchedChangedHandlerSelector value

-- | A normalized value for the input. Between 0 and 1 for button inputs. Values are saturated and thus never exceed the range of [0, 1].
--
-- See: valueChangedHandler
--
-- See: pressed
--
-- ObjC selector: @- value@
value :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> IO CFloat
value gcControllerButtonInput =
  sendMessage gcControllerButtonInput valueSelector

-- | Buttons are mostly used in a digital sense, thus we have a recommended method for checking for pressed state instead of interpreting the value.
--
-- As a general guideline a button is pressed if the value exceeds 0. However there may be hysterisis applied to counter noisy input values, thus incidental values around the threshold value may not trigger a change in pressed state.
--
-- Others buttons may support two-stage actuation, where the button reports a value between 0 and 1 but is only considered pressed when its value is greater than some threshold other than 0.
--
-- See: pressedChangedHandler
--
-- See: value
--
-- ObjC selector: @- pressed@
pressed :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> IO Bool
pressed gcControllerButtonInput =
  sendMessage gcControllerButtonInput pressedSelector

-- | Some buttons feature capacitive touch capabilities where the user can touch the button without pressing it. In such cases, a button will be touched before it is pressed.
--
-- For buttons without capacitive sensing, the touched state is true if the value exceeds 0.
--
-- See: touchChangedHandler
--
-- See: pressed
--
-- ObjC selector: @- touched@
touched :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> IO Bool
touched gcControllerButtonInput =
  sendMessage gcControllerButtonInput touchedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CFloat] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @valueChangedHandler@
valueChangedHandlerSelector :: Selector '[] (Ptr ())
valueChangedHandlerSelector = mkSelector "valueChangedHandler"

-- | @Selector@ for @setValueChangedHandler:@
setValueChangedHandlerSelector :: Selector '[Ptr ()] ()
setValueChangedHandlerSelector = mkSelector "setValueChangedHandler:"

-- | @Selector@ for @pressedChangedHandler@
pressedChangedHandlerSelector :: Selector '[] (Ptr ())
pressedChangedHandlerSelector = mkSelector "pressedChangedHandler"

-- | @Selector@ for @setPressedChangedHandler:@
setPressedChangedHandlerSelector :: Selector '[Ptr ()] ()
setPressedChangedHandlerSelector = mkSelector "setPressedChangedHandler:"

-- | @Selector@ for @touchedChangedHandler@
touchedChangedHandlerSelector :: Selector '[] (Ptr ())
touchedChangedHandlerSelector = mkSelector "touchedChangedHandler"

-- | @Selector@ for @setTouchedChangedHandler:@
setTouchedChangedHandlerSelector :: Selector '[Ptr ()] ()
setTouchedChangedHandlerSelector = mkSelector "setTouchedChangedHandler:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CFloat
valueSelector = mkSelector "value"

-- | @Selector@ for @pressed@
pressedSelector :: Selector '[] Bool
pressedSelector = mkSelector "pressed"

-- | @Selector@ for @touched@
touchedSelector :: Selector '[] Bool
touchedSelector = mkSelector "touched"

