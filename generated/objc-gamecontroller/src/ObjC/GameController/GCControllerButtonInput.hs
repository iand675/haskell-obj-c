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
  , setValueSelector
  , valueChangedHandlerSelector
  , setValueChangedHandlerSelector
  , pressedChangedHandlerSelector
  , setPressedChangedHandlerSelector
  , touchedChangedHandlerSelector
  , setTouchedChangedHandlerSelector
  , valueSelector
  , pressedSelector
  , touchedSelector


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
setValue gcControllerButtonInput  value =
  sendMsg gcControllerButtonInput (mkSelector "setValue:") retVoid [argCFloat (fromIntegral value)]

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> IO (Ptr ())
valueChangedHandler gcControllerButtonInput  =
  fmap castPtr $ sendMsg gcControllerButtonInput (mkSelector "valueChangedHandler") (retPtr retVoid) []

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> Ptr () -> IO ()
setValueChangedHandler gcControllerButtonInput  value =
  sendMsg gcControllerButtonInput (mkSelector "setValueChangedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Set this block if you want to be notified when only the pressed state on this button changes. This will get called less often than the valueChangedHandler with the additional feature of the pressed state being different to the last time it was called.
--
-- ObjC selector: @- pressedChangedHandler@
pressedChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> IO (Ptr ())
pressedChangedHandler gcControllerButtonInput  =
  fmap castPtr $ sendMsg gcControllerButtonInput (mkSelector "pressedChangedHandler") (retPtr retVoid) []

-- | Set this block if you want to be notified when only the pressed state on this button changes. This will get called less often than the valueChangedHandler with the additional feature of the pressed state being different to the last time it was called.
--
-- ObjC selector: @- setPressedChangedHandler:@
setPressedChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> Ptr () -> IO ()
setPressedChangedHandler gcControllerButtonInput  value =
  sendMsg gcControllerButtonInput (mkSelector "setPressedChangedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- touchedChangedHandler@
touchedChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> IO (Ptr ())
touchedChangedHandler gcControllerButtonInput  =
  fmap castPtr $ sendMsg gcControllerButtonInput (mkSelector "touchedChangedHandler") (retPtr retVoid) []

-- | @- setTouchedChangedHandler:@
setTouchedChangedHandler :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> Ptr () -> IO ()
setTouchedChangedHandler gcControllerButtonInput  value =
  sendMsg gcControllerButtonInput (mkSelector "setTouchedChangedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | A normalized value for the input. Between 0 and 1 for button inputs. Values are saturated and thus never exceed the range of [0, 1].
--
-- See: valueChangedHandler
--
-- See: pressed
--
-- ObjC selector: @- value@
value :: IsGCControllerButtonInput gcControllerButtonInput => gcControllerButtonInput -> IO CFloat
value gcControllerButtonInput  =
  sendMsg gcControllerButtonInput (mkSelector "value") retCFloat []

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
pressed gcControllerButtonInput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcControllerButtonInput (mkSelector "pressed") retCULong []

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
touched gcControllerButtonInput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcControllerButtonInput (mkSelector "touched") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @valueChangedHandler@
valueChangedHandlerSelector :: Selector
valueChangedHandlerSelector = mkSelector "valueChangedHandler"

-- | @Selector@ for @setValueChangedHandler:@
setValueChangedHandlerSelector :: Selector
setValueChangedHandlerSelector = mkSelector "setValueChangedHandler:"

-- | @Selector@ for @pressedChangedHandler@
pressedChangedHandlerSelector :: Selector
pressedChangedHandlerSelector = mkSelector "pressedChangedHandler"

-- | @Selector@ for @setPressedChangedHandler:@
setPressedChangedHandlerSelector :: Selector
setPressedChangedHandlerSelector = mkSelector "setPressedChangedHandler:"

-- | @Selector@ for @touchedChangedHandler@
touchedChangedHandlerSelector :: Selector
touchedChangedHandlerSelector = mkSelector "touchedChangedHandler"

-- | @Selector@ for @setTouchedChangedHandler:@
setTouchedChangedHandlerSelector :: Selector
setTouchedChangedHandlerSelector = mkSelector "setTouchedChangedHandler:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @pressed@
pressedSelector :: Selector
pressedSelector = mkSelector "pressed"

-- | @Selector@ for @touched@
touchedSelector :: Selector
touchedSelector = mkSelector "touched"

