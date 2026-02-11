{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCControllerAxisInput@.
module ObjC.GameController.GCControllerAxisInput
  ( GCControllerAxisInput
  , IsGCControllerAxisInput(..)
  , setValue
  , valueChangedHandler
  , setValueChangedHandler
  , value
  , setValueSelector
  , valueChangedHandlerSelector
  , setValueChangedHandlerSelector
  , valueSelector


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

-- | Sets the normalized value for the input.
--
-- @value@ — the value to set the input to.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: value
--
-- ObjC selector: @- setValue:@
setValue :: IsGCControllerAxisInput gcControllerAxisInput => gcControllerAxisInput -> CFloat -> IO ()
setValue gcControllerAxisInput  value =
  sendMsg gcControllerAxisInput (mkSelector "setValue:") retVoid [argCFloat (fromIntegral value)]

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCControllerAxisInput gcControllerAxisInput => gcControllerAxisInput -> IO (Ptr ())
valueChangedHandler gcControllerAxisInput  =
  fmap castPtr $ sendMsg gcControllerAxisInput (mkSelector "valueChangedHandler") (retPtr retVoid) []

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCControllerAxisInput gcControllerAxisInput => gcControllerAxisInput -> Ptr () -> IO ()
setValueChangedHandler gcControllerAxisInput  value =
  sendMsg gcControllerAxisInput (mkSelector "setValueChangedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | A normalized value for the input, between -1 and 1 for axis inputs. The values are deadzoned and saturated before they are returned so there is no value ouside the range. Deadzoning does not remove values from the range, the full 0 to 1 magnitude of values are possible from the input.
--
-- As an axis is often used in a digital sense, you can rely on a value of 0 meaning the axis is inside the deadzone. Any value greater than or less than zero is not in the deadzone.
--
-- ObjC selector: @- value@
value :: IsGCControllerAxisInput gcControllerAxisInput => gcControllerAxisInput -> IO CFloat
value gcControllerAxisInput  =
  sendMsg gcControllerAxisInput (mkSelector "value") retCFloat []

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

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

