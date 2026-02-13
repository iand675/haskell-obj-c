{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A direction pad is a common grouping of 2 axis inputs where the input can also be interpreted as 2 sets of mutually exclusive button pairs. Only one button in each pair, {up, down} and {left, right}, can be pressed at any one time.
--
-- Generated bindings for @GCControllerDirectionPad@.
module ObjC.GameController.GCControllerDirectionPad
  ( GCControllerDirectionPad
  , IsGCControllerDirectionPad(..)
  , setValueForXAxis_yAxis
  , valueChangedHandler
  , setValueChangedHandler
  , xAxis
  , yAxis
  , up
  , down
  , left
  , right
  , downSelector
  , leftSelector
  , rightSelector
  , setValueChangedHandlerSelector
  , setValueForXAxis_yAxisSelector
  , upSelector
  , valueChangedHandlerSelector
  , xAxisSelector
  , yAxisSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Sets the normalized value for the direction pad's axis inputs. Will update the states of the direction pad's button inputs as well.
--
-- @xAxis@ — the value to set the xAxis of the touchpad to.
--
-- @yAxis@ — the value to set the yAxis of the touchpad to.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: value
--
-- See: pressed
--
-- ObjC selector: @- setValueForXAxis:yAxis:@
setValueForXAxis_yAxis :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> CFloat -> CFloat -> IO ()
setValueForXAxis_yAxis gcControllerDirectionPad xAxis yAxis =
  sendMessage gcControllerDirectionPad setValueForXAxis_yAxisSelector xAxis yAxis

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Ptr ())
valueChangedHandler gcControllerDirectionPad =
  sendMessage gcControllerDirectionPad valueChangedHandlerSelector

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> Ptr () -> IO ()
setValueChangedHandler gcControllerDirectionPad value =
  sendMessage gcControllerDirectionPad setValueChangedHandlerSelector value

-- | @- xAxis@
xAxis :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerAxisInput)
xAxis gcControllerDirectionPad =
  sendMessage gcControllerDirectionPad xAxisSelector

-- | @- yAxis@
yAxis :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerAxisInput)
yAxis gcControllerDirectionPad =
  sendMessage gcControllerDirectionPad yAxisSelector

-- | @- up@
up :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerButtonInput)
up gcControllerDirectionPad =
  sendMessage gcControllerDirectionPad upSelector

-- | @- down@
down :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerButtonInput)
down gcControllerDirectionPad =
  sendMessage gcControllerDirectionPad downSelector

-- | @- left@
left :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerButtonInput)
left gcControllerDirectionPad =
  sendMessage gcControllerDirectionPad leftSelector

-- | @- right@
right :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerButtonInput)
right gcControllerDirectionPad =
  sendMessage gcControllerDirectionPad rightSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValueForXAxis:yAxis:@
setValueForXAxis_yAxisSelector :: Selector '[CFloat, CFloat] ()
setValueForXAxis_yAxisSelector = mkSelector "setValueForXAxis:yAxis:"

-- | @Selector@ for @valueChangedHandler@
valueChangedHandlerSelector :: Selector '[] (Ptr ())
valueChangedHandlerSelector = mkSelector "valueChangedHandler"

-- | @Selector@ for @setValueChangedHandler:@
setValueChangedHandlerSelector :: Selector '[Ptr ()] ()
setValueChangedHandlerSelector = mkSelector "setValueChangedHandler:"

-- | @Selector@ for @xAxis@
xAxisSelector :: Selector '[] (Id GCControllerAxisInput)
xAxisSelector = mkSelector "xAxis"

-- | @Selector@ for @yAxis@
yAxisSelector :: Selector '[] (Id GCControllerAxisInput)
yAxisSelector = mkSelector "yAxis"

-- | @Selector@ for @up@
upSelector :: Selector '[] (Id GCControllerButtonInput)
upSelector = mkSelector "up"

-- | @Selector@ for @down@
downSelector :: Selector '[] (Id GCControllerButtonInput)
downSelector = mkSelector "down"

-- | @Selector@ for @left@
leftSelector :: Selector '[] (Id GCControllerButtonInput)
leftSelector = mkSelector "left"

-- | @Selector@ for @right@
rightSelector :: Selector '[] (Id GCControllerButtonInput)
rightSelector = mkSelector "right"

