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
  , setValueForXAxis_yAxisSelector
  , valueChangedHandlerSelector
  , setValueChangedHandlerSelector
  , xAxisSelector
  , yAxisSelector
  , upSelector
  , downSelector
  , leftSelector
  , rightSelector


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
setValueForXAxis_yAxis gcControllerDirectionPad  xAxis yAxis =
  sendMsg gcControllerDirectionPad (mkSelector "setValueForXAxis:yAxis:") retVoid [argCFloat (fromIntegral xAxis), argCFloat (fromIntegral yAxis)]

-- | @- valueChangedHandler@
valueChangedHandler :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Ptr ())
valueChangedHandler gcControllerDirectionPad  =
  fmap castPtr $ sendMsg gcControllerDirectionPad (mkSelector "valueChangedHandler") (retPtr retVoid) []

-- | @- setValueChangedHandler:@
setValueChangedHandler :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> Ptr () -> IO ()
setValueChangedHandler gcControllerDirectionPad  value =
  sendMsg gcControllerDirectionPad (mkSelector "setValueChangedHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- xAxis@
xAxis :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerAxisInput)
xAxis gcControllerDirectionPad  =
  sendMsg gcControllerDirectionPad (mkSelector "xAxis") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- yAxis@
yAxis :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerAxisInput)
yAxis gcControllerDirectionPad  =
  sendMsg gcControllerDirectionPad (mkSelector "yAxis") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- up@
up :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerButtonInput)
up gcControllerDirectionPad  =
  sendMsg gcControllerDirectionPad (mkSelector "up") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- down@
down :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerButtonInput)
down gcControllerDirectionPad  =
  sendMsg gcControllerDirectionPad (mkSelector "down") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- left@
left :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerButtonInput)
left gcControllerDirectionPad  =
  sendMsg gcControllerDirectionPad (mkSelector "left") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- right@
right :: IsGCControllerDirectionPad gcControllerDirectionPad => gcControllerDirectionPad -> IO (Id GCControllerButtonInput)
right gcControllerDirectionPad  =
  sendMsg gcControllerDirectionPad (mkSelector "right") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValueForXAxis:yAxis:@
setValueForXAxis_yAxisSelector :: Selector
setValueForXAxis_yAxisSelector = mkSelector "setValueForXAxis:yAxis:"

-- | @Selector@ for @valueChangedHandler@
valueChangedHandlerSelector :: Selector
valueChangedHandlerSelector = mkSelector "valueChangedHandler"

-- | @Selector@ for @setValueChangedHandler:@
setValueChangedHandlerSelector :: Selector
setValueChangedHandlerSelector = mkSelector "setValueChangedHandler:"

-- | @Selector@ for @xAxis@
xAxisSelector :: Selector
xAxisSelector = mkSelector "xAxis"

-- | @Selector@ for @yAxis@
yAxisSelector :: Selector
yAxisSelector = mkSelector "yAxis"

-- | @Selector@ for @up@
upSelector :: Selector
upSelector = mkSelector "up"

-- | @Selector@ for @down@
downSelector :: Selector
downSelector = mkSelector "down"

-- | @Selector@ for @left@
leftSelector :: Selector
leftSelector = mkSelector "left"

-- | @Selector@ for @right@
rightSelector :: Selector
rightSelector = mkSelector "right"

