{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A touchpad is a touch-based two axis input with a notion of "touch state". It keeps track of whether the touchpad is actively being touched, and generates events based on a change in touch state.
--
-- Generated bindings for @GCControllerTouchpad@.
module ObjC.GameController.GCControllerTouchpad
  ( GCControllerTouchpad
  , IsGCControllerTouchpad(..)
  , setValueForXAxis_yAxis_touchDown_buttonValue
  , button
  , touchDown
  , setTouchDown
  , touchMoved
  , setTouchMoved
  , touchUp
  , setTouchUp
  , touchSurface
  , touchState
  , reportsAbsoluteTouchSurfaceValues
  , setReportsAbsoluteTouchSurfaceValues
  , setValueForXAxis_yAxis_touchDown_buttonValueSelector
  , buttonSelector
  , touchDownSelector
  , setTouchDownSelector
  , touchMovedSelector
  , setTouchMovedSelector
  , touchUpSelector
  , setTouchUpSelector
  , touchSurfaceSelector
  , touchStateSelector
  , reportsAbsoluteTouchSurfaceValuesSelector
  , setReportsAbsoluteTouchSurfaceValuesSelector

  -- * Enum types
  , GCTouchState(GCTouchState)
  , pattern GCTouchStateUp
  , pattern GCTouchStateDown
  , pattern GCTouchStateMoving

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
import ObjC.GameController.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Sets the normalized value for the touchpad's axes, as well as its current touch and button state.
--
-- Note: If the controller's snapshot flag is set to NO, this method has no effect.
--
-- See: touchSurface
--
-- See: touchState
--
-- ObjC selector: @- setValueForXAxis:yAxis:touchDown:buttonValue:@
setValueForXAxis_yAxis_touchDown_buttonValue :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> CFloat -> CFloat -> Bool -> CFloat -> IO ()
setValueForXAxis_yAxis_touchDown_buttonValue gcControllerTouchpad  xAxis yAxis touchDown buttonValue =
  sendMsg gcControllerTouchpad (mkSelector "setValueForXAxis:yAxis:touchDown:buttonValue:") retVoid [argCFloat (fromIntegral xAxis), argCFloat (fromIntegral yAxis), argCULong (if touchDown then 1 else 0), argCFloat (fromIntegral buttonValue)]

-- | Button is the button built into the touch surface.
--
-- ObjC selector: @- button@
button :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> IO (Id GCControllerButtonInput)
button gcControllerTouchpad  =
  sendMsg gcControllerTouchpad (mkSelector "button") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Called when a touch event begins on the touchpad.
--
-- ObjC selector: @- touchDown@
touchDown :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> IO (Ptr ())
touchDown gcControllerTouchpad  =
  fmap castPtr $ sendMsg gcControllerTouchpad (mkSelector "touchDown") (retPtr retVoid) []

-- | Called when a touch event begins on the touchpad.
--
-- ObjC selector: @- setTouchDown:@
setTouchDown :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> Ptr () -> IO ()
setTouchDown gcControllerTouchpad  value =
  sendMsg gcControllerTouchpad (mkSelector "setTouchDown:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called when a touch event continues on the touchpad, but not when it begins or ends.
--
-- ObjC selector: @- touchMoved@
touchMoved :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> IO (Ptr ())
touchMoved gcControllerTouchpad  =
  fmap castPtr $ sendMsg gcControllerTouchpad (mkSelector "touchMoved") (retPtr retVoid) []

-- | Called when a touch event continues on the touchpad, but not when it begins or ends.
--
-- ObjC selector: @- setTouchMoved:@
setTouchMoved :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> Ptr () -> IO ()
setTouchMoved gcControllerTouchpad  value =
  sendMsg gcControllerTouchpad (mkSelector "setTouchMoved:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called when a touch event ends on the touchpad.
--
-- ObjC selector: @- touchUp@
touchUp :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> IO (Ptr ())
touchUp gcControllerTouchpad  =
  fmap castPtr $ sendMsg gcControllerTouchpad (mkSelector "touchUp") (retPtr retVoid) []

-- | Called when a touch event ends on the touchpad.
--
-- ObjC selector: @- setTouchUp:@
setTouchUp :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> Ptr () -> IO ()
setTouchUp gcControllerTouchpad  value =
  sendMsg gcControllerTouchpad (mkSelector "setTouchUp:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | The touch surface is a 2-axis control that represents the position of a touch event on the touchpad.
--
-- The axes will indicate the most recent touch position - a non-zero value does not indicate that the surface is being touched, and a value of (0, 0) does not indicate the surface is not being touched.
--
-- See: touchState - Should be polled in conjunction with touchSurface to determine if values are valid
--
-- ObjC selector: @- touchSurface@
touchSurface :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> IO (Id GCControllerDirectionPad)
touchSurface gcControllerTouchpad  =
  sendMsg gcControllerTouchpad (mkSelector "touchSurface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the current state of the touch event on the touchpad.
--
-- ObjC selector: @- touchState@
touchState :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> IO GCTouchState
touchState gcControllerTouchpad  =
  fmap (coerce :: CLong -> GCTouchState) $ sendMsg gcControllerTouchpad (mkSelector "touchState") retCLong []

-- | The touchpad can use the raw position values of its surface as D-pad values, or it can create a virtual dpad centered around the first contact point with the surface.
--
-- If NO; a smaller sliding window is created around the initial touch point and subsequent movement is relative to that center. Movement outside the window will slide the window with it to re-center it. This is great for surfaces where there is no clear sense of a middle and drift over time is an issue.
--
-- If YES; the absolute values are used and any drift will have to managed manually either through user traning or by a developer using the dpad.
--
-- The default value for this property is YES, meaning the touch surface's raw positional values are reported.
--
-- ObjC selector: @- reportsAbsoluteTouchSurfaceValues@
reportsAbsoluteTouchSurfaceValues :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> IO Bool
reportsAbsoluteTouchSurfaceValues gcControllerTouchpad  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcControllerTouchpad (mkSelector "reportsAbsoluteTouchSurfaceValues") retCULong []

-- | The touchpad can use the raw position values of its surface as D-pad values, or it can create a virtual dpad centered around the first contact point with the surface.
--
-- If NO; a smaller sliding window is created around the initial touch point and subsequent movement is relative to that center. Movement outside the window will slide the window with it to re-center it. This is great for surfaces where there is no clear sense of a middle and drift over time is an issue.
--
-- If YES; the absolute values are used and any drift will have to managed manually either through user traning or by a developer using the dpad.
--
-- The default value for this property is YES, meaning the touch surface's raw positional values are reported.
--
-- ObjC selector: @- setReportsAbsoluteTouchSurfaceValues:@
setReportsAbsoluteTouchSurfaceValues :: IsGCControllerTouchpad gcControllerTouchpad => gcControllerTouchpad -> Bool -> IO ()
setReportsAbsoluteTouchSurfaceValues gcControllerTouchpad  value =
  sendMsg gcControllerTouchpad (mkSelector "setReportsAbsoluteTouchSurfaceValues:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValueForXAxis:yAxis:touchDown:buttonValue:@
setValueForXAxis_yAxis_touchDown_buttonValueSelector :: Selector
setValueForXAxis_yAxis_touchDown_buttonValueSelector = mkSelector "setValueForXAxis:yAxis:touchDown:buttonValue:"

-- | @Selector@ for @button@
buttonSelector :: Selector
buttonSelector = mkSelector "button"

-- | @Selector@ for @touchDown@
touchDownSelector :: Selector
touchDownSelector = mkSelector "touchDown"

-- | @Selector@ for @setTouchDown:@
setTouchDownSelector :: Selector
setTouchDownSelector = mkSelector "setTouchDown:"

-- | @Selector@ for @touchMoved@
touchMovedSelector :: Selector
touchMovedSelector = mkSelector "touchMoved"

-- | @Selector@ for @setTouchMoved:@
setTouchMovedSelector :: Selector
setTouchMovedSelector = mkSelector "setTouchMoved:"

-- | @Selector@ for @touchUp@
touchUpSelector :: Selector
touchUpSelector = mkSelector "touchUp"

-- | @Selector@ for @setTouchUp:@
setTouchUpSelector :: Selector
setTouchUpSelector = mkSelector "setTouchUp:"

-- | @Selector@ for @touchSurface@
touchSurfaceSelector :: Selector
touchSurfaceSelector = mkSelector "touchSurface"

-- | @Selector@ for @touchState@
touchStateSelector :: Selector
touchStateSelector = mkSelector "touchState"

-- | @Selector@ for @reportsAbsoluteTouchSurfaceValues@
reportsAbsoluteTouchSurfaceValuesSelector :: Selector
reportsAbsoluteTouchSurfaceValuesSelector = mkSelector "reportsAbsoluteTouchSurfaceValues"

-- | @Selector@ for @setReportsAbsoluteTouchSurfaceValues:@
setReportsAbsoluteTouchSurfaceValuesSelector :: Selector
setReportsAbsoluteTouchSurfaceValuesSelector = mkSelector "setReportsAbsoluteTouchSurfaceValues:"

