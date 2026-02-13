{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPressGestureRecognizer@.
module ObjC.AppKit.NSPressGestureRecognizer
  ( NSPressGestureRecognizer
  , IsNSPressGestureRecognizer(..)
  , buttonMask
  , setButtonMask
  , minimumPressDuration
  , setMinimumPressDuration
  , allowableMovement
  , setAllowableMovement
  , numberOfTouchesRequired
  , setNumberOfTouchesRequired
  , allowableMovementSelector
  , buttonMaskSelector
  , minimumPressDurationSelector
  , numberOfTouchesRequiredSelector
  , setAllowableMovementSelector
  , setButtonMaskSelector
  , setMinimumPressDurationSelector
  , setNumberOfTouchesRequiredSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- buttonMask@
buttonMask :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> IO CULong
buttonMask nsPressGestureRecognizer =
  sendMessage nsPressGestureRecognizer buttonMaskSelector

-- | @- setButtonMask:@
setButtonMask :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> CULong -> IO ()
setButtonMask nsPressGestureRecognizer value =
  sendMessage nsPressGestureRecognizer setButtonMaskSelector value

-- | @- minimumPressDuration@
minimumPressDuration :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> IO CDouble
minimumPressDuration nsPressGestureRecognizer =
  sendMessage nsPressGestureRecognizer minimumPressDurationSelector

-- | @- setMinimumPressDuration:@
setMinimumPressDuration :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> CDouble -> IO ()
setMinimumPressDuration nsPressGestureRecognizer value =
  sendMessage nsPressGestureRecognizer setMinimumPressDurationSelector value

-- | @- allowableMovement@
allowableMovement :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> IO CDouble
allowableMovement nsPressGestureRecognizer =
  sendMessage nsPressGestureRecognizer allowableMovementSelector

-- | @- setAllowableMovement:@
setAllowableMovement :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> CDouble -> IO ()
setAllowableMovement nsPressGestureRecognizer value =
  sendMessage nsPressGestureRecognizer setAllowableMovementSelector value

-- | @- numberOfTouchesRequired@
numberOfTouchesRequired :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> IO CLong
numberOfTouchesRequired nsPressGestureRecognizer =
  sendMessage nsPressGestureRecognizer numberOfTouchesRequiredSelector

-- | @- setNumberOfTouchesRequired:@
setNumberOfTouchesRequired :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> CLong -> IO ()
setNumberOfTouchesRequired nsPressGestureRecognizer value =
  sendMessage nsPressGestureRecognizer setNumberOfTouchesRequiredSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonMask@
buttonMaskSelector :: Selector '[] CULong
buttonMaskSelector = mkSelector "buttonMask"

-- | @Selector@ for @setButtonMask:@
setButtonMaskSelector :: Selector '[CULong] ()
setButtonMaskSelector = mkSelector "setButtonMask:"

-- | @Selector@ for @minimumPressDuration@
minimumPressDurationSelector :: Selector '[] CDouble
minimumPressDurationSelector = mkSelector "minimumPressDuration"

-- | @Selector@ for @setMinimumPressDuration:@
setMinimumPressDurationSelector :: Selector '[CDouble] ()
setMinimumPressDurationSelector = mkSelector "setMinimumPressDuration:"

-- | @Selector@ for @allowableMovement@
allowableMovementSelector :: Selector '[] CDouble
allowableMovementSelector = mkSelector "allowableMovement"

-- | @Selector@ for @setAllowableMovement:@
setAllowableMovementSelector :: Selector '[CDouble] ()
setAllowableMovementSelector = mkSelector "setAllowableMovement:"

-- | @Selector@ for @numberOfTouchesRequired@
numberOfTouchesRequiredSelector :: Selector '[] CLong
numberOfTouchesRequiredSelector = mkSelector "numberOfTouchesRequired"

-- | @Selector@ for @setNumberOfTouchesRequired:@
setNumberOfTouchesRequiredSelector :: Selector '[CLong] ()
setNumberOfTouchesRequiredSelector = mkSelector "setNumberOfTouchesRequired:"

