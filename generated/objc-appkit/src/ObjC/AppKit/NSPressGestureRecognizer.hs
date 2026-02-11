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
  , buttonMaskSelector
  , setButtonMaskSelector
  , minimumPressDurationSelector
  , setMinimumPressDurationSelector
  , allowableMovementSelector
  , setAllowableMovementSelector
  , numberOfTouchesRequiredSelector
  , setNumberOfTouchesRequiredSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- buttonMask@
buttonMask :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> IO CULong
buttonMask nsPressGestureRecognizer  =
  sendMsg nsPressGestureRecognizer (mkSelector "buttonMask") retCULong []

-- | @- setButtonMask:@
setButtonMask :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> CULong -> IO ()
setButtonMask nsPressGestureRecognizer  value =
  sendMsg nsPressGestureRecognizer (mkSelector "setButtonMask:") retVoid [argCULong (fromIntegral value)]

-- | @- minimumPressDuration@
minimumPressDuration :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> IO CDouble
minimumPressDuration nsPressGestureRecognizer  =
  sendMsg nsPressGestureRecognizer (mkSelector "minimumPressDuration") retCDouble []

-- | @- setMinimumPressDuration:@
setMinimumPressDuration :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> CDouble -> IO ()
setMinimumPressDuration nsPressGestureRecognizer  value =
  sendMsg nsPressGestureRecognizer (mkSelector "setMinimumPressDuration:") retVoid [argCDouble (fromIntegral value)]

-- | @- allowableMovement@
allowableMovement :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> IO CDouble
allowableMovement nsPressGestureRecognizer  =
  sendMsg nsPressGestureRecognizer (mkSelector "allowableMovement") retCDouble []

-- | @- setAllowableMovement:@
setAllowableMovement :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> CDouble -> IO ()
setAllowableMovement nsPressGestureRecognizer  value =
  sendMsg nsPressGestureRecognizer (mkSelector "setAllowableMovement:") retVoid [argCDouble (fromIntegral value)]

-- | @- numberOfTouchesRequired@
numberOfTouchesRequired :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> IO CLong
numberOfTouchesRequired nsPressGestureRecognizer  =
  sendMsg nsPressGestureRecognizer (mkSelector "numberOfTouchesRequired") retCLong []

-- | @- setNumberOfTouchesRequired:@
setNumberOfTouchesRequired :: IsNSPressGestureRecognizer nsPressGestureRecognizer => nsPressGestureRecognizer -> CLong -> IO ()
setNumberOfTouchesRequired nsPressGestureRecognizer  value =
  sendMsg nsPressGestureRecognizer (mkSelector "setNumberOfTouchesRequired:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonMask@
buttonMaskSelector :: Selector
buttonMaskSelector = mkSelector "buttonMask"

-- | @Selector@ for @setButtonMask:@
setButtonMaskSelector :: Selector
setButtonMaskSelector = mkSelector "setButtonMask:"

-- | @Selector@ for @minimumPressDuration@
minimumPressDurationSelector :: Selector
minimumPressDurationSelector = mkSelector "minimumPressDuration"

-- | @Selector@ for @setMinimumPressDuration:@
setMinimumPressDurationSelector :: Selector
setMinimumPressDurationSelector = mkSelector "setMinimumPressDuration:"

-- | @Selector@ for @allowableMovement@
allowableMovementSelector :: Selector
allowableMovementSelector = mkSelector "allowableMovement"

-- | @Selector@ for @setAllowableMovement:@
setAllowableMovementSelector :: Selector
setAllowableMovementSelector = mkSelector "setAllowableMovement:"

-- | @Selector@ for @numberOfTouchesRequired@
numberOfTouchesRequiredSelector :: Selector
numberOfTouchesRequiredSelector = mkSelector "numberOfTouchesRequired"

-- | @Selector@ for @setNumberOfTouchesRequired:@
setNumberOfTouchesRequiredSelector :: Selector
setNumberOfTouchesRequiredSelector = mkSelector "setNumberOfTouchesRequired:"

