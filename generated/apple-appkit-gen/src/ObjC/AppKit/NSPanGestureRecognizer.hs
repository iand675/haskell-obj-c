{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPanGestureRecognizer@.
module ObjC.AppKit.NSPanGestureRecognizer
  ( NSPanGestureRecognizer
  , IsNSPanGestureRecognizer(..)
  , translationInView
  , setTranslation_inView
  , velocityInView
  , buttonMask
  , setButtonMask
  , numberOfTouchesRequired
  , setNumberOfTouchesRequired
  , buttonMaskSelector
  , numberOfTouchesRequiredSelector
  , setButtonMaskSelector
  , setNumberOfTouchesRequiredSelector
  , setTranslation_inViewSelector
  , translationInViewSelector
  , velocityInViewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- translationInView:@
translationInView :: (IsNSPanGestureRecognizer nsPanGestureRecognizer, IsNSView view) => nsPanGestureRecognizer -> view -> IO NSPoint
translationInView nsPanGestureRecognizer view =
  sendMessage nsPanGestureRecognizer translationInViewSelector (toNSView view)

-- | @- setTranslation:inView:@
setTranslation_inView :: (IsNSPanGestureRecognizer nsPanGestureRecognizer, IsNSView view) => nsPanGestureRecognizer -> NSPoint -> view -> IO ()
setTranslation_inView nsPanGestureRecognizer translation view =
  sendMessage nsPanGestureRecognizer setTranslation_inViewSelector translation (toNSView view)

-- | @- velocityInView:@
velocityInView :: (IsNSPanGestureRecognizer nsPanGestureRecognizer, IsNSView view) => nsPanGestureRecognizer -> view -> IO NSPoint
velocityInView nsPanGestureRecognizer view =
  sendMessage nsPanGestureRecognizer velocityInViewSelector (toNSView view)

-- | @- buttonMask@
buttonMask :: IsNSPanGestureRecognizer nsPanGestureRecognizer => nsPanGestureRecognizer -> IO CULong
buttonMask nsPanGestureRecognizer =
  sendMessage nsPanGestureRecognizer buttonMaskSelector

-- | @- setButtonMask:@
setButtonMask :: IsNSPanGestureRecognizer nsPanGestureRecognizer => nsPanGestureRecognizer -> CULong -> IO ()
setButtonMask nsPanGestureRecognizer value =
  sendMessage nsPanGestureRecognizer setButtonMaskSelector value

-- | @- numberOfTouchesRequired@
numberOfTouchesRequired :: IsNSPanGestureRecognizer nsPanGestureRecognizer => nsPanGestureRecognizer -> IO CLong
numberOfTouchesRequired nsPanGestureRecognizer =
  sendMessage nsPanGestureRecognizer numberOfTouchesRequiredSelector

-- | @- setNumberOfTouchesRequired:@
setNumberOfTouchesRequired :: IsNSPanGestureRecognizer nsPanGestureRecognizer => nsPanGestureRecognizer -> CLong -> IO ()
setNumberOfTouchesRequired nsPanGestureRecognizer value =
  sendMessage nsPanGestureRecognizer setNumberOfTouchesRequiredSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @translationInView:@
translationInViewSelector :: Selector '[Id NSView] NSPoint
translationInViewSelector = mkSelector "translationInView:"

-- | @Selector@ for @setTranslation:inView:@
setTranslation_inViewSelector :: Selector '[NSPoint, Id NSView] ()
setTranslation_inViewSelector = mkSelector "setTranslation:inView:"

-- | @Selector@ for @velocityInView:@
velocityInViewSelector :: Selector '[Id NSView] NSPoint
velocityInViewSelector = mkSelector "velocityInView:"

-- | @Selector@ for @buttonMask@
buttonMaskSelector :: Selector '[] CULong
buttonMaskSelector = mkSelector "buttonMask"

-- | @Selector@ for @setButtonMask:@
setButtonMaskSelector :: Selector '[CULong] ()
setButtonMaskSelector = mkSelector "setButtonMask:"

-- | @Selector@ for @numberOfTouchesRequired@
numberOfTouchesRequiredSelector :: Selector '[] CLong
numberOfTouchesRequiredSelector = mkSelector "numberOfTouchesRequired"

-- | @Selector@ for @setNumberOfTouchesRequired:@
setNumberOfTouchesRequiredSelector :: Selector '[CLong] ()
setNumberOfTouchesRequiredSelector = mkSelector "setNumberOfTouchesRequired:"

