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
  , translationInViewSelector
  , setTranslation_inViewSelector
  , velocityInViewSelector
  , buttonMaskSelector
  , setButtonMaskSelector
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- translationInView:@
translationInView :: (IsNSPanGestureRecognizer nsPanGestureRecognizer, IsNSView view) => nsPanGestureRecognizer -> view -> IO NSPoint
translationInView nsPanGestureRecognizer  view =
withObjCPtr view $ \raw_view ->
    sendMsgStret nsPanGestureRecognizer (mkSelector "translationInView:") retNSPoint [argPtr (castPtr raw_view :: Ptr ())]

-- | @- setTranslation:inView:@
setTranslation_inView :: (IsNSPanGestureRecognizer nsPanGestureRecognizer, IsNSView view) => nsPanGestureRecognizer -> NSPoint -> view -> IO ()
setTranslation_inView nsPanGestureRecognizer  translation view =
withObjCPtr view $ \raw_view ->
    sendMsg nsPanGestureRecognizer (mkSelector "setTranslation:inView:") retVoid [argNSPoint translation, argPtr (castPtr raw_view :: Ptr ())]

-- | @- velocityInView:@
velocityInView :: (IsNSPanGestureRecognizer nsPanGestureRecognizer, IsNSView view) => nsPanGestureRecognizer -> view -> IO NSPoint
velocityInView nsPanGestureRecognizer  view =
withObjCPtr view $ \raw_view ->
    sendMsgStret nsPanGestureRecognizer (mkSelector "velocityInView:") retNSPoint [argPtr (castPtr raw_view :: Ptr ())]

-- | @- buttonMask@
buttonMask :: IsNSPanGestureRecognizer nsPanGestureRecognizer => nsPanGestureRecognizer -> IO CULong
buttonMask nsPanGestureRecognizer  =
  sendMsg nsPanGestureRecognizer (mkSelector "buttonMask") retCULong []

-- | @- setButtonMask:@
setButtonMask :: IsNSPanGestureRecognizer nsPanGestureRecognizer => nsPanGestureRecognizer -> CULong -> IO ()
setButtonMask nsPanGestureRecognizer  value =
  sendMsg nsPanGestureRecognizer (mkSelector "setButtonMask:") retVoid [argCULong (fromIntegral value)]

-- | @- numberOfTouchesRequired@
numberOfTouchesRequired :: IsNSPanGestureRecognizer nsPanGestureRecognizer => nsPanGestureRecognizer -> IO CLong
numberOfTouchesRequired nsPanGestureRecognizer  =
  sendMsg nsPanGestureRecognizer (mkSelector "numberOfTouchesRequired") retCLong []

-- | @- setNumberOfTouchesRequired:@
setNumberOfTouchesRequired :: IsNSPanGestureRecognizer nsPanGestureRecognizer => nsPanGestureRecognizer -> CLong -> IO ()
setNumberOfTouchesRequired nsPanGestureRecognizer  value =
  sendMsg nsPanGestureRecognizer (mkSelector "setNumberOfTouchesRequired:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @translationInView:@
translationInViewSelector :: Selector
translationInViewSelector = mkSelector "translationInView:"

-- | @Selector@ for @setTranslation:inView:@
setTranslation_inViewSelector :: Selector
setTranslation_inViewSelector = mkSelector "setTranslation:inView:"

-- | @Selector@ for @velocityInView:@
velocityInViewSelector :: Selector
velocityInViewSelector = mkSelector "velocityInView:"

-- | @Selector@ for @buttonMask@
buttonMaskSelector :: Selector
buttonMaskSelector = mkSelector "buttonMask"

-- | @Selector@ for @setButtonMask:@
setButtonMaskSelector :: Selector
setButtonMaskSelector = mkSelector "setButtonMask:"

-- | @Selector@ for @numberOfTouchesRequired@
numberOfTouchesRequiredSelector :: Selector
numberOfTouchesRequiredSelector = mkSelector "numberOfTouchesRequired"

-- | @Selector@ for @setNumberOfTouchesRequired:@
setNumberOfTouchesRequiredSelector :: Selector
setNumberOfTouchesRequiredSelector = mkSelector "setNumberOfTouchesRequired:"

