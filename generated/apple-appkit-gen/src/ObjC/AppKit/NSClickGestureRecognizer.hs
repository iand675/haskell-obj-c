{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSClickGestureRecognizer@.
module ObjC.AppKit.NSClickGestureRecognizer
  ( NSClickGestureRecognizer
  , IsNSClickGestureRecognizer(..)
  , buttonMask
  , setButtonMask
  , numberOfClicksRequired
  , setNumberOfClicksRequired
  , numberOfTouchesRequired
  , setNumberOfTouchesRequired
  , buttonMaskSelector
  , numberOfClicksRequiredSelector
  , numberOfTouchesRequiredSelector
  , setButtonMaskSelector
  , setNumberOfClicksRequiredSelector
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
buttonMask :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> IO CULong
buttonMask nsClickGestureRecognizer =
  sendMessage nsClickGestureRecognizer buttonMaskSelector

-- | @- setButtonMask:@
setButtonMask :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> CULong -> IO ()
setButtonMask nsClickGestureRecognizer value =
  sendMessage nsClickGestureRecognizer setButtonMaskSelector value

-- | @- numberOfClicksRequired@
numberOfClicksRequired :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> IO CLong
numberOfClicksRequired nsClickGestureRecognizer =
  sendMessage nsClickGestureRecognizer numberOfClicksRequiredSelector

-- | @- setNumberOfClicksRequired:@
setNumberOfClicksRequired :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> CLong -> IO ()
setNumberOfClicksRequired nsClickGestureRecognizer value =
  sendMessage nsClickGestureRecognizer setNumberOfClicksRequiredSelector value

-- | @- numberOfTouchesRequired@
numberOfTouchesRequired :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> IO CLong
numberOfTouchesRequired nsClickGestureRecognizer =
  sendMessage nsClickGestureRecognizer numberOfTouchesRequiredSelector

-- | @- setNumberOfTouchesRequired:@
setNumberOfTouchesRequired :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> CLong -> IO ()
setNumberOfTouchesRequired nsClickGestureRecognizer value =
  sendMessage nsClickGestureRecognizer setNumberOfTouchesRequiredSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonMask@
buttonMaskSelector :: Selector '[] CULong
buttonMaskSelector = mkSelector "buttonMask"

-- | @Selector@ for @setButtonMask:@
setButtonMaskSelector :: Selector '[CULong] ()
setButtonMaskSelector = mkSelector "setButtonMask:"

-- | @Selector@ for @numberOfClicksRequired@
numberOfClicksRequiredSelector :: Selector '[] CLong
numberOfClicksRequiredSelector = mkSelector "numberOfClicksRequired"

-- | @Selector@ for @setNumberOfClicksRequired:@
setNumberOfClicksRequiredSelector :: Selector '[CLong] ()
setNumberOfClicksRequiredSelector = mkSelector "setNumberOfClicksRequired:"

-- | @Selector@ for @numberOfTouchesRequired@
numberOfTouchesRequiredSelector :: Selector '[] CLong
numberOfTouchesRequiredSelector = mkSelector "numberOfTouchesRequired"

-- | @Selector@ for @setNumberOfTouchesRequired:@
setNumberOfTouchesRequiredSelector :: Selector '[CLong] ()
setNumberOfTouchesRequiredSelector = mkSelector "setNumberOfTouchesRequired:"

