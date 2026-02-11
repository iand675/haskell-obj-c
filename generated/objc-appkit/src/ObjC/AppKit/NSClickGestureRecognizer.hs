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
  , setButtonMaskSelector
  , numberOfClicksRequiredSelector
  , setNumberOfClicksRequiredSelector
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
buttonMask :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> IO CULong
buttonMask nsClickGestureRecognizer  =
  sendMsg nsClickGestureRecognizer (mkSelector "buttonMask") retCULong []

-- | @- setButtonMask:@
setButtonMask :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> CULong -> IO ()
setButtonMask nsClickGestureRecognizer  value =
  sendMsg nsClickGestureRecognizer (mkSelector "setButtonMask:") retVoid [argCULong (fromIntegral value)]

-- | @- numberOfClicksRequired@
numberOfClicksRequired :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> IO CLong
numberOfClicksRequired nsClickGestureRecognizer  =
  sendMsg nsClickGestureRecognizer (mkSelector "numberOfClicksRequired") retCLong []

-- | @- setNumberOfClicksRequired:@
setNumberOfClicksRequired :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> CLong -> IO ()
setNumberOfClicksRequired nsClickGestureRecognizer  value =
  sendMsg nsClickGestureRecognizer (mkSelector "setNumberOfClicksRequired:") retVoid [argCLong (fromIntegral value)]

-- | @- numberOfTouchesRequired@
numberOfTouchesRequired :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> IO CLong
numberOfTouchesRequired nsClickGestureRecognizer  =
  sendMsg nsClickGestureRecognizer (mkSelector "numberOfTouchesRequired") retCLong []

-- | @- setNumberOfTouchesRequired:@
setNumberOfTouchesRequired :: IsNSClickGestureRecognizer nsClickGestureRecognizer => nsClickGestureRecognizer -> CLong -> IO ()
setNumberOfTouchesRequired nsClickGestureRecognizer  value =
  sendMsg nsClickGestureRecognizer (mkSelector "setNumberOfTouchesRequired:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonMask@
buttonMaskSelector :: Selector
buttonMaskSelector = mkSelector "buttonMask"

-- | @Selector@ for @setButtonMask:@
setButtonMaskSelector :: Selector
setButtonMaskSelector = mkSelector "setButtonMask:"

-- | @Selector@ for @numberOfClicksRequired@
numberOfClicksRequiredSelector :: Selector
numberOfClicksRequiredSelector = mkSelector "numberOfClicksRequired"

-- | @Selector@ for @setNumberOfClicksRequired:@
setNumberOfClicksRequiredSelector :: Selector
setNumberOfClicksRequiredSelector = mkSelector "setNumberOfClicksRequired:"

-- | @Selector@ for @numberOfTouchesRequired@
numberOfTouchesRequiredSelector :: Selector
numberOfTouchesRequiredSelector = mkSelector "numberOfTouchesRequired"

-- | @Selector@ for @setNumberOfTouchesRequired:@
setNumberOfTouchesRequiredSelector :: Selector
setNumberOfTouchesRequiredSelector = mkSelector "setNumberOfTouchesRequired:"

