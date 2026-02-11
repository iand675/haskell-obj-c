{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMWheelEvent@.
module ObjC.WebKit.DOMWheelEvent
  ( DOMWheelEvent
  , IsDOMWheelEvent(..)
  , initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey
  , wheelDeltaX
  , wheelDeltaY
  , wheelDelta
  , isHorizontal
  , initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKeySelector
  , wheelDeltaXSelector
  , wheelDeltaYSelector
  , wheelDeltaSelector
  , isHorizontalSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWheelEvent:wheelDeltaY:view:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:@
initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey :: (IsDOMWheelEvent domWheelEvent, IsDOMAbstractView view) => domWheelEvent -> CInt -> CInt -> view -> CInt -> CInt -> CInt -> CInt -> Bool -> Bool -> Bool -> Bool -> IO ()
initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey domWheelEvent  wheelDeltaX wheelDeltaY view screenX screenY clientX clientY ctrlKey altKey shiftKey metaKey =
withObjCPtr view $ \raw_view ->
    sendMsg domWheelEvent (mkSelector "initWheelEvent:wheelDeltaY:view:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:") retVoid [argCInt (fromIntegral wheelDeltaX), argCInt (fromIntegral wheelDeltaY), argPtr (castPtr raw_view :: Ptr ()), argCInt (fromIntegral screenX), argCInt (fromIntegral screenY), argCInt (fromIntegral clientX), argCInt (fromIntegral clientY), argCULong (if ctrlKey then 1 else 0), argCULong (if altKey then 1 else 0), argCULong (if shiftKey then 1 else 0), argCULong (if metaKey then 1 else 0)]

-- | @- wheelDeltaX@
wheelDeltaX :: IsDOMWheelEvent domWheelEvent => domWheelEvent -> IO CInt
wheelDeltaX domWheelEvent  =
  sendMsg domWheelEvent (mkSelector "wheelDeltaX") retCInt []

-- | @- wheelDeltaY@
wheelDeltaY :: IsDOMWheelEvent domWheelEvent => domWheelEvent -> IO CInt
wheelDeltaY domWheelEvent  =
  sendMsg domWheelEvent (mkSelector "wheelDeltaY") retCInt []

-- | @- wheelDelta@
wheelDelta :: IsDOMWheelEvent domWheelEvent => domWheelEvent -> IO CInt
wheelDelta domWheelEvent  =
  sendMsg domWheelEvent (mkSelector "wheelDelta") retCInt []

-- | @- isHorizontal@
isHorizontal :: IsDOMWheelEvent domWheelEvent => domWheelEvent -> IO Bool
isHorizontal domWheelEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domWheelEvent (mkSelector "isHorizontal") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWheelEvent:wheelDeltaY:view:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:@
initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKeySelector :: Selector
initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKeySelector = mkSelector "initWheelEvent:wheelDeltaY:view:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:"

-- | @Selector@ for @wheelDeltaX@
wheelDeltaXSelector :: Selector
wheelDeltaXSelector = mkSelector "wheelDeltaX"

-- | @Selector@ for @wheelDeltaY@
wheelDeltaYSelector :: Selector
wheelDeltaYSelector = mkSelector "wheelDeltaY"

-- | @Selector@ for @wheelDelta@
wheelDeltaSelector :: Selector
wheelDeltaSelector = mkSelector "wheelDelta"

-- | @Selector@ for @isHorizontal@
isHorizontalSelector :: Selector
isHorizontalSelector = mkSelector "isHorizontal"

