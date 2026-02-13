{-# LANGUAGE DataKinds #-}
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
  , isHorizontalSelector
  , wheelDeltaSelector
  , wheelDeltaXSelector
  , wheelDeltaYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWheelEvent:wheelDeltaY:view:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:@
initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey :: (IsDOMWheelEvent domWheelEvent, IsDOMAbstractView view) => domWheelEvent -> CInt -> CInt -> view -> CInt -> CInt -> CInt -> CInt -> Bool -> Bool -> Bool -> Bool -> IO ()
initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey domWheelEvent wheelDeltaX wheelDeltaY view screenX screenY clientX clientY ctrlKey altKey shiftKey metaKey =
  sendOwnedMessage domWheelEvent initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKeySelector wheelDeltaX wheelDeltaY (toDOMAbstractView view) screenX screenY clientX clientY ctrlKey altKey shiftKey metaKey

-- | @- wheelDeltaX@
wheelDeltaX :: IsDOMWheelEvent domWheelEvent => domWheelEvent -> IO CInt
wheelDeltaX domWheelEvent =
  sendMessage domWheelEvent wheelDeltaXSelector

-- | @- wheelDeltaY@
wheelDeltaY :: IsDOMWheelEvent domWheelEvent => domWheelEvent -> IO CInt
wheelDeltaY domWheelEvent =
  sendMessage domWheelEvent wheelDeltaYSelector

-- | @- wheelDelta@
wheelDelta :: IsDOMWheelEvent domWheelEvent => domWheelEvent -> IO CInt
wheelDelta domWheelEvent =
  sendMessage domWheelEvent wheelDeltaSelector

-- | @- isHorizontal@
isHorizontal :: IsDOMWheelEvent domWheelEvent => domWheelEvent -> IO Bool
isHorizontal domWheelEvent =
  sendMessage domWheelEvent isHorizontalSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWheelEvent:wheelDeltaY:view:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:@
initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKeySelector :: Selector '[CInt, CInt, Id DOMAbstractView, CInt, CInt, CInt, CInt, Bool, Bool, Bool, Bool] ()
initWheelEvent_wheelDeltaY_view_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKeySelector = mkSelector "initWheelEvent:wheelDeltaY:view:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:"

-- | @Selector@ for @wheelDeltaX@
wheelDeltaXSelector :: Selector '[] CInt
wheelDeltaXSelector = mkSelector "wheelDeltaX"

-- | @Selector@ for @wheelDeltaY@
wheelDeltaYSelector :: Selector '[] CInt
wheelDeltaYSelector = mkSelector "wheelDeltaY"

-- | @Selector@ for @wheelDelta@
wheelDeltaSelector :: Selector '[] CInt
wheelDeltaSelector = mkSelector "wheelDelta"

-- | @Selector@ for @isHorizontal@
isHorizontalSelector :: Selector '[] Bool
isHorizontalSelector = mkSelector "isHorizontal"

