{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMMouseEvent@.
module ObjC.WebKit.DOMMouseEvent
  ( DOMMouseEvent
  , IsDOMMouseEvent(..)
  , initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTarget
  , initMouseEvent
  , screenX
  , screenY
  , clientX
  , clientY
  , ctrlKey
  , shiftKey
  , altKey
  , metaKey
  , button
  , relatedTarget
  , offsetX
  , offsetY
  , x
  , y
  , fromElement
  , toElement
  , altKeySelector
  , buttonSelector
  , clientXSelector
  , clientYSelector
  , ctrlKeySelector
  , fromElementSelector
  , initMouseEventSelector
  , initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTargetSelector
  , metaKeySelector
  , offsetXSelector
  , offsetYSelector
  , relatedTargetSelector
  , screenXSelector
  , screenYSelector
  , shiftKeySelector
  , toElementSelector
  , xSelector
  , ySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initMouseEvent:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:relatedTarget:@
initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTarget :: (IsDOMMouseEvent domMouseEvent, IsNSString type_, IsDOMAbstractView view) => domMouseEvent -> type_ -> Bool -> Bool -> view -> CInt -> CInt -> CInt -> CInt -> CInt -> Bool -> Bool -> Bool -> Bool -> CUShort -> RawId -> IO ()
initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTarget domMouseEvent type_ canBubble cancelable view detail screenX screenY clientX clientY ctrlKey altKey shiftKey metaKey button relatedTarget =
  sendOwnedMessage domMouseEvent initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTargetSelector (toNSString type_) canBubble cancelable (toDOMAbstractView view) detail screenX screenY clientX clientY ctrlKey altKey shiftKey metaKey button relatedTarget

-- | @- initMouseEvent:::::::::::::::@
initMouseEvent :: (IsDOMMouseEvent domMouseEvent, IsNSString type_, IsDOMAbstractView view) => domMouseEvent -> type_ -> Bool -> Bool -> view -> CInt -> CInt -> CInt -> CInt -> CInt -> Bool -> Bool -> Bool -> Bool -> CUShort -> RawId -> IO ()
initMouseEvent domMouseEvent type_ canBubble cancelable view detail screenX screenY clientX clientY ctrlKey altKey shiftKey metaKey button relatedTarget =
  sendOwnedMessage domMouseEvent initMouseEventSelector (toNSString type_) canBubble cancelable (toDOMAbstractView view) detail screenX screenY clientX clientY ctrlKey altKey shiftKey metaKey button relatedTarget

-- | @- screenX@
screenX :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
screenX domMouseEvent =
  sendMessage domMouseEvent screenXSelector

-- | @- screenY@
screenY :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
screenY domMouseEvent =
  sendMessage domMouseEvent screenYSelector

-- | @- clientX@
clientX :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
clientX domMouseEvent =
  sendMessage domMouseEvent clientXSelector

-- | @- clientY@
clientY :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
clientY domMouseEvent =
  sendMessage domMouseEvent clientYSelector

-- | @- ctrlKey@
ctrlKey :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO Bool
ctrlKey domMouseEvent =
  sendMessage domMouseEvent ctrlKeySelector

-- | @- shiftKey@
shiftKey :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO Bool
shiftKey domMouseEvent =
  sendMessage domMouseEvent shiftKeySelector

-- | @- altKey@
altKey :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO Bool
altKey domMouseEvent =
  sendMessage domMouseEvent altKeySelector

-- | @- metaKey@
metaKey :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO Bool
metaKey domMouseEvent =
  sendMessage domMouseEvent metaKeySelector

-- | @- button@
button :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CShort
button domMouseEvent =
  sendMessage domMouseEvent buttonSelector

-- | @- relatedTarget@
relatedTarget :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO RawId
relatedTarget domMouseEvent =
  sendMessage domMouseEvent relatedTargetSelector

-- | @- offsetX@
offsetX :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
offsetX domMouseEvent =
  sendMessage domMouseEvent offsetXSelector

-- | @- offsetY@
offsetY :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
offsetY domMouseEvent =
  sendMessage domMouseEvent offsetYSelector

-- | @- x@
x :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
x domMouseEvent =
  sendMessage domMouseEvent xSelector

-- | @- y@
y :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
y domMouseEvent =
  sendMessage domMouseEvent ySelector

-- | @- fromElement@
fromElement :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO (Id DOMNode)
fromElement domMouseEvent =
  sendMessage domMouseEvent fromElementSelector

-- | @- toElement@
toElement :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO (Id DOMNode)
toElement domMouseEvent =
  sendMessage domMouseEvent toElementSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initMouseEvent:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:relatedTarget:@
initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTargetSelector :: Selector '[Id NSString, Bool, Bool, Id DOMAbstractView, CInt, CInt, CInt, CInt, CInt, Bool, Bool, Bool, Bool, CUShort, RawId] ()
initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTargetSelector = mkSelector "initMouseEvent:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:relatedTarget:"

-- | @Selector@ for @initMouseEvent:::::::::::::::@
initMouseEventSelector :: Selector '[Id NSString, Bool, Bool, Id DOMAbstractView, CInt, CInt, CInt, CInt, CInt, Bool, Bool, Bool, Bool, CUShort, RawId] ()
initMouseEventSelector = mkSelector "initMouseEvent:::::::::::::::"

-- | @Selector@ for @screenX@
screenXSelector :: Selector '[] CInt
screenXSelector = mkSelector "screenX"

-- | @Selector@ for @screenY@
screenYSelector :: Selector '[] CInt
screenYSelector = mkSelector "screenY"

-- | @Selector@ for @clientX@
clientXSelector :: Selector '[] CInt
clientXSelector = mkSelector "clientX"

-- | @Selector@ for @clientY@
clientYSelector :: Selector '[] CInt
clientYSelector = mkSelector "clientY"

-- | @Selector@ for @ctrlKey@
ctrlKeySelector :: Selector '[] Bool
ctrlKeySelector = mkSelector "ctrlKey"

-- | @Selector@ for @shiftKey@
shiftKeySelector :: Selector '[] Bool
shiftKeySelector = mkSelector "shiftKey"

-- | @Selector@ for @altKey@
altKeySelector :: Selector '[] Bool
altKeySelector = mkSelector "altKey"

-- | @Selector@ for @metaKey@
metaKeySelector :: Selector '[] Bool
metaKeySelector = mkSelector "metaKey"

-- | @Selector@ for @button@
buttonSelector :: Selector '[] CShort
buttonSelector = mkSelector "button"

-- | @Selector@ for @relatedTarget@
relatedTargetSelector :: Selector '[] RawId
relatedTargetSelector = mkSelector "relatedTarget"

-- | @Selector@ for @offsetX@
offsetXSelector :: Selector '[] CInt
offsetXSelector = mkSelector "offsetX"

-- | @Selector@ for @offsetY@
offsetYSelector :: Selector '[] CInt
offsetYSelector = mkSelector "offsetY"

-- | @Selector@ for @x@
xSelector :: Selector '[] CInt
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector '[] CInt
ySelector = mkSelector "y"

-- | @Selector@ for @fromElement@
fromElementSelector :: Selector '[] (Id DOMNode)
fromElementSelector = mkSelector "fromElement"

-- | @Selector@ for @toElement@
toElementSelector :: Selector '[] (Id DOMNode)
toElementSelector = mkSelector "toElement"

