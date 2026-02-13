{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMUIEvent@.
module ObjC.WebKit.DOMUIEvent
  ( DOMUIEvent
  , IsDOMUIEvent(..)
  , initUIEvent_canBubble_cancelable_view_detail
  , initUIEvent
  , view
  , detail
  , keyCode
  , charCode
  , layerX
  , layerY
  , pageX
  , pageY
  , which
  , charCodeSelector
  , detailSelector
  , initUIEventSelector
  , initUIEvent_canBubble_cancelable_view_detailSelector
  , keyCodeSelector
  , layerXSelector
  , layerYSelector
  , pageXSelector
  , pageYSelector
  , viewSelector
  , whichSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initUIEvent:canBubble:cancelable:view:detail:@
initUIEvent_canBubble_cancelable_view_detail :: (IsDOMUIEvent domuiEvent, IsNSString type_, IsDOMAbstractView view) => domuiEvent -> type_ -> Bool -> Bool -> view -> CInt -> IO ()
initUIEvent_canBubble_cancelable_view_detail domuiEvent type_ canBubble cancelable view detail =
  sendOwnedMessage domuiEvent initUIEvent_canBubble_cancelable_view_detailSelector (toNSString type_) canBubble cancelable (toDOMAbstractView view) detail

-- | @- initUIEvent:::::@
initUIEvent :: (IsDOMUIEvent domuiEvent, IsNSString type_, IsDOMAbstractView view) => domuiEvent -> type_ -> Bool -> Bool -> view -> CInt -> IO ()
initUIEvent domuiEvent type_ canBubble cancelable view detail =
  sendOwnedMessage domuiEvent initUIEventSelector (toNSString type_) canBubble cancelable (toDOMAbstractView view) detail

-- | @- view@
view :: IsDOMUIEvent domuiEvent => domuiEvent -> IO (Id DOMAbstractView)
view domuiEvent =
  sendMessage domuiEvent viewSelector

-- | @- detail@
detail :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
detail domuiEvent =
  sendMessage domuiEvent detailSelector

-- | @- keyCode@
keyCode :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
keyCode domuiEvent =
  sendMessage domuiEvent keyCodeSelector

-- | @- charCode@
charCode :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
charCode domuiEvent =
  sendMessage domuiEvent charCodeSelector

-- | @- layerX@
layerX :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
layerX domuiEvent =
  sendMessage domuiEvent layerXSelector

-- | @- layerY@
layerY :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
layerY domuiEvent =
  sendMessage domuiEvent layerYSelector

-- | @- pageX@
pageX :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
pageX domuiEvent =
  sendMessage domuiEvent pageXSelector

-- | @- pageY@
pageY :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
pageY domuiEvent =
  sendMessage domuiEvent pageYSelector

-- | @- which@
which :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
which domuiEvent =
  sendMessage domuiEvent whichSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initUIEvent:canBubble:cancelable:view:detail:@
initUIEvent_canBubble_cancelable_view_detailSelector :: Selector '[Id NSString, Bool, Bool, Id DOMAbstractView, CInt] ()
initUIEvent_canBubble_cancelable_view_detailSelector = mkSelector "initUIEvent:canBubble:cancelable:view:detail:"

-- | @Selector@ for @initUIEvent:::::@
initUIEventSelector :: Selector '[Id NSString, Bool, Bool, Id DOMAbstractView, CInt] ()
initUIEventSelector = mkSelector "initUIEvent:::::"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id DOMAbstractView)
viewSelector = mkSelector "view"

-- | @Selector@ for @detail@
detailSelector :: Selector '[] CInt
detailSelector = mkSelector "detail"

-- | @Selector@ for @keyCode@
keyCodeSelector :: Selector '[] CInt
keyCodeSelector = mkSelector "keyCode"

-- | @Selector@ for @charCode@
charCodeSelector :: Selector '[] CInt
charCodeSelector = mkSelector "charCode"

-- | @Selector@ for @layerX@
layerXSelector :: Selector '[] CInt
layerXSelector = mkSelector "layerX"

-- | @Selector@ for @layerY@
layerYSelector :: Selector '[] CInt
layerYSelector = mkSelector "layerY"

-- | @Selector@ for @pageX@
pageXSelector :: Selector '[] CInt
pageXSelector = mkSelector "pageX"

-- | @Selector@ for @pageY@
pageYSelector :: Selector '[] CInt
pageYSelector = mkSelector "pageY"

-- | @Selector@ for @which@
whichSelector :: Selector '[] CInt
whichSelector = mkSelector "which"

