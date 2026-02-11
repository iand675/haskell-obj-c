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
  , initUIEvent_canBubble_cancelable_view_detailSelector
  , initUIEventSelector
  , viewSelector
  , detailSelector
  , keyCodeSelector
  , charCodeSelector
  , layerXSelector
  , layerYSelector
  , pageXSelector
  , pageYSelector
  , whichSelector


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

-- | @- initUIEvent:canBubble:cancelable:view:detail:@
initUIEvent_canBubble_cancelable_view_detail :: (IsDOMUIEvent domuiEvent, IsNSString type_, IsDOMAbstractView view) => domuiEvent -> type_ -> Bool -> Bool -> view -> CInt -> IO ()
initUIEvent_canBubble_cancelable_view_detail domuiEvent  type_ canBubble cancelable view detail =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr view $ \raw_view ->
      sendMsg domuiEvent (mkSelector "initUIEvent:canBubble:cancelable:view:detail:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if canBubble then 1 else 0), argCULong (if cancelable then 1 else 0), argPtr (castPtr raw_view :: Ptr ()), argCInt (fromIntegral detail)]

-- | @- initUIEvent:::::@
initUIEvent :: (IsDOMUIEvent domuiEvent, IsNSString type_, IsDOMAbstractView view) => domuiEvent -> type_ -> Bool -> Bool -> view -> CInt -> IO ()
initUIEvent domuiEvent  type_ canBubble cancelable view detail =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr view $ \raw_view ->
      sendMsg domuiEvent (mkSelector "initUIEvent:::::") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if canBubble then 1 else 0), argCULong (if cancelable then 1 else 0), argPtr (castPtr raw_view :: Ptr ()), argCInt (fromIntegral detail)]

-- | @- view@
view :: IsDOMUIEvent domuiEvent => domuiEvent -> IO (Id DOMAbstractView)
view domuiEvent  =
  sendMsg domuiEvent (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- detail@
detail :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
detail domuiEvent  =
  sendMsg domuiEvent (mkSelector "detail") retCInt []

-- | @- keyCode@
keyCode :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
keyCode domuiEvent  =
  sendMsg domuiEvent (mkSelector "keyCode") retCInt []

-- | @- charCode@
charCode :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
charCode domuiEvent  =
  sendMsg domuiEvent (mkSelector "charCode") retCInt []

-- | @- layerX@
layerX :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
layerX domuiEvent  =
  sendMsg domuiEvent (mkSelector "layerX") retCInt []

-- | @- layerY@
layerY :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
layerY domuiEvent  =
  sendMsg domuiEvent (mkSelector "layerY") retCInt []

-- | @- pageX@
pageX :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
pageX domuiEvent  =
  sendMsg domuiEvent (mkSelector "pageX") retCInt []

-- | @- pageY@
pageY :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
pageY domuiEvent  =
  sendMsg domuiEvent (mkSelector "pageY") retCInt []

-- | @- which@
which :: IsDOMUIEvent domuiEvent => domuiEvent -> IO CInt
which domuiEvent  =
  sendMsg domuiEvent (mkSelector "which") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initUIEvent:canBubble:cancelable:view:detail:@
initUIEvent_canBubble_cancelable_view_detailSelector :: Selector
initUIEvent_canBubble_cancelable_view_detailSelector = mkSelector "initUIEvent:canBubble:cancelable:view:detail:"

-- | @Selector@ for @initUIEvent:::::@
initUIEventSelector :: Selector
initUIEventSelector = mkSelector "initUIEvent:::::"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @detail@
detailSelector :: Selector
detailSelector = mkSelector "detail"

-- | @Selector@ for @keyCode@
keyCodeSelector :: Selector
keyCodeSelector = mkSelector "keyCode"

-- | @Selector@ for @charCode@
charCodeSelector :: Selector
charCodeSelector = mkSelector "charCode"

-- | @Selector@ for @layerX@
layerXSelector :: Selector
layerXSelector = mkSelector "layerX"

-- | @Selector@ for @layerY@
layerYSelector :: Selector
layerYSelector = mkSelector "layerY"

-- | @Selector@ for @pageX@
pageXSelector :: Selector
pageXSelector = mkSelector "pageX"

-- | @Selector@ for @pageY@
pageYSelector :: Selector
pageYSelector = mkSelector "pageY"

-- | @Selector@ for @which@
whichSelector :: Selector
whichSelector = mkSelector "which"

