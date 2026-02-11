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
  , offsetX
  , offsetY
  , x
  , y
  , fromElement
  , toElement
  , initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTargetSelector
  , initMouseEventSelector
  , screenXSelector
  , screenYSelector
  , clientXSelector
  , clientYSelector
  , ctrlKeySelector
  , shiftKeySelector
  , altKeySelector
  , metaKeySelector
  , buttonSelector
  , offsetXSelector
  , offsetYSelector
  , xSelector
  , ySelector
  , fromElementSelector
  , toElementSelector


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

-- | @- initMouseEvent:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:relatedTarget:@
initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTarget :: (IsDOMMouseEvent domMouseEvent, IsNSString type_, IsDOMAbstractView view) => domMouseEvent -> type_ -> Bool -> Bool -> view -> CInt -> CInt -> CInt -> CInt -> CInt -> Bool -> Bool -> Bool -> Bool -> CUShort -> RawId -> IO ()
initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTarget domMouseEvent  type_ canBubble cancelable view detail screenX screenY clientX clientY ctrlKey altKey shiftKey metaKey button relatedTarget =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr view $ \raw_view ->
      sendMsg domMouseEvent (mkSelector "initMouseEvent:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:relatedTarget:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if canBubble then 1 else 0), argCULong (if cancelable then 1 else 0), argPtr (castPtr raw_view :: Ptr ()), argCInt (fromIntegral detail), argCInt (fromIntegral screenX), argCInt (fromIntegral screenY), argCInt (fromIntegral clientX), argCInt (fromIntegral clientY), argCULong (if ctrlKey then 1 else 0), argCULong (if altKey then 1 else 0), argCULong (if shiftKey then 1 else 0), argCULong (if metaKey then 1 else 0), argCUInt (fromIntegral button), argPtr (castPtr (unRawId relatedTarget) :: Ptr ())]

-- | @- initMouseEvent:::::::::::::::@
initMouseEvent :: (IsDOMMouseEvent domMouseEvent, IsNSString type_, IsDOMAbstractView view) => domMouseEvent -> type_ -> Bool -> Bool -> view -> CInt -> CInt -> CInt -> CInt -> CInt -> Bool -> Bool -> Bool -> Bool -> CUShort -> RawId -> IO ()
initMouseEvent domMouseEvent  type_ canBubble cancelable view detail screenX screenY clientX clientY ctrlKey altKey shiftKey metaKey button relatedTarget =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr view $ \raw_view ->
      sendMsg domMouseEvent (mkSelector "initMouseEvent:::::::::::::::") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if canBubble then 1 else 0), argCULong (if cancelable then 1 else 0), argPtr (castPtr raw_view :: Ptr ()), argCInt (fromIntegral detail), argCInt (fromIntegral screenX), argCInt (fromIntegral screenY), argCInt (fromIntegral clientX), argCInt (fromIntegral clientY), argCULong (if ctrlKey then 1 else 0), argCULong (if altKey then 1 else 0), argCULong (if shiftKey then 1 else 0), argCULong (if metaKey then 1 else 0), argCUInt (fromIntegral button), argPtr (castPtr (unRawId relatedTarget) :: Ptr ())]

-- | @- screenX@
screenX :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
screenX domMouseEvent  =
  sendMsg domMouseEvent (mkSelector "screenX") retCInt []

-- | @- screenY@
screenY :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
screenY domMouseEvent  =
  sendMsg domMouseEvent (mkSelector "screenY") retCInt []

-- | @- clientX@
clientX :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
clientX domMouseEvent  =
  sendMsg domMouseEvent (mkSelector "clientX") retCInt []

-- | @- clientY@
clientY :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
clientY domMouseEvent  =
  sendMsg domMouseEvent (mkSelector "clientY") retCInt []

-- | @- ctrlKey@
ctrlKey :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO Bool
ctrlKey domMouseEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domMouseEvent (mkSelector "ctrlKey") retCULong []

-- | @- shiftKey@
shiftKey :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO Bool
shiftKey domMouseEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domMouseEvent (mkSelector "shiftKey") retCULong []

-- | @- altKey@
altKey :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO Bool
altKey domMouseEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domMouseEvent (mkSelector "altKey") retCULong []

-- | @- metaKey@
metaKey :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO Bool
metaKey domMouseEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domMouseEvent (mkSelector "metaKey") retCULong []

-- | @- button@
button :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CShort
button domMouseEvent  =
  fmap fromIntegral $ sendMsg domMouseEvent (mkSelector "button") retCInt []

-- | @- offsetX@
offsetX :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
offsetX domMouseEvent  =
  sendMsg domMouseEvent (mkSelector "offsetX") retCInt []

-- | @- offsetY@
offsetY :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
offsetY domMouseEvent  =
  sendMsg domMouseEvent (mkSelector "offsetY") retCInt []

-- | @- x@
x :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
x domMouseEvent  =
  sendMsg domMouseEvent (mkSelector "x") retCInt []

-- | @- y@
y :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO CInt
y domMouseEvent  =
  sendMsg domMouseEvent (mkSelector "y") retCInt []

-- | @- fromElement@
fromElement :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO (Id DOMNode)
fromElement domMouseEvent  =
  sendMsg domMouseEvent (mkSelector "fromElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- toElement@
toElement :: IsDOMMouseEvent domMouseEvent => domMouseEvent -> IO (Id DOMNode)
toElement domMouseEvent  =
  sendMsg domMouseEvent (mkSelector "toElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initMouseEvent:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:relatedTarget:@
initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTargetSelector :: Selector
initMouseEvent_canBubble_cancelable_view_detail_screenX_screenY_clientX_clientY_ctrlKey_altKey_shiftKey_metaKey_button_relatedTargetSelector = mkSelector "initMouseEvent:canBubble:cancelable:view:detail:screenX:screenY:clientX:clientY:ctrlKey:altKey:shiftKey:metaKey:button:relatedTarget:"

-- | @Selector@ for @initMouseEvent:::::::::::::::@
initMouseEventSelector :: Selector
initMouseEventSelector = mkSelector "initMouseEvent:::::::::::::::"

-- | @Selector@ for @screenX@
screenXSelector :: Selector
screenXSelector = mkSelector "screenX"

-- | @Selector@ for @screenY@
screenYSelector :: Selector
screenYSelector = mkSelector "screenY"

-- | @Selector@ for @clientX@
clientXSelector :: Selector
clientXSelector = mkSelector "clientX"

-- | @Selector@ for @clientY@
clientYSelector :: Selector
clientYSelector = mkSelector "clientY"

-- | @Selector@ for @ctrlKey@
ctrlKeySelector :: Selector
ctrlKeySelector = mkSelector "ctrlKey"

-- | @Selector@ for @shiftKey@
shiftKeySelector :: Selector
shiftKeySelector = mkSelector "shiftKey"

-- | @Selector@ for @altKey@
altKeySelector :: Selector
altKeySelector = mkSelector "altKey"

-- | @Selector@ for @metaKey@
metaKeySelector :: Selector
metaKeySelector = mkSelector "metaKey"

-- | @Selector@ for @button@
buttonSelector :: Selector
buttonSelector = mkSelector "button"

-- | @Selector@ for @offsetX@
offsetXSelector :: Selector
offsetXSelector = mkSelector "offsetX"

-- | @Selector@ for @offsetY@
offsetYSelector :: Selector
offsetYSelector = mkSelector "offsetY"

-- | @Selector@ for @x@
xSelector :: Selector
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector
ySelector = mkSelector "y"

-- | @Selector@ for @fromElement@
fromElementSelector :: Selector
fromElementSelector = mkSelector "fromElement"

-- | @Selector@ for @toElement@
toElementSelector :: Selector
toElementSelector = mkSelector "toElement"

