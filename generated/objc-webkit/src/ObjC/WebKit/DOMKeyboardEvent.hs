{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMKeyboardEvent@.
module ObjC.WebKit.DOMKeyboardEvent
  ( DOMKeyboardEvent
  , IsDOMKeyboardEvent(..)
  , getModifierState
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKey
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKey
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey
  , keyIdentifier
  , location
  , keyLocation
  , ctrlKey
  , shiftKey
  , altKey
  , metaKey
  , altGraphKey
  , keyCode
  , charCode
  , getModifierStateSelector
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKeySelector
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKeySelector
  , keyIdentifierSelector
  , locationSelector
  , keyLocationSelector
  , ctrlKeySelector
  , shiftKeySelector
  , altKeySelector
  , metaKeySelector
  , altGraphKeySelector
  , keyCodeSelector
  , charCodeSelector


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

-- | @- getModifierState:@
getModifierState :: (IsDOMKeyboardEvent domKeyboardEvent, IsNSString keyIdentifierArg) => domKeyboardEvent -> keyIdentifierArg -> IO Bool
getModifierState domKeyboardEvent  keyIdentifierArg =
withObjCPtr keyIdentifierArg $ \raw_keyIdentifierArg ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domKeyboardEvent (mkSelector "getModifierState:") retCULong [argPtr (castPtr raw_keyIdentifierArg :: Ptr ())]

-- | @- initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKey :: (IsDOMKeyboardEvent domKeyboardEvent, IsNSString type_, IsDOMAbstractView view, IsNSString keyIdentifier) => domKeyboardEvent -> type_ -> Bool -> Bool -> view -> keyIdentifier -> CUInt -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKey domKeyboardEvent  type_ canBubble cancelable view keyIdentifier location ctrlKey altKey shiftKey metaKey altGraphKey =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr view $ \raw_view ->
    withObjCPtr keyIdentifier $ \raw_keyIdentifier ->
        sendMsg domKeyboardEvent (mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if canBubble then 1 else 0), argCULong (if cancelable then 1 else 0), argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr raw_keyIdentifier :: Ptr ()), argCUInt (fromIntegral location), argCULong (if ctrlKey then 1 else 0), argCULong (if altKey then 1 else 0), argCULong (if shiftKey then 1 else 0), argCULong (if metaKey then 1 else 0), argCULong (if altGraphKey then 1 else 0)]

-- | @- initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey :: (IsDOMKeyboardEvent domKeyboardEvent, IsNSString type_, IsDOMAbstractView view, IsNSString keyIdentifier) => domKeyboardEvent -> type_ -> Bool -> Bool -> view -> keyIdentifier -> CUInt -> Bool -> Bool -> Bool -> Bool -> IO ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey domKeyboardEvent  type_ canBubble cancelable view keyIdentifier location ctrlKey altKey shiftKey metaKey =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr view $ \raw_view ->
    withObjCPtr keyIdentifier $ \raw_keyIdentifier ->
        sendMsg domKeyboardEvent (mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if canBubble then 1 else 0), argCULong (if cancelable then 1 else 0), argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr raw_keyIdentifier :: Ptr ()), argCUInt (fromIntegral location), argCULong (if ctrlKey then 1 else 0), argCULong (if altKey then 1 else 0), argCULong (if shiftKey then 1 else 0), argCULong (if metaKey then 1 else 0)]

-- | @- initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKey :: (IsDOMKeyboardEvent domKeyboardEvent, IsNSString type_, IsDOMAbstractView view, IsNSString keyIdentifier) => domKeyboardEvent -> type_ -> Bool -> Bool -> view -> keyIdentifier -> CUInt -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKey domKeyboardEvent  type_ canBubble cancelable view keyIdentifier keyLocation ctrlKey altKey shiftKey metaKey altGraphKey =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr view $ \raw_view ->
    withObjCPtr keyIdentifier $ \raw_keyIdentifier ->
        sendMsg domKeyboardEvent (mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if canBubble then 1 else 0), argCULong (if cancelable then 1 else 0), argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr raw_keyIdentifier :: Ptr ()), argCUInt (fromIntegral keyLocation), argCULong (if ctrlKey then 1 else 0), argCULong (if altKey then 1 else 0), argCULong (if shiftKey then 1 else 0), argCULong (if metaKey then 1 else 0), argCULong (if altGraphKey then 1 else 0)]

-- | @- initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey :: (IsDOMKeyboardEvent domKeyboardEvent, IsNSString type_, IsDOMAbstractView view, IsNSString keyIdentifier) => domKeyboardEvent -> type_ -> Bool -> Bool -> view -> keyIdentifier -> CUInt -> Bool -> Bool -> Bool -> Bool -> IO ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey domKeyboardEvent  type_ canBubble cancelable view keyIdentifier keyLocation ctrlKey altKey shiftKey metaKey =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr view $ \raw_view ->
    withObjCPtr keyIdentifier $ \raw_keyIdentifier ->
        sendMsg domKeyboardEvent (mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if canBubble then 1 else 0), argCULong (if cancelable then 1 else 0), argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr raw_keyIdentifier :: Ptr ()), argCUInt (fromIntegral keyLocation), argCULong (if ctrlKey then 1 else 0), argCULong (if altKey then 1 else 0), argCULong (if shiftKey then 1 else 0), argCULong (if metaKey then 1 else 0)]

-- | @- keyIdentifier@
keyIdentifier :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO (Id NSString)
keyIdentifier domKeyboardEvent  =
  sendMsg domKeyboardEvent (mkSelector "keyIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- location@
location :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO CUInt
location domKeyboardEvent  =
  sendMsg domKeyboardEvent (mkSelector "location") retCUInt []

-- | @- keyLocation@
keyLocation :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO CUInt
keyLocation domKeyboardEvent  =
  sendMsg domKeyboardEvent (mkSelector "keyLocation") retCUInt []

-- | @- ctrlKey@
ctrlKey :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO Bool
ctrlKey domKeyboardEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domKeyboardEvent (mkSelector "ctrlKey") retCULong []

-- | @- shiftKey@
shiftKey :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO Bool
shiftKey domKeyboardEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domKeyboardEvent (mkSelector "shiftKey") retCULong []

-- | @- altKey@
altKey :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO Bool
altKey domKeyboardEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domKeyboardEvent (mkSelector "altKey") retCULong []

-- | @- metaKey@
metaKey :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO Bool
metaKey domKeyboardEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domKeyboardEvent (mkSelector "metaKey") retCULong []

-- | @- altGraphKey@
altGraphKey :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO Bool
altGraphKey domKeyboardEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domKeyboardEvent (mkSelector "altGraphKey") retCULong []

-- | @- keyCode@
keyCode :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO CInt
keyCode domKeyboardEvent  =
  sendMsg domKeyboardEvent (mkSelector "keyCode") retCInt []

-- | @- charCode@
charCode :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO CInt
charCode domKeyboardEvent  =
  sendMsg domKeyboardEvent (mkSelector "charCode") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getModifierState:@
getModifierStateSelector :: Selector
getModifierStateSelector = mkSelector "getModifierState:"

-- | @Selector@ for @initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector :: Selector
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector = mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:"

-- | @Selector@ for @initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKeySelector :: Selector
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKeySelector = mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:"

-- | @Selector@ for @initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector :: Selector
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector = mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:"

-- | @Selector@ for @initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKeySelector :: Selector
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKeySelector = mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:"

-- | @Selector@ for @keyIdentifier@
keyIdentifierSelector :: Selector
keyIdentifierSelector = mkSelector "keyIdentifier"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @keyLocation@
keyLocationSelector :: Selector
keyLocationSelector = mkSelector "keyLocation"

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

-- | @Selector@ for @altGraphKey@
altGraphKeySelector :: Selector
altGraphKeySelector = mkSelector "altGraphKey"

-- | @Selector@ for @keyCode@
keyCodeSelector :: Selector
keyCodeSelector = mkSelector "keyCode"

-- | @Selector@ for @charCode@
charCodeSelector :: Selector
charCodeSelector = mkSelector "charCode"

