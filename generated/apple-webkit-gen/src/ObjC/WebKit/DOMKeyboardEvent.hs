{-# LANGUAGE DataKinds #-}
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
  , altGraphKeySelector
  , altKeySelector
  , charCodeSelector
  , ctrlKeySelector
  , getModifierStateSelector
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKeySelector
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKeySelector
  , initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector
  , keyCodeSelector
  , keyIdentifierSelector
  , keyLocationSelector
  , locationSelector
  , metaKeySelector
  , shiftKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- getModifierState:@
getModifierState :: (IsDOMKeyboardEvent domKeyboardEvent, IsNSString keyIdentifierArg) => domKeyboardEvent -> keyIdentifierArg -> IO Bool
getModifierState domKeyboardEvent keyIdentifierArg =
  sendMessage domKeyboardEvent getModifierStateSelector (toNSString keyIdentifierArg)

-- | @- initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKey :: (IsDOMKeyboardEvent domKeyboardEvent, IsNSString type_, IsDOMAbstractView view, IsNSString keyIdentifier) => domKeyboardEvent -> type_ -> Bool -> Bool -> view -> keyIdentifier -> CUInt -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKey domKeyboardEvent type_ canBubble cancelable view keyIdentifier location ctrlKey altKey shiftKey metaKey altGraphKey =
  sendOwnedMessage domKeyboardEvent initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector (toNSString type_) canBubble cancelable (toDOMAbstractView view) (toNSString keyIdentifier) location ctrlKey altKey shiftKey metaKey altGraphKey

-- | @- initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey :: (IsDOMKeyboardEvent domKeyboardEvent, IsNSString type_, IsDOMAbstractView view, IsNSString keyIdentifier) => domKeyboardEvent -> type_ -> Bool -> Bool -> view -> keyIdentifier -> CUInt -> Bool -> Bool -> Bool -> Bool -> IO ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey domKeyboardEvent type_ canBubble cancelable view keyIdentifier location ctrlKey altKey shiftKey metaKey =
  sendOwnedMessage domKeyboardEvent initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKeySelector (toNSString type_) canBubble cancelable (toDOMAbstractView view) (toNSString keyIdentifier) location ctrlKey altKey shiftKey metaKey

-- | @- initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKey :: (IsDOMKeyboardEvent domKeyboardEvent, IsNSString type_, IsDOMAbstractView view, IsNSString keyIdentifier) => domKeyboardEvent -> type_ -> Bool -> Bool -> view -> keyIdentifier -> CUInt -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKey domKeyboardEvent type_ canBubble cancelable view keyIdentifier keyLocation ctrlKey altKey shiftKey metaKey altGraphKey =
  sendOwnedMessage domKeyboardEvent initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector (toNSString type_) canBubble cancelable (toDOMAbstractView view) (toNSString keyIdentifier) keyLocation ctrlKey altKey shiftKey metaKey altGraphKey

-- | @- initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey :: (IsDOMKeyboardEvent domKeyboardEvent, IsNSString type_, IsDOMAbstractView view, IsNSString keyIdentifier) => domKeyboardEvent -> type_ -> Bool -> Bool -> view -> keyIdentifier -> CUInt -> Bool -> Bool -> Bool -> Bool -> IO ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey domKeyboardEvent type_ canBubble cancelable view keyIdentifier keyLocation ctrlKey altKey shiftKey metaKey =
  sendOwnedMessage domKeyboardEvent initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKeySelector (toNSString type_) canBubble cancelable (toDOMAbstractView view) (toNSString keyIdentifier) keyLocation ctrlKey altKey shiftKey metaKey

-- | @- keyIdentifier@
keyIdentifier :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO (Id NSString)
keyIdentifier domKeyboardEvent =
  sendMessage domKeyboardEvent keyIdentifierSelector

-- | @- location@
location :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO CUInt
location domKeyboardEvent =
  sendMessage domKeyboardEvent locationSelector

-- | @- keyLocation@
keyLocation :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO CUInt
keyLocation domKeyboardEvent =
  sendMessage domKeyboardEvent keyLocationSelector

-- | @- ctrlKey@
ctrlKey :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO Bool
ctrlKey domKeyboardEvent =
  sendMessage domKeyboardEvent ctrlKeySelector

-- | @- shiftKey@
shiftKey :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO Bool
shiftKey domKeyboardEvent =
  sendMessage domKeyboardEvent shiftKeySelector

-- | @- altKey@
altKey :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO Bool
altKey domKeyboardEvent =
  sendMessage domKeyboardEvent altKeySelector

-- | @- metaKey@
metaKey :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO Bool
metaKey domKeyboardEvent =
  sendMessage domKeyboardEvent metaKeySelector

-- | @- altGraphKey@
altGraphKey :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO Bool
altGraphKey domKeyboardEvent =
  sendMessage domKeyboardEvent altGraphKeySelector

-- | @- keyCode@
keyCode :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO CInt
keyCode domKeyboardEvent =
  sendMessage domKeyboardEvent keyCodeSelector

-- | @- charCode@
charCode :: IsDOMKeyboardEvent domKeyboardEvent => domKeyboardEvent -> IO CInt
charCode domKeyboardEvent =
  sendMessage domKeyboardEvent charCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getModifierState:@
getModifierStateSelector :: Selector '[Id NSString] Bool
getModifierStateSelector = mkSelector "getModifierState:"

-- | @Selector@ for @initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector :: Selector '[Id NSString, Bool, Bool, Id DOMAbstractView, Id NSString, CUInt, Bool, Bool, Bool, Bool, Bool] ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector = mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:"

-- | @Selector@ for @initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKeySelector :: Selector '[Id NSString, Bool, Bool, Id DOMAbstractView, Id NSString, CUInt, Bool, Bool, Bool, Bool] ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_location_ctrlKey_altKey_shiftKey_metaKeySelector = mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:location:ctrlKey:altKey:shiftKey:metaKey:"

-- | @Selector@ for @initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector :: Selector '[Id NSString, Bool, Bool, Id DOMAbstractView, Id NSString, CUInt, Bool, Bool, Bool, Bool, Bool] ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKey_altGraphKeySelector = mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:altGraphKey:"

-- | @Selector@ for @initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:@
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKeySelector :: Selector '[Id NSString, Bool, Bool, Id DOMAbstractView, Id NSString, CUInt, Bool, Bool, Bool, Bool] ()
initKeyboardEvent_canBubble_cancelable_view_keyIdentifier_keyLocation_ctrlKey_altKey_shiftKey_metaKeySelector = mkSelector "initKeyboardEvent:canBubble:cancelable:view:keyIdentifier:keyLocation:ctrlKey:altKey:shiftKey:metaKey:"

-- | @Selector@ for @keyIdentifier@
keyIdentifierSelector :: Selector '[] (Id NSString)
keyIdentifierSelector = mkSelector "keyIdentifier"

-- | @Selector@ for @location@
locationSelector :: Selector '[] CUInt
locationSelector = mkSelector "location"

-- | @Selector@ for @keyLocation@
keyLocationSelector :: Selector '[] CUInt
keyLocationSelector = mkSelector "keyLocation"

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

-- | @Selector@ for @altGraphKey@
altGraphKeySelector :: Selector '[] Bool
altGraphKeySelector = mkSelector "altGraphKey"

-- | @Selector@ for @keyCode@
keyCodeSelector :: Selector '[] CInt
keyCodeSelector = mkSelector "keyCode"

-- | @Selector@ for @charCode@
charCodeSelector :: Selector '[] CInt
charCodeSelector = mkSelector "charCode"

