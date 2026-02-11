{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Every controller element knows which collection it belongs to and whether its input value is analog or digital.
--
-- Generated bindings for @GCControllerElement@.
module ObjC.GameController.GCControllerElement
  ( GCControllerElement
  , IsGCControllerElement(..)
  , collection
  , analog
  , boundToSystemGesture
  , preferredSystemGestureState
  , setPreferredSystemGestureState
  , sfSymbolsName
  , setSfSymbolsName
  , localizedName
  , setLocalizedName
  , unmappedSfSymbolsName
  , setUnmappedSfSymbolsName
  , unmappedLocalizedName
  , setUnmappedLocalizedName
  , aliases
  , collectionSelector
  , analogSelector
  , boundToSystemGestureSelector
  , preferredSystemGestureStateSelector
  , setPreferredSystemGestureStateSelector
  , sfSymbolsNameSelector
  , setSfSymbolsNameSelector
  , localizedNameSelector
  , setLocalizedNameSelector
  , unmappedSfSymbolsNameSelector
  , setUnmappedSfSymbolsNameSelector
  , unmappedLocalizedNameSelector
  , setUnmappedLocalizedNameSelector
  , aliasesSelector

  -- * Enum types
  , GCSystemGestureState(GCSystemGestureState)
  , pattern GCSystemGestureStateEnabled
  , pattern GCSystemGestureStateAlwaysReceive
  , pattern GCSystemGestureStateDisabled

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

import ObjC.GameController.Internal.Classes
import ObjC.GameController.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Each element can be part of a wider collection of inputs that map to a single logical element. A directional pad (dpad) is a logical collection of two axis inputs and thus each axis belongs to the same collection element - the dpad.
--
-- ObjC selector: @- collection@
collection :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id GCControllerElement)
collection gcControllerElement  =
  sendMsg gcControllerElement (mkSelector "collection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Check if the element can support more than just digital values, such as decimal ranges between 0 and 1. Defaults to YES for most elements.
--
-- ObjC selector: @- analog@
analog :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO Bool
analog gcControllerElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcControllerElement (mkSelector "analog") retCULong []

-- | Check if the element is bound to a system gesture. Defaults to NO for most elements.
--
-- See: preferredSystemGestureState
--
-- See: GCSystemGestureState
--
-- ObjC selector: @- boundToSystemGesture@
boundToSystemGesture :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO Bool
boundToSystemGesture gcControllerElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcControllerElement (mkSelector "boundToSystemGesture") retCULong []

-- | The preferred system gesture state for this element. Defaults to GCSystemGestureStateEnabled for most elements
--
-- Note: This is merely the preferred system gesture state - it is not guaranteed to be respected by the system.
--
-- Note: It is highly recommended to leave this set to the default value, however there may be situations (for example, game streaming apps) where it is preferrable to disable system gestures.
--
-- See: boundToSystemGesture
--
-- ObjC selector: @- preferredSystemGestureState@
preferredSystemGestureState :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO GCSystemGestureState
preferredSystemGestureState gcControllerElement  =
  fmap (coerce :: CLong -> GCSystemGestureState) $ sendMsg gcControllerElement (mkSelector "preferredSystemGestureState") retCLong []

-- | The preferred system gesture state for this element. Defaults to GCSystemGestureStateEnabled for most elements
--
-- Note: This is merely the preferred system gesture state - it is not guaranteed to be respected by the system.
--
-- Note: It is highly recommended to leave this set to the default value, however there may be situations (for example, game streaming apps) where it is preferrable to disable system gestures.
--
-- See: boundToSystemGesture
--
-- ObjC selector: @- setPreferredSystemGestureState:@
setPreferredSystemGestureState :: IsGCControllerElement gcControllerElement => gcControllerElement -> GCSystemGestureState -> IO ()
setPreferredSystemGestureState gcControllerElement  value =
  sendMsg gcControllerElement (mkSelector "setPreferredSystemGestureState:") retVoid [argCLong (coerce value)]

-- | The element's SF Symbols name, taking input remapping into account.
--
-- Note: In almost all instances, you should use this over unmappedSfSymbolsName in your UI.
--
-- ObjC selector: @- sfSymbolsName@
sfSymbolsName :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id NSString)
sfSymbolsName gcControllerElement  =
  sendMsg gcControllerElement (mkSelector "sfSymbolsName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The element's SF Symbols name, taking input remapping into account.
--
-- Note: In almost all instances, you should use this over unmappedSfSymbolsName in your UI.
--
-- ObjC selector: @- setSfSymbolsName:@
setSfSymbolsName :: (IsGCControllerElement gcControllerElement, IsNSString value) => gcControllerElement -> value -> IO ()
setSfSymbolsName gcControllerElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg gcControllerElement (mkSelector "setSfSymbolsName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The element's localized name, taking input remapping into account.
--
-- Note: In almost all instances, you should use this over unmappedLocalizedName in your UI.
--
-- ObjC selector: @- localizedName@
localizedName :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id NSString)
localizedName gcControllerElement  =
  sendMsg gcControllerElement (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The element's localized name, taking input remapping into account.
--
-- Note: In almost all instances, you should use this over unmappedLocalizedName in your UI.
--
-- ObjC selector: @- setLocalizedName:@
setLocalizedName :: (IsGCControllerElement gcControllerElement, IsNSString value) => gcControllerElement -> value -> IO ()
setLocalizedName gcControllerElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg gcControllerElement (mkSelector "setLocalizedName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The element's SF Symbols name, not taking any input remapping into account.
--
-- Note: Use this in your games own remapping UI, or when you need to prompt a user that a given button has no mapping (sfSymbolsName is nil).
--
-- ObjC selector: @- unmappedSfSymbolsName@
unmappedSfSymbolsName :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id NSString)
unmappedSfSymbolsName gcControllerElement  =
  sendMsg gcControllerElement (mkSelector "unmappedSfSymbolsName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The element's SF Symbols name, not taking any input remapping into account.
--
-- Note: Use this in your games own remapping UI, or when you need to prompt a user that a given button has no mapping (sfSymbolsName is nil).
--
-- ObjC selector: @- setUnmappedSfSymbolsName:@
setUnmappedSfSymbolsName :: (IsGCControllerElement gcControllerElement, IsNSString value) => gcControllerElement -> value -> IO ()
setUnmappedSfSymbolsName gcControllerElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg gcControllerElement (mkSelector "setUnmappedSfSymbolsName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The element's localized name, not taking any input remapping into account.
--
-- Note: Use this in your games own remapping UI, or when you need to prompt a user that a given button has no mapping (localizedName is nil).
--
-- ObjC selector: @- unmappedLocalizedName@
unmappedLocalizedName :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id NSString)
unmappedLocalizedName gcControllerElement  =
  sendMsg gcControllerElement (mkSelector "unmappedLocalizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The element's localized name, not taking any input remapping into account.
--
-- Note: Use this in your games own remapping UI, or when you need to prompt a user that a given button has no mapping (localizedName is nil).
--
-- ObjC selector: @- setUnmappedLocalizedName:@
setUnmappedLocalizedName :: (IsGCControllerElement gcControllerElement, IsNSString value) => gcControllerElement -> value -> IO ()
setUnmappedLocalizedName gcControllerElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg gcControllerElement (mkSelector "setUnmappedLocalizedName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A set of aliases that can be used to access this element with keyed subscript notation.
--
-- ObjC selector: @- aliases@
aliases :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id NSSet)
aliases gcControllerElement  =
  sendMsg gcControllerElement (mkSelector "aliases") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @collection@
collectionSelector :: Selector
collectionSelector = mkSelector "collection"

-- | @Selector@ for @analog@
analogSelector :: Selector
analogSelector = mkSelector "analog"

-- | @Selector@ for @boundToSystemGesture@
boundToSystemGestureSelector :: Selector
boundToSystemGestureSelector = mkSelector "boundToSystemGesture"

-- | @Selector@ for @preferredSystemGestureState@
preferredSystemGestureStateSelector :: Selector
preferredSystemGestureStateSelector = mkSelector "preferredSystemGestureState"

-- | @Selector@ for @setPreferredSystemGestureState:@
setPreferredSystemGestureStateSelector :: Selector
setPreferredSystemGestureStateSelector = mkSelector "setPreferredSystemGestureState:"

-- | @Selector@ for @sfSymbolsName@
sfSymbolsNameSelector :: Selector
sfSymbolsNameSelector = mkSelector "sfSymbolsName"

-- | @Selector@ for @setSfSymbolsName:@
setSfSymbolsNameSelector :: Selector
setSfSymbolsNameSelector = mkSelector "setSfSymbolsName:"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @setLocalizedName:@
setLocalizedNameSelector :: Selector
setLocalizedNameSelector = mkSelector "setLocalizedName:"

-- | @Selector@ for @unmappedSfSymbolsName@
unmappedSfSymbolsNameSelector :: Selector
unmappedSfSymbolsNameSelector = mkSelector "unmappedSfSymbolsName"

-- | @Selector@ for @setUnmappedSfSymbolsName:@
setUnmappedSfSymbolsNameSelector :: Selector
setUnmappedSfSymbolsNameSelector = mkSelector "setUnmappedSfSymbolsName:"

-- | @Selector@ for @unmappedLocalizedName@
unmappedLocalizedNameSelector :: Selector
unmappedLocalizedNameSelector = mkSelector "unmappedLocalizedName"

-- | @Selector@ for @setUnmappedLocalizedName:@
setUnmappedLocalizedNameSelector :: Selector
setUnmappedLocalizedNameSelector = mkSelector "setUnmappedLocalizedName:"

-- | @Selector@ for @aliases@
aliasesSelector :: Selector
aliasesSelector = mkSelector "aliases"

