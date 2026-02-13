{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , aliasesSelector
  , analogSelector
  , boundToSystemGestureSelector
  , collectionSelector
  , localizedNameSelector
  , preferredSystemGestureStateSelector
  , setLocalizedNameSelector
  , setPreferredSystemGestureStateSelector
  , setSfSymbolsNameSelector
  , setUnmappedLocalizedNameSelector
  , setUnmappedSfSymbolsNameSelector
  , sfSymbolsNameSelector
  , unmappedLocalizedNameSelector
  , unmappedSfSymbolsNameSelector

  -- * Enum types
  , GCSystemGestureState(GCSystemGestureState)
  , pattern GCSystemGestureStateEnabled
  , pattern GCSystemGestureStateAlwaysReceive
  , pattern GCSystemGestureStateDisabled

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.GameController.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Each element can be part of a wider collection of inputs that map to a single logical element. A directional pad (dpad) is a logical collection of two axis inputs and thus each axis belongs to the same collection element - the dpad.
--
-- ObjC selector: @- collection@
collection :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id GCControllerElement)
collection gcControllerElement =
  sendMessage gcControllerElement collectionSelector

-- | Check if the element can support more than just digital values, such as decimal ranges between 0 and 1. Defaults to YES for most elements.
--
-- ObjC selector: @- analog@
analog :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO Bool
analog gcControllerElement =
  sendMessage gcControllerElement analogSelector

-- | Check if the element is bound to a system gesture. Defaults to NO for most elements.
--
-- See: preferredSystemGestureState
--
-- See: GCSystemGestureState
--
-- ObjC selector: @- boundToSystemGesture@
boundToSystemGesture :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO Bool
boundToSystemGesture gcControllerElement =
  sendMessage gcControllerElement boundToSystemGestureSelector

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
preferredSystemGestureState gcControllerElement =
  sendMessage gcControllerElement preferredSystemGestureStateSelector

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
setPreferredSystemGestureState gcControllerElement value =
  sendMessage gcControllerElement setPreferredSystemGestureStateSelector value

-- | The element's SF Symbols name, taking input remapping into account.
--
-- Note: In almost all instances, you should use this over unmappedSfSymbolsName in your UI.
--
-- ObjC selector: @- sfSymbolsName@
sfSymbolsName :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id NSString)
sfSymbolsName gcControllerElement =
  sendMessage gcControllerElement sfSymbolsNameSelector

-- | The element's SF Symbols name, taking input remapping into account.
--
-- Note: In almost all instances, you should use this over unmappedSfSymbolsName in your UI.
--
-- ObjC selector: @- setSfSymbolsName:@
setSfSymbolsName :: (IsGCControllerElement gcControllerElement, IsNSString value) => gcControllerElement -> value -> IO ()
setSfSymbolsName gcControllerElement value =
  sendMessage gcControllerElement setSfSymbolsNameSelector (toNSString value)

-- | The element's localized name, taking input remapping into account.
--
-- Note: In almost all instances, you should use this over unmappedLocalizedName in your UI.
--
-- ObjC selector: @- localizedName@
localizedName :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id NSString)
localizedName gcControllerElement =
  sendMessage gcControllerElement localizedNameSelector

-- | The element's localized name, taking input remapping into account.
--
-- Note: In almost all instances, you should use this over unmappedLocalizedName in your UI.
--
-- ObjC selector: @- setLocalizedName:@
setLocalizedName :: (IsGCControllerElement gcControllerElement, IsNSString value) => gcControllerElement -> value -> IO ()
setLocalizedName gcControllerElement value =
  sendMessage gcControllerElement setLocalizedNameSelector (toNSString value)

-- | The element's SF Symbols name, not taking any input remapping into account.
--
-- Note: Use this in your games own remapping UI, or when you need to prompt a user that a given button has no mapping (sfSymbolsName is nil).
--
-- ObjC selector: @- unmappedSfSymbolsName@
unmappedSfSymbolsName :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id NSString)
unmappedSfSymbolsName gcControllerElement =
  sendMessage gcControllerElement unmappedSfSymbolsNameSelector

-- | The element's SF Symbols name, not taking any input remapping into account.
--
-- Note: Use this in your games own remapping UI, or when you need to prompt a user that a given button has no mapping (sfSymbolsName is nil).
--
-- ObjC selector: @- setUnmappedSfSymbolsName:@
setUnmappedSfSymbolsName :: (IsGCControllerElement gcControllerElement, IsNSString value) => gcControllerElement -> value -> IO ()
setUnmappedSfSymbolsName gcControllerElement value =
  sendMessage gcControllerElement setUnmappedSfSymbolsNameSelector (toNSString value)

-- | The element's localized name, not taking any input remapping into account.
--
-- Note: Use this in your games own remapping UI, or when you need to prompt a user that a given button has no mapping (localizedName is nil).
--
-- ObjC selector: @- unmappedLocalizedName@
unmappedLocalizedName :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id NSString)
unmappedLocalizedName gcControllerElement =
  sendMessage gcControllerElement unmappedLocalizedNameSelector

-- | The element's localized name, not taking any input remapping into account.
--
-- Note: Use this in your games own remapping UI, or when you need to prompt a user that a given button has no mapping (localizedName is nil).
--
-- ObjC selector: @- setUnmappedLocalizedName:@
setUnmappedLocalizedName :: (IsGCControllerElement gcControllerElement, IsNSString value) => gcControllerElement -> value -> IO ()
setUnmappedLocalizedName gcControllerElement value =
  sendMessage gcControllerElement setUnmappedLocalizedNameSelector (toNSString value)

-- | A set of aliases that can be used to access this element with keyed subscript notation.
--
-- ObjC selector: @- aliases@
aliases :: IsGCControllerElement gcControllerElement => gcControllerElement -> IO (Id NSSet)
aliases gcControllerElement =
  sendMessage gcControllerElement aliasesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @collection@
collectionSelector :: Selector '[] (Id GCControllerElement)
collectionSelector = mkSelector "collection"

-- | @Selector@ for @analog@
analogSelector :: Selector '[] Bool
analogSelector = mkSelector "analog"

-- | @Selector@ for @boundToSystemGesture@
boundToSystemGestureSelector :: Selector '[] Bool
boundToSystemGestureSelector = mkSelector "boundToSystemGesture"

-- | @Selector@ for @preferredSystemGestureState@
preferredSystemGestureStateSelector :: Selector '[] GCSystemGestureState
preferredSystemGestureStateSelector = mkSelector "preferredSystemGestureState"

-- | @Selector@ for @setPreferredSystemGestureState:@
setPreferredSystemGestureStateSelector :: Selector '[GCSystemGestureState] ()
setPreferredSystemGestureStateSelector = mkSelector "setPreferredSystemGestureState:"

-- | @Selector@ for @sfSymbolsName@
sfSymbolsNameSelector :: Selector '[] (Id NSString)
sfSymbolsNameSelector = mkSelector "sfSymbolsName"

-- | @Selector@ for @setSfSymbolsName:@
setSfSymbolsNameSelector :: Selector '[Id NSString] ()
setSfSymbolsNameSelector = mkSelector "setSfSymbolsName:"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @setLocalizedName:@
setLocalizedNameSelector :: Selector '[Id NSString] ()
setLocalizedNameSelector = mkSelector "setLocalizedName:"

-- | @Selector@ for @unmappedSfSymbolsName@
unmappedSfSymbolsNameSelector :: Selector '[] (Id NSString)
unmappedSfSymbolsNameSelector = mkSelector "unmappedSfSymbolsName"

-- | @Selector@ for @setUnmappedSfSymbolsName:@
setUnmappedSfSymbolsNameSelector :: Selector '[Id NSString] ()
setUnmappedSfSymbolsNameSelector = mkSelector "setUnmappedSfSymbolsName:"

-- | @Selector@ for @unmappedLocalizedName@
unmappedLocalizedNameSelector :: Selector '[] (Id NSString)
unmappedLocalizedNameSelector = mkSelector "unmappedLocalizedName"

-- | @Selector@ for @setUnmappedLocalizedName:@
setUnmappedLocalizedNameSelector :: Selector '[Id NSString] ()
setUnmappedLocalizedNameSelector = mkSelector "setUnmappedLocalizedName:"

-- | @Selector@ for @aliases@
aliasesSelector :: Selector '[] (Id NSSet)
aliasesSelector = mkSelector "aliases"

