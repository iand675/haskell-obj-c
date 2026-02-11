{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.IntentsUI.Internal.Classes (
    module ObjC.IntentsUI.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.Intents.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Intents.Internal.Classes

-- ---------- INUIAddVoiceShortcutViewController ----------

-- | A view controller that will take the user through the setup flow to add a shortcut to Siri.
--
-- First create the @INShortcut@ object that represents the shortcut the user wants to perform. Then create an @INUIAddVoiceShortcutViewController@ object and set its delegate. Then, present the view controller modally from another view controller in your app. The delegate must dismiss the view controller when the user completes the set up.
-- 
-- Phantom type for @INUIAddVoiceShortcutViewController@.
data INUIAddVoiceShortcutViewController

instance IsObjCObject (Id INUIAddVoiceShortcutViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INUIAddVoiceShortcutViewController"

class IsNSViewController a => IsINUIAddVoiceShortcutViewController a where
  toINUIAddVoiceShortcutViewController :: a -> Id INUIAddVoiceShortcutViewController

instance IsINUIAddVoiceShortcutViewController (Id INUIAddVoiceShortcutViewController) where
  toINUIAddVoiceShortcutViewController = unsafeCastId

instance IsNSObject (Id INUIAddVoiceShortcutViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id INUIAddVoiceShortcutViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id INUIAddVoiceShortcutViewController) where
  toNSViewController = unsafeCastId

-- ---------- INUIEditVoiceShortcutViewController ----------

-- | A view controller that shows the details of a voice shortcut, and lets the user edit the phrase.
--
-- To have the user edit a voice shortcut, create an @INUIEditVoiceShortcutViewController@ object with the @INVoiceShortcut@ that they wish to edit, and set its delegate. Then, present the view controller modally from another view controller in your app. Your delegate must dismiss the view controller when the user finishes editing.
-- 
-- Phantom type for @INUIEditVoiceShortcutViewController@.
data INUIEditVoiceShortcutViewController

instance IsObjCObject (Id INUIEditVoiceShortcutViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INUIEditVoiceShortcutViewController"

class IsNSViewController a => IsINUIEditVoiceShortcutViewController a where
  toINUIEditVoiceShortcutViewController :: a -> Id INUIEditVoiceShortcutViewController

instance IsINUIEditVoiceShortcutViewController (Id INUIEditVoiceShortcutViewController) where
  toINUIEditVoiceShortcutViewController = unsafeCastId

instance IsNSObject (Id INUIEditVoiceShortcutViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id INUIEditVoiceShortcutViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id INUIEditVoiceShortcutViewController) where
  toNSViewController = unsafeCastId

-- ---------- INUIAddVoiceShortcutButton ----------

-- | Phantom type for @INUIAddVoiceShortcutButton@.
data INUIAddVoiceShortcutButton

instance IsObjCObject (Id INUIAddVoiceShortcutButton) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "INUIAddVoiceShortcutButton"

class IsNSButton a => IsINUIAddVoiceShortcutButton a where
  toINUIAddVoiceShortcutButton :: a -> Id INUIAddVoiceShortcutButton

instance IsINUIAddVoiceShortcutButton (Id INUIAddVoiceShortcutButton) where
  toINUIAddVoiceShortcutButton = unsafeCastId

instance IsNSButton (Id INUIAddVoiceShortcutButton) where
  toNSButton = unsafeCastId

instance IsNSControl (Id INUIAddVoiceShortcutButton) where
  toNSControl = unsafeCastId

instance IsNSObject (Id INUIAddVoiceShortcutButton) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id INUIAddVoiceShortcutButton) where
  toNSResponder = unsafeCastId

instance IsNSView (Id INUIAddVoiceShortcutButton) where
  toNSView = unsafeCastId
