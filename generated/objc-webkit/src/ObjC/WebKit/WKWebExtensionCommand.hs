{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtensionCommand`` object encapsulates the properties for an individual web extension command.
--
-- Provides access to command properties such as a unique identifier, a descriptive title, and shortcut keys. Commands can be used by a web extension to perform specific actions within a web extension context, such toggling features, or interacting with web content. These commands enhance the functionality of the extension by allowing users to invoke actions quickly.
--
-- Generated bindings for @WKWebExtensionCommand@.
module ObjC.WebKit.WKWebExtensionCommand
  ( WKWebExtensionCommand
  , IsWKWebExtensionCommand(..)
  , new
  , init_
  , webExtensionContext
  , title
  , activationKey
  , setActivationKey
  , modifierFlags
  , setModifierFlags
  , menuItem
  , newSelector
  , initSelector
  , webExtensionContextSelector
  , titleSelector
  , activationKeySelector
  , setActivationKeySelector
  , modifierFlagsSelector
  , setModifierFlagsSelector
  , menuItemSelector

  -- * Enum types
  , NSEventModifierFlags(NSEventModifierFlags)
  , pattern NSEventModifierFlagCapsLock
  , pattern NSEventModifierFlagShift
  , pattern NSEventModifierFlagControl
  , pattern NSEventModifierFlagOption
  , pattern NSEventModifierFlagCommand
  , pattern NSEventModifierFlagNumericPad
  , pattern NSEventModifierFlagHelp
  , pattern NSEventModifierFlagFunction
  , pattern NSEventModifierFlagDeviceIndependentFlagsMask

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
import ObjC.AppKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionCommand)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionCommand"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id WKWebExtensionCommand)
init_ wkWebExtensionCommand  =
  sendMsg wkWebExtensionCommand (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The web extension context associated with the command.
--
-- ObjC selector: @- webExtensionContext@
webExtensionContext :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id WKWebExtensionContext)
webExtensionContext wkWebExtensionCommand  =
  sendMsg wkWebExtensionCommand (mkSelector "webExtensionContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Descriptive title for the command aiding discoverability.
--
-- This title can be displayed in user interface elements such as keyboard shortcuts lists or menu items to help users understand its purpose.
--
-- ObjC selector: @- title@
title :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id NSString)
title wkWebExtensionCommand  =
  sendMsg wkWebExtensionCommand (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The primary key used to trigger the command, distinct from any modifier flags.
--
-- This property can be customized within the app to avoid conflicts with existing shortcuts or to enable user personalization. It should accurately represent the activation key as used by the app, which the extension can use to display the complete shortcut in its interface. If no shortcut is desired for the command, the property should be set to @nil@. This value should be saved and restored as needed by the app.
--
-- ObjC selector: @- activationKey@
activationKey :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id NSString)
activationKey wkWebExtensionCommand  =
  sendMsg wkWebExtensionCommand (mkSelector "activationKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The primary key used to trigger the command, distinct from any modifier flags.
--
-- This property can be customized within the app to avoid conflicts with existing shortcuts or to enable user personalization. It should accurately represent the activation key as used by the app, which the extension can use to display the complete shortcut in its interface. If no shortcut is desired for the command, the property should be set to @nil@. This value should be saved and restored as needed by the app.
--
-- ObjC selector: @- setActivationKey:@
setActivationKey :: (IsWKWebExtensionCommand wkWebExtensionCommand, IsNSString value) => wkWebExtensionCommand -> value -> IO ()
setActivationKey wkWebExtensionCommand  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionCommand (mkSelector "setActivationKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modifierFlags@
modifierFlags :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO NSEventModifierFlags
modifierFlags wkWebExtensionCommand  =
  fmap (coerce :: CULong -> NSEventModifierFlags) $ sendMsg wkWebExtensionCommand (mkSelector "modifierFlags") retCULong []

-- | @- setModifierFlags:@
setModifierFlags :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> NSEventModifierFlags -> IO ()
setModifierFlags wkWebExtensionCommand  value =
  sendMsg wkWebExtensionCommand (mkSelector "setModifierFlags:") retVoid [argCULong (coerce value)]

-- | @- menuItem@
menuItem :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id NSMenuItem)
menuItem wkWebExtensionCommand  =
  sendMsg wkWebExtensionCommand (mkSelector "menuItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @webExtensionContext@
webExtensionContextSelector :: Selector
webExtensionContextSelector = mkSelector "webExtensionContext"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @activationKey@
activationKeySelector :: Selector
activationKeySelector = mkSelector "activationKey"

-- | @Selector@ for @setActivationKey:@
setActivationKeySelector :: Selector
setActivationKeySelector = mkSelector "setActivationKey:"

-- | @Selector@ for @modifierFlags@
modifierFlagsSelector :: Selector
modifierFlagsSelector = mkSelector "modifierFlags"

-- | @Selector@ for @setModifierFlags:@
setModifierFlagsSelector :: Selector
setModifierFlagsSelector = mkSelector "setModifierFlags:"

-- | @Selector@ for @menuItem@
menuItemSelector :: Selector
menuItemSelector = mkSelector "menuItem"

