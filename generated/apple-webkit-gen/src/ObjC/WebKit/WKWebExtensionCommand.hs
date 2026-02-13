{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , identifier
  , title
  , activationKey
  , setActivationKey
  , modifierFlags
  , setModifierFlags
  , menuItem
  , activationKeySelector
  , identifierSelector
  , initSelector
  , menuItemSelector
  , modifierFlagsSelector
  , newSelector
  , setActivationKeySelector
  , setModifierFlagsSelector
  , titleSelector
  , webExtensionContextSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id WKWebExtensionCommand)
init_ wkWebExtensionCommand =
  sendOwnedMessage wkWebExtensionCommand initSelector

-- | The web extension context associated with the command.
--
-- ObjC selector: @- webExtensionContext@
webExtensionContext :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id WKWebExtensionContext)
webExtensionContext wkWebExtensionCommand =
  sendMessage wkWebExtensionCommand webExtensionContextSelector

-- | A unique identifier for the command.
--
-- ObjC selector: @- identifier@
identifier :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id NSString)
identifier wkWebExtensionCommand =
  sendMessage wkWebExtensionCommand identifierSelector

-- | Descriptive title for the command aiding discoverability.
--
-- This title can be displayed in user interface elements such as keyboard shortcuts lists or menu items to help users understand its purpose.
--
-- ObjC selector: @- title@
title :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id NSString)
title wkWebExtensionCommand =
  sendMessage wkWebExtensionCommand titleSelector

-- | The primary key used to trigger the command, distinct from any modifier flags.
--
-- This property can be customized within the app to avoid conflicts with existing shortcuts or to enable user personalization. It should accurately represent the activation key as used by the app, which the extension can use to display the complete shortcut in its interface. If no shortcut is desired for the command, the property should be set to @nil@. This value should be saved and restored as needed by the app.
--
-- ObjC selector: @- activationKey@
activationKey :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id NSString)
activationKey wkWebExtensionCommand =
  sendMessage wkWebExtensionCommand activationKeySelector

-- | The primary key used to trigger the command, distinct from any modifier flags.
--
-- This property can be customized within the app to avoid conflicts with existing shortcuts or to enable user personalization. It should accurately represent the activation key as used by the app, which the extension can use to display the complete shortcut in its interface. If no shortcut is desired for the command, the property should be set to @nil@. This value should be saved and restored as needed by the app.
--
-- ObjC selector: @- setActivationKey:@
setActivationKey :: (IsWKWebExtensionCommand wkWebExtensionCommand, IsNSString value) => wkWebExtensionCommand -> value -> IO ()
setActivationKey wkWebExtensionCommand value =
  sendMessage wkWebExtensionCommand setActivationKeySelector (toNSString value)

-- | @- modifierFlags@
modifierFlags :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO NSEventModifierFlags
modifierFlags wkWebExtensionCommand =
  sendMessage wkWebExtensionCommand modifierFlagsSelector

-- | @- setModifierFlags:@
setModifierFlags :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> NSEventModifierFlags -> IO ()
setModifierFlags wkWebExtensionCommand value =
  sendMessage wkWebExtensionCommand setModifierFlagsSelector value

-- | @- menuItem@
menuItem :: IsWKWebExtensionCommand wkWebExtensionCommand => wkWebExtensionCommand -> IO (Id NSMenuItem)
menuItem wkWebExtensionCommand =
  sendMessage wkWebExtensionCommand menuItemSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKWebExtensionCommand)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKWebExtensionCommand)
initSelector = mkSelector "init"

-- | @Selector@ for @webExtensionContext@
webExtensionContextSelector :: Selector '[] (Id WKWebExtensionContext)
webExtensionContextSelector = mkSelector "webExtensionContext"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @activationKey@
activationKeySelector :: Selector '[] (Id NSString)
activationKeySelector = mkSelector "activationKey"

-- | @Selector@ for @setActivationKey:@
setActivationKeySelector :: Selector '[Id NSString] ()
setActivationKeySelector = mkSelector "setActivationKey:"

-- | @Selector@ for @modifierFlags@
modifierFlagsSelector :: Selector '[] NSEventModifierFlags
modifierFlagsSelector = mkSelector "modifierFlags"

-- | @Selector@ for @setModifierFlags:@
setModifierFlagsSelector :: Selector '[NSEventModifierFlags] ()
setModifierFlagsSelector = mkSelector "setModifierFlags:"

-- | @Selector@ for @menuItem@
menuItemSelector :: Selector '[] (Id NSMenuItem)
menuItemSelector = mkSelector "menuItem"

