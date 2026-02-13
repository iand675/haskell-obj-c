{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A view controller that will take the user through the setup flow to add a shortcut to Siri.
--
-- First create the @INShortcut@ object that represents the shortcut the user wants to perform. Then create an @INUIAddVoiceShortcutViewController@ object and set its delegate. Then, present the view controller modally from another view controller in your app. The delegate must dismiss the view controller when the user completes the set up.
--
-- Generated bindings for @INUIAddVoiceShortcutViewController@.
module ObjC.IntentsUI.INUIAddVoiceShortcutViewController
  ( INUIAddVoiceShortcutViewController
  , IsINUIAddVoiceShortcutViewController(..)
  , initWithShortcut
  , init_
  , initWithNibName_bundle
  , delegate
  , setDelegate
  , delegateSelector
  , initSelector
  , initWithNibName_bundleSelector
  , initWithShortcutSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IntentsUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Intents.Internal.Classes

-- | @shortcut@ — The shortcut is what will be run when the resulting voice shortcut is invoked. It also provides the suggested invocation phrase, via the @suggestedInvocationPhrase@ property on the intent or user activity.
--
-- ObjC selector: @- initWithShortcut:@
initWithShortcut :: (IsINUIAddVoiceShortcutViewController inuiAddVoiceShortcutViewController, IsINShortcut shortcut) => inuiAddVoiceShortcutViewController -> shortcut -> IO (Id INUIAddVoiceShortcutViewController)
initWithShortcut inuiAddVoiceShortcutViewController shortcut =
  sendOwnedMessage inuiAddVoiceShortcutViewController initWithShortcutSelector (toINShortcut shortcut)

-- | @- init@
init_ :: IsINUIAddVoiceShortcutViewController inuiAddVoiceShortcutViewController => inuiAddVoiceShortcutViewController -> IO (Id INUIAddVoiceShortcutViewController)
init_ inuiAddVoiceShortcutViewController =
  sendOwnedMessage inuiAddVoiceShortcutViewController initSelector

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsINUIAddVoiceShortcutViewController inuiAddVoiceShortcutViewController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => inuiAddVoiceShortcutViewController -> nibNameOrNil -> nibBundleOrNil -> IO (Id INUIAddVoiceShortcutViewController)
initWithNibName_bundle inuiAddVoiceShortcutViewController nibNameOrNil nibBundleOrNil =
  sendOwnedMessage inuiAddVoiceShortcutViewController initWithNibName_bundleSelector (toNSString nibNameOrNil) (toNSBundle nibBundleOrNil)

-- | @- delegate@
delegate :: IsINUIAddVoiceShortcutViewController inuiAddVoiceShortcutViewController => inuiAddVoiceShortcutViewController -> IO RawId
delegate inuiAddVoiceShortcutViewController =
  sendMessage inuiAddVoiceShortcutViewController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsINUIAddVoiceShortcutViewController inuiAddVoiceShortcutViewController => inuiAddVoiceShortcutViewController -> RawId -> IO ()
setDelegate inuiAddVoiceShortcutViewController value =
  sendMessage inuiAddVoiceShortcutViewController setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithShortcut:@
initWithShortcutSelector :: Selector '[Id INShortcut] (Id INUIAddVoiceShortcutViewController)
initWithShortcutSelector = mkSelector "initWithShortcut:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INUIAddVoiceShortcutViewController)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id INUIAddVoiceShortcutViewController)
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

