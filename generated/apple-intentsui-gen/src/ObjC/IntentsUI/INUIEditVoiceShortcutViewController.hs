{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A view controller that shows the details of a voice shortcut, and lets the user edit the phrase.
--
-- To have the user edit a voice shortcut, create an @INUIEditVoiceShortcutViewController@ object with the @INVoiceShortcut@ that they wish to edit, and set its delegate. Then, present the view controller modally from another view controller in your app. Your delegate must dismiss the view controller when the user finishes editing.
--
-- Generated bindings for @INUIEditVoiceShortcutViewController@.
module ObjC.IntentsUI.INUIEditVoiceShortcutViewController
  ( INUIEditVoiceShortcutViewController
  , IsINUIEditVoiceShortcutViewController(..)
  , initWithVoiceShortcut
  , init_
  , initWithNibName_bundle
  , delegate
  , setDelegate
  , delegateSelector
  , initSelector
  , initWithNibName_bundleSelector
  , initWithVoiceShortcutSelector
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

-- | @voiceShortcut@ — The voice shortcut to be edited.
--
-- ObjC selector: @- initWithVoiceShortcut:@
initWithVoiceShortcut :: (IsINUIEditVoiceShortcutViewController inuiEditVoiceShortcutViewController, IsINVoiceShortcut voiceShortcut) => inuiEditVoiceShortcutViewController -> voiceShortcut -> IO (Id INUIEditVoiceShortcutViewController)
initWithVoiceShortcut inuiEditVoiceShortcutViewController voiceShortcut =
  sendOwnedMessage inuiEditVoiceShortcutViewController initWithVoiceShortcutSelector (toINVoiceShortcut voiceShortcut)

-- | @- init@
init_ :: IsINUIEditVoiceShortcutViewController inuiEditVoiceShortcutViewController => inuiEditVoiceShortcutViewController -> IO (Id INUIEditVoiceShortcutViewController)
init_ inuiEditVoiceShortcutViewController =
  sendOwnedMessage inuiEditVoiceShortcutViewController initSelector

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsINUIEditVoiceShortcutViewController inuiEditVoiceShortcutViewController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => inuiEditVoiceShortcutViewController -> nibNameOrNil -> nibBundleOrNil -> IO (Id INUIEditVoiceShortcutViewController)
initWithNibName_bundle inuiEditVoiceShortcutViewController nibNameOrNil nibBundleOrNil =
  sendOwnedMessage inuiEditVoiceShortcutViewController initWithNibName_bundleSelector (toNSString nibNameOrNil) (toNSBundle nibBundleOrNil)

-- | @- delegate@
delegate :: IsINUIEditVoiceShortcutViewController inuiEditVoiceShortcutViewController => inuiEditVoiceShortcutViewController -> IO RawId
delegate inuiEditVoiceShortcutViewController =
  sendMessage inuiEditVoiceShortcutViewController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsINUIEditVoiceShortcutViewController inuiEditVoiceShortcutViewController => inuiEditVoiceShortcutViewController -> RawId -> IO ()
setDelegate inuiEditVoiceShortcutViewController value =
  sendMessage inuiEditVoiceShortcutViewController setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithVoiceShortcut:@
initWithVoiceShortcutSelector :: Selector '[Id INVoiceShortcut] (Id INUIEditVoiceShortcutViewController)
initWithVoiceShortcutSelector = mkSelector "initWithVoiceShortcut:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INUIEditVoiceShortcutViewController)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id INUIEditVoiceShortcutViewController)
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

