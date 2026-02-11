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
  , initWithVoiceShortcutSelector
  , initSelector
  , initWithNibName_bundleSelector


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

import ObjC.IntentsUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Intents.Internal.Classes

-- | @voiceShortcut@ — The voice shortcut to be edited.
--
-- ObjC selector: @- initWithVoiceShortcut:@
initWithVoiceShortcut :: (IsINUIEditVoiceShortcutViewController inuiEditVoiceShortcutViewController, IsINVoiceShortcut voiceShortcut) => inuiEditVoiceShortcutViewController -> voiceShortcut -> IO (Id INUIEditVoiceShortcutViewController)
initWithVoiceShortcut inuiEditVoiceShortcutViewController  voiceShortcut =
withObjCPtr voiceShortcut $ \raw_voiceShortcut ->
    sendMsg inuiEditVoiceShortcutViewController (mkSelector "initWithVoiceShortcut:") (retPtr retVoid) [argPtr (castPtr raw_voiceShortcut :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsINUIEditVoiceShortcutViewController inuiEditVoiceShortcutViewController => inuiEditVoiceShortcutViewController -> IO (Id INUIEditVoiceShortcutViewController)
init_ inuiEditVoiceShortcutViewController  =
  sendMsg inuiEditVoiceShortcutViewController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsINUIEditVoiceShortcutViewController inuiEditVoiceShortcutViewController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => inuiEditVoiceShortcutViewController -> nibNameOrNil -> nibBundleOrNil -> IO (Id INUIEditVoiceShortcutViewController)
initWithNibName_bundle inuiEditVoiceShortcutViewController  nibNameOrNil nibBundleOrNil =
withObjCPtr nibNameOrNil $ \raw_nibNameOrNil ->
  withObjCPtr nibBundleOrNil $ \raw_nibBundleOrNil ->
      sendMsg inuiEditVoiceShortcutViewController (mkSelector "initWithNibName:bundle:") (retPtr retVoid) [argPtr (castPtr raw_nibNameOrNil :: Ptr ()), argPtr (castPtr raw_nibBundleOrNil :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithVoiceShortcut:@
initWithVoiceShortcutSelector :: Selector
initWithVoiceShortcutSelector = mkSelector "initWithVoiceShortcut:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

