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
  , initWithShortcutSelector
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

-- | @shortcut@ — The shortcut is what will be run when the resulting voice shortcut is invoked. It also provides the suggested invocation phrase, via the @suggestedInvocationPhrase@ property on the intent or user activity.
--
-- ObjC selector: @- initWithShortcut:@
initWithShortcut :: (IsINUIAddVoiceShortcutViewController inuiAddVoiceShortcutViewController, IsINShortcut shortcut) => inuiAddVoiceShortcutViewController -> shortcut -> IO (Id INUIAddVoiceShortcutViewController)
initWithShortcut inuiAddVoiceShortcutViewController  shortcut =
withObjCPtr shortcut $ \raw_shortcut ->
    sendMsg inuiAddVoiceShortcutViewController (mkSelector "initWithShortcut:") (retPtr retVoid) [argPtr (castPtr raw_shortcut :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsINUIAddVoiceShortcutViewController inuiAddVoiceShortcutViewController => inuiAddVoiceShortcutViewController -> IO (Id INUIAddVoiceShortcutViewController)
init_ inuiAddVoiceShortcutViewController  =
  sendMsg inuiAddVoiceShortcutViewController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsINUIAddVoiceShortcutViewController inuiAddVoiceShortcutViewController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => inuiAddVoiceShortcutViewController -> nibNameOrNil -> nibBundleOrNil -> IO (Id INUIAddVoiceShortcutViewController)
initWithNibName_bundle inuiAddVoiceShortcutViewController  nibNameOrNil nibBundleOrNil =
withObjCPtr nibNameOrNil $ \raw_nibNameOrNil ->
  withObjCPtr nibBundleOrNil $ \raw_nibBundleOrNil ->
      sendMsg inuiAddVoiceShortcutViewController (mkSelector "initWithNibName:bundle:") (retPtr retVoid) [argPtr (castPtr raw_nibNameOrNil :: Ptr ()), argPtr (castPtr raw_nibBundleOrNil :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithShortcut:@
initWithShortcutSelector :: Selector
initWithShortcutSelector = mkSelector "initWithShortcut:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

