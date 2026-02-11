{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPreferencePane@.
module ObjC.PreferencePanes.NSPreferencePane
  ( NSPreferencePane
  , IsNSPreferencePane(..)
  , initWithBundle
  , loadMainView
  , mainViewDidLoad
  , assignMainView
  , willSelect
  , didSelect
  , replyToShouldUnselect
  , willUnselect
  , didUnselect
  , updateHelpMenuWithArray
  , bundle
  , mainNibName
  , shouldUnselect
  , mainView
  , setMainView
  , initialKeyView
  , setInitialKeyView
  , firstKeyView
  , setFirstKeyView
  , lastKeyView
  , setLastKeyView
  , autoSaveTextFields
  , selected
  , initWithBundleSelector
  , loadMainViewSelector
  , mainViewDidLoadSelector
  , assignMainViewSelector
  , willSelectSelector
  , didSelectSelector
  , replyToShouldUnselectSelector
  , willUnselectSelector
  , didUnselectSelector
  , updateHelpMenuWithArraySelector
  , bundleSelector
  , mainNibNameSelector
  , shouldUnselectSelector
  , mainViewSelector
  , setMainViewSelector
  , initialKeyViewSelector
  , setInitialKeyViewSelector
  , firstKeyViewSelector
  , setFirstKeyViewSelector
  , lastKeyViewSelector
  , setLastKeyViewSelector
  , autoSaveTextFieldsSelector
  , selectedSelector

  -- * Enum types
  , NSPreferencePaneUnselectReply(NSPreferencePaneUnselectReply)
  , pattern NSUnselectCancel
  , pattern NSUnselectNow
  , pattern NSUnselectLater

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

import ObjC.PreferencePanes.Internal.Classes
import ObjC.PreferencePanes.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithBundle:@
initWithBundle :: (IsNSPreferencePane nsPreferencePane, IsNSBundle bundle) => nsPreferencePane -> bundle -> IO (Id NSPreferencePane)
initWithBundle nsPreferencePane  bundle =
withObjCPtr bundle $ \raw_bundle ->
    sendMsg nsPreferencePane (mkSelector "initWithBundle:") (retPtr retVoid) [argPtr (castPtr raw_bundle :: Ptr ())] >>= ownedObject . castPtr

-- | @- loadMainView@
loadMainView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSView)
loadMainView nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "loadMainView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mainViewDidLoad@
mainViewDidLoad :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
mainViewDidLoad nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "mainViewDidLoad") retVoid []

-- | @- assignMainView@
assignMainView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
assignMainView nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "assignMainView") retVoid []

-- | @- willSelect@
willSelect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
willSelect nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "willSelect") retVoid []

-- | @- didSelect@
didSelect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
didSelect nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "didSelect") retVoid []

-- | @- replyToShouldUnselect:@
replyToShouldUnselect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> Bool -> IO ()
replyToShouldUnselect nsPreferencePane  shouldUnselect =
  sendMsg nsPreferencePane (mkSelector "replyToShouldUnselect:") retVoid [argCULong (if shouldUnselect then 1 else 0)]

-- | @- willUnselect@
willUnselect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
willUnselect nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "willUnselect") retVoid []

-- | @- didUnselect@
didUnselect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
didUnselect nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "didUnselect") retVoid []

-- | @- updateHelpMenuWithArray:@
updateHelpMenuWithArray :: (IsNSPreferencePane nsPreferencePane, IsNSArray inArrayOfMenuItems) => nsPreferencePane -> inArrayOfMenuItems -> IO ()
updateHelpMenuWithArray nsPreferencePane  inArrayOfMenuItems =
withObjCPtr inArrayOfMenuItems $ \raw_inArrayOfMenuItems ->
    sendMsg nsPreferencePane (mkSelector "updateHelpMenuWithArray:") retVoid [argPtr (castPtr raw_inArrayOfMenuItems :: Ptr ())]

-- | @- bundle@
bundle :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSBundle)
bundle nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "bundle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mainNibName@
mainNibName :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSString)
mainNibName nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "mainNibName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shouldUnselect@
shouldUnselect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO NSPreferencePaneUnselectReply
shouldUnselect nsPreferencePane  =
  fmap (coerce :: CULong -> NSPreferencePaneUnselectReply) $ sendMsg nsPreferencePane (mkSelector "shouldUnselect") retCULong []

-- | @- mainView@
mainView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSView)
mainView nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "mainView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMainView:@
setMainView :: (IsNSPreferencePane nsPreferencePane, IsNSView value) => nsPreferencePane -> value -> IO ()
setMainView nsPreferencePane  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPreferencePane (mkSelector "setMainView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- initialKeyView@
initialKeyView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSView)
initialKeyView nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "initialKeyView") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setInitialKeyView:@
setInitialKeyView :: (IsNSPreferencePane nsPreferencePane, IsNSView value) => nsPreferencePane -> value -> IO ()
setInitialKeyView nsPreferencePane  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPreferencePane (mkSelector "setInitialKeyView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- firstKeyView@
firstKeyView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSView)
firstKeyView nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "firstKeyView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFirstKeyView:@
setFirstKeyView :: (IsNSPreferencePane nsPreferencePane, IsNSView value) => nsPreferencePane -> value -> IO ()
setFirstKeyView nsPreferencePane  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPreferencePane (mkSelector "setFirstKeyView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lastKeyView@
lastKeyView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSView)
lastKeyView nsPreferencePane  =
  sendMsg nsPreferencePane (mkSelector "lastKeyView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLastKeyView:@
setLastKeyView :: (IsNSPreferencePane nsPreferencePane, IsNSView value) => nsPreferencePane -> value -> IO ()
setLastKeyView nsPreferencePane  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPreferencePane (mkSelector "setLastKeyView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- autoSaveTextFields@
autoSaveTextFields :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO Bool
autoSaveTextFields nsPreferencePane  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPreferencePane (mkSelector "autoSaveTextFields") retCULong []

-- | @- selected@
selected :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO Bool
selected nsPreferencePane  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPreferencePane (mkSelector "selected") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBundle:@
initWithBundleSelector :: Selector
initWithBundleSelector = mkSelector "initWithBundle:"

-- | @Selector@ for @loadMainView@
loadMainViewSelector :: Selector
loadMainViewSelector = mkSelector "loadMainView"

-- | @Selector@ for @mainViewDidLoad@
mainViewDidLoadSelector :: Selector
mainViewDidLoadSelector = mkSelector "mainViewDidLoad"

-- | @Selector@ for @assignMainView@
assignMainViewSelector :: Selector
assignMainViewSelector = mkSelector "assignMainView"

-- | @Selector@ for @willSelect@
willSelectSelector :: Selector
willSelectSelector = mkSelector "willSelect"

-- | @Selector@ for @didSelect@
didSelectSelector :: Selector
didSelectSelector = mkSelector "didSelect"

-- | @Selector@ for @replyToShouldUnselect:@
replyToShouldUnselectSelector :: Selector
replyToShouldUnselectSelector = mkSelector "replyToShouldUnselect:"

-- | @Selector@ for @willUnselect@
willUnselectSelector :: Selector
willUnselectSelector = mkSelector "willUnselect"

-- | @Selector@ for @didUnselect@
didUnselectSelector :: Selector
didUnselectSelector = mkSelector "didUnselect"

-- | @Selector@ for @updateHelpMenuWithArray:@
updateHelpMenuWithArraySelector :: Selector
updateHelpMenuWithArraySelector = mkSelector "updateHelpMenuWithArray:"

-- | @Selector@ for @bundle@
bundleSelector :: Selector
bundleSelector = mkSelector "bundle"

-- | @Selector@ for @mainNibName@
mainNibNameSelector :: Selector
mainNibNameSelector = mkSelector "mainNibName"

-- | @Selector@ for @shouldUnselect@
shouldUnselectSelector :: Selector
shouldUnselectSelector = mkSelector "shouldUnselect"

-- | @Selector@ for @mainView@
mainViewSelector :: Selector
mainViewSelector = mkSelector "mainView"

-- | @Selector@ for @setMainView:@
setMainViewSelector :: Selector
setMainViewSelector = mkSelector "setMainView:"

-- | @Selector@ for @initialKeyView@
initialKeyViewSelector :: Selector
initialKeyViewSelector = mkSelector "initialKeyView"

-- | @Selector@ for @setInitialKeyView:@
setInitialKeyViewSelector :: Selector
setInitialKeyViewSelector = mkSelector "setInitialKeyView:"

-- | @Selector@ for @firstKeyView@
firstKeyViewSelector :: Selector
firstKeyViewSelector = mkSelector "firstKeyView"

-- | @Selector@ for @setFirstKeyView:@
setFirstKeyViewSelector :: Selector
setFirstKeyViewSelector = mkSelector "setFirstKeyView:"

-- | @Selector@ for @lastKeyView@
lastKeyViewSelector :: Selector
lastKeyViewSelector = mkSelector "lastKeyView"

-- | @Selector@ for @setLastKeyView:@
setLastKeyViewSelector :: Selector
setLastKeyViewSelector = mkSelector "setLastKeyView:"

-- | @Selector@ for @autoSaveTextFields@
autoSaveTextFieldsSelector :: Selector
autoSaveTextFieldsSelector = mkSelector "autoSaveTextFields"

-- | @Selector@ for @selected@
selectedSelector :: Selector
selectedSelector = mkSelector "selected"

