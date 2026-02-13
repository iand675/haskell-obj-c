{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , assignMainViewSelector
  , autoSaveTextFieldsSelector
  , bundleSelector
  , didSelectSelector
  , didUnselectSelector
  , firstKeyViewSelector
  , initWithBundleSelector
  , initialKeyViewSelector
  , lastKeyViewSelector
  , loadMainViewSelector
  , mainNibNameSelector
  , mainViewDidLoadSelector
  , mainViewSelector
  , replyToShouldUnselectSelector
  , selectedSelector
  , setFirstKeyViewSelector
  , setInitialKeyViewSelector
  , setLastKeyViewSelector
  , setMainViewSelector
  , shouldUnselectSelector
  , updateHelpMenuWithArraySelector
  , willSelectSelector
  , willUnselectSelector

  -- * Enum types
  , NSPreferencePaneUnselectReply(NSPreferencePaneUnselectReply)
  , pattern NSUnselectCancel
  , pattern NSUnselectNow
  , pattern NSUnselectLater

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PreferencePanes.Internal.Classes
import ObjC.PreferencePanes.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithBundle:@
initWithBundle :: (IsNSPreferencePane nsPreferencePane, IsNSBundle bundle) => nsPreferencePane -> bundle -> IO (Id NSPreferencePane)
initWithBundle nsPreferencePane bundle =
  sendOwnedMessage nsPreferencePane initWithBundleSelector (toNSBundle bundle)

-- | @- loadMainView@
loadMainView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSView)
loadMainView nsPreferencePane =
  sendMessage nsPreferencePane loadMainViewSelector

-- | @- mainViewDidLoad@
mainViewDidLoad :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
mainViewDidLoad nsPreferencePane =
  sendMessage nsPreferencePane mainViewDidLoadSelector

-- | @- assignMainView@
assignMainView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
assignMainView nsPreferencePane =
  sendMessage nsPreferencePane assignMainViewSelector

-- | @- willSelect@
willSelect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
willSelect nsPreferencePane =
  sendMessage nsPreferencePane willSelectSelector

-- | @- didSelect@
didSelect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
didSelect nsPreferencePane =
  sendMessage nsPreferencePane didSelectSelector

-- | @- replyToShouldUnselect:@
replyToShouldUnselect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> Bool -> IO ()
replyToShouldUnselect nsPreferencePane shouldUnselect =
  sendMessage nsPreferencePane replyToShouldUnselectSelector shouldUnselect

-- | @- willUnselect@
willUnselect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
willUnselect nsPreferencePane =
  sendMessage nsPreferencePane willUnselectSelector

-- | @- didUnselect@
didUnselect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO ()
didUnselect nsPreferencePane =
  sendMessage nsPreferencePane didUnselectSelector

-- | @- updateHelpMenuWithArray:@
updateHelpMenuWithArray :: (IsNSPreferencePane nsPreferencePane, IsNSArray inArrayOfMenuItems) => nsPreferencePane -> inArrayOfMenuItems -> IO ()
updateHelpMenuWithArray nsPreferencePane inArrayOfMenuItems =
  sendMessage nsPreferencePane updateHelpMenuWithArraySelector (toNSArray inArrayOfMenuItems)

-- | @- bundle@
bundle :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSBundle)
bundle nsPreferencePane =
  sendMessage nsPreferencePane bundleSelector

-- | @- mainNibName@
mainNibName :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSString)
mainNibName nsPreferencePane =
  sendMessage nsPreferencePane mainNibNameSelector

-- | @- shouldUnselect@
shouldUnselect :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO NSPreferencePaneUnselectReply
shouldUnselect nsPreferencePane =
  sendMessage nsPreferencePane shouldUnselectSelector

-- | @- mainView@
mainView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSView)
mainView nsPreferencePane =
  sendMessage nsPreferencePane mainViewSelector

-- | @- setMainView:@
setMainView :: (IsNSPreferencePane nsPreferencePane, IsNSView value) => nsPreferencePane -> value -> IO ()
setMainView nsPreferencePane value =
  sendMessage nsPreferencePane setMainViewSelector (toNSView value)

-- | @- initialKeyView@
initialKeyView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSView)
initialKeyView nsPreferencePane =
  sendOwnedMessage nsPreferencePane initialKeyViewSelector

-- | @- setInitialKeyView:@
setInitialKeyView :: (IsNSPreferencePane nsPreferencePane, IsNSView value) => nsPreferencePane -> value -> IO ()
setInitialKeyView nsPreferencePane value =
  sendMessage nsPreferencePane setInitialKeyViewSelector (toNSView value)

-- | @- firstKeyView@
firstKeyView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSView)
firstKeyView nsPreferencePane =
  sendMessage nsPreferencePane firstKeyViewSelector

-- | @- setFirstKeyView:@
setFirstKeyView :: (IsNSPreferencePane nsPreferencePane, IsNSView value) => nsPreferencePane -> value -> IO ()
setFirstKeyView nsPreferencePane value =
  sendMessage nsPreferencePane setFirstKeyViewSelector (toNSView value)

-- | @- lastKeyView@
lastKeyView :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO (Id NSView)
lastKeyView nsPreferencePane =
  sendMessage nsPreferencePane lastKeyViewSelector

-- | @- setLastKeyView:@
setLastKeyView :: (IsNSPreferencePane nsPreferencePane, IsNSView value) => nsPreferencePane -> value -> IO ()
setLastKeyView nsPreferencePane value =
  sendMessage nsPreferencePane setLastKeyViewSelector (toNSView value)

-- | @- autoSaveTextFields@
autoSaveTextFields :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO Bool
autoSaveTextFields nsPreferencePane =
  sendMessage nsPreferencePane autoSaveTextFieldsSelector

-- | @- selected@
selected :: IsNSPreferencePane nsPreferencePane => nsPreferencePane -> IO Bool
selected nsPreferencePane =
  sendMessage nsPreferencePane selectedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBundle:@
initWithBundleSelector :: Selector '[Id NSBundle] (Id NSPreferencePane)
initWithBundleSelector = mkSelector "initWithBundle:"

-- | @Selector@ for @loadMainView@
loadMainViewSelector :: Selector '[] (Id NSView)
loadMainViewSelector = mkSelector "loadMainView"

-- | @Selector@ for @mainViewDidLoad@
mainViewDidLoadSelector :: Selector '[] ()
mainViewDidLoadSelector = mkSelector "mainViewDidLoad"

-- | @Selector@ for @assignMainView@
assignMainViewSelector :: Selector '[] ()
assignMainViewSelector = mkSelector "assignMainView"

-- | @Selector@ for @willSelect@
willSelectSelector :: Selector '[] ()
willSelectSelector = mkSelector "willSelect"

-- | @Selector@ for @didSelect@
didSelectSelector :: Selector '[] ()
didSelectSelector = mkSelector "didSelect"

-- | @Selector@ for @replyToShouldUnselect:@
replyToShouldUnselectSelector :: Selector '[Bool] ()
replyToShouldUnselectSelector = mkSelector "replyToShouldUnselect:"

-- | @Selector@ for @willUnselect@
willUnselectSelector :: Selector '[] ()
willUnselectSelector = mkSelector "willUnselect"

-- | @Selector@ for @didUnselect@
didUnselectSelector :: Selector '[] ()
didUnselectSelector = mkSelector "didUnselect"

-- | @Selector@ for @updateHelpMenuWithArray:@
updateHelpMenuWithArraySelector :: Selector '[Id NSArray] ()
updateHelpMenuWithArraySelector = mkSelector "updateHelpMenuWithArray:"

-- | @Selector@ for @bundle@
bundleSelector :: Selector '[] (Id NSBundle)
bundleSelector = mkSelector "bundle"

-- | @Selector@ for @mainNibName@
mainNibNameSelector :: Selector '[] (Id NSString)
mainNibNameSelector = mkSelector "mainNibName"

-- | @Selector@ for @shouldUnselect@
shouldUnselectSelector :: Selector '[] NSPreferencePaneUnselectReply
shouldUnselectSelector = mkSelector "shouldUnselect"

-- | @Selector@ for @mainView@
mainViewSelector :: Selector '[] (Id NSView)
mainViewSelector = mkSelector "mainView"

-- | @Selector@ for @setMainView:@
setMainViewSelector :: Selector '[Id NSView] ()
setMainViewSelector = mkSelector "setMainView:"

-- | @Selector@ for @initialKeyView@
initialKeyViewSelector :: Selector '[] (Id NSView)
initialKeyViewSelector = mkSelector "initialKeyView"

-- | @Selector@ for @setInitialKeyView:@
setInitialKeyViewSelector :: Selector '[Id NSView] ()
setInitialKeyViewSelector = mkSelector "setInitialKeyView:"

-- | @Selector@ for @firstKeyView@
firstKeyViewSelector :: Selector '[] (Id NSView)
firstKeyViewSelector = mkSelector "firstKeyView"

-- | @Selector@ for @setFirstKeyView:@
setFirstKeyViewSelector :: Selector '[Id NSView] ()
setFirstKeyViewSelector = mkSelector "setFirstKeyView:"

-- | @Selector@ for @lastKeyView@
lastKeyViewSelector :: Selector '[] (Id NSView)
lastKeyViewSelector = mkSelector "lastKeyView"

-- | @Selector@ for @setLastKeyView:@
setLastKeyViewSelector :: Selector '[Id NSView] ()
setLastKeyViewSelector = mkSelector "setLastKeyView:"

-- | @Selector@ for @autoSaveTextFields@
autoSaveTextFieldsSelector :: Selector '[] Bool
autoSaveTextFieldsSelector = mkSelector "autoSaveTextFields"

-- | @Selector@ for @selected@
selectedSelector :: Selector '[] Bool
selectedSelector = mkSelector "selected"

