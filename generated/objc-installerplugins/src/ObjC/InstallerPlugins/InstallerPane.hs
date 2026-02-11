{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @InstallerPane@.
module ObjC.InstallerPlugins.InstallerPane
  ( InstallerPane
  , IsInstallerPane(..)
  , initWithSection
  , contentView
  , initialKeyView
  , firstKeyView
  , lastKeyView
  , nextPane
  , willEnterPane
  , didEnterPane
  , shouldExitPane
  , willExitPane
  , didExitPane
  , setContentView
  , setInitialKeyView
  , setFirstKeyView
  , setLastKeyView
  , setNextPane
  , title
  , section
  , nextEnabled
  , setNextEnabled
  , previousEnabled
  , setPreviousEnabled
  , gotoNextPane
  , gotoPreviousPane
  , initWithSectionSelector
  , contentViewSelector
  , initialKeyViewSelector
  , firstKeyViewSelector
  , lastKeyViewSelector
  , nextPaneSelector
  , willEnterPaneSelector
  , didEnterPaneSelector
  , shouldExitPaneSelector
  , willExitPaneSelector
  , didExitPaneSelector
  , setContentViewSelector
  , setInitialKeyViewSelector
  , setFirstKeyViewSelector
  , setLastKeyViewSelector
  , setNextPaneSelector
  , titleSelector
  , sectionSelector
  , nextEnabledSelector
  , setNextEnabledSelector
  , previousEnabledSelector
  , setPreviousEnabledSelector
  , gotoNextPaneSelector
  , gotoPreviousPaneSelector

  -- * Enum types
  , InstallerSectionDirection(InstallerSectionDirection)
  , pattern InstallerDirectionForward
  , pattern InstallerDirectionBackward
  , pattern InstallerDirectionUndefined

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

import ObjC.InstallerPlugins.Internal.Classes
import ObjC.InstallerPlugins.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithSection
--
-- Init method for InstallerPane.  This method takes it's parent section as an argument.
--
-- ObjC selector: @- initWithSection:@
initWithSection :: IsInstallerPane installerPane => installerPane -> RawId -> IO (Id InstallerPane)
initWithSection installerPane  parent =
  sendMsg installerPane (mkSelector "initWithSection:") (retPtr retVoid) [argPtr (castPtr (unRawId parent) :: Ptr ())] >>= ownedObject . castPtr

-- | contentView
--
-- Returns the contentView outlet.  ContentView is used to determine what is to				be displayed on screen when this pane is active.  The contentView must be the 				same view when the pane is exited, as when the pane is first entered.
--
-- Subclasses can override this method to return dynamic views which are not loaded				from a nib.
--
-- ObjC selector: @- contentView@
contentView :: IsInstallerPane installerPane => installerPane -> IO (Id NSView)
contentView installerPane  =
  sendMsg installerPane (mkSelector "contentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | initialKeyView
--
-- Returns the view that should have the keyboard focus when the pane is entered.				This method returns the initialKeyView outlet.  This outlet should be connected				in the nib containing the pane.
--
-- A subclass can override this method to return a dynamically defined initialKeyView.
--
-- ObjC selector: @- initialKeyView@
initialKeyView :: IsInstallerPane installerPane => installerPane -> IO (Id NSView)
initialKeyView installerPane  =
  sendMsg installerPane (mkSelector "initialKeyView") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | firstKeyView
--
-- Returns the view that should first have keyboard focus when the content view of the pane				first becomes key.  This method returns the firstKeyView outlet.  This outlet should be connected				in the nib containing the pane.
--
-- A subclass can override this method to return a dynamically defined firstKeyView.
--
-- ObjC selector: @- firstKeyView@
firstKeyView :: IsInstallerPane installerPane => installerPane -> IO (Id NSView)
firstKeyView installerPane  =
  sendMsg installerPane (mkSelector "firstKeyView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | lastKeyView
--
-- Returns the lastKeyView which has focus before the contentView of the pane is no longer				the key view.
--
-- A subclass can override this method to return a dynamically defined lastKeyView.
--
-- ObjC selector: @- lastKeyView@
lastKeyView :: IsInstallerPane installerPane => installerPane -> IO (Id NSView)
lastKeyView installerPane  =
  sendMsg installerPane (mkSelector "lastKeyView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | nextPane
--
-- Returns the next InstallerPane to follow this one.  Set the nextPane outlet in a nib to define				a default nextPane.
--
-- A subclass may want to override nextPane if the pane determines it's nextPane dynamically.
--
-- ObjC selector: @- nextPane@
nextPane :: IsInstallerPane installerPane => installerPane -> IO (Id InstallerPane)
nextPane installerPane  =
  sendMsg installerPane (mkSelector "nextPane") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | willEnterPane:
--
-- Called immediatly before the InstallerPane is displayed on the screen.
--
-- @dir@ — The direction in which the pane will be entered from
--
-- ObjC selector: @- willEnterPane:@
willEnterPane :: IsInstallerPane installerPane => installerPane -> InstallerSectionDirection -> IO ()
willEnterPane installerPane  dir =
  sendMsg installerPane (mkSelector "willEnterPane:") retVoid [argCLong (coerce dir)]

-- | didEnterPane:
--
-- Called immediatly after the InstallerPane is displayed on the screen.
--
-- @dir@ — The direction in which the pane was entered
--
-- ObjC selector: @- didEnterPane:@
didEnterPane :: IsInstallerPane installerPane => installerPane -> InstallerSectionDirection -> IO ()
didEnterPane installerPane  dir =
  sendMsg installerPane (mkSelector "didEnterPane:") retVoid [argCLong (coerce dir)]

-- | shouldExitPane:
--
-- Called to determine if a pane should exit and allow another pane to be display on screen.
--
-- A subclass should override this method if it needs to prevent the InstallerPane from exiting.
--
-- Once the InstallerPane decides it is time to exit, it can call gotoNextPane or gotoPreviousPane				to exit without calling shouldExitPane again.
--
-- @dir@ — The direction in which the pane was entered
--
-- Returns: Yes or No defining if the pane should really exit
--
-- ObjC selector: @- shouldExitPane:@
shouldExitPane :: IsInstallerPane installerPane => installerPane -> InstallerSectionDirection -> IO Bool
shouldExitPane installerPane  dir =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg installerPane (mkSelector "shouldExitPane:") retCULong [argCLong (coerce dir)]

-- | willExitPane:
--
-- Called immediatly before the InstallerPane will exit and be removed from the screen.
--
-- @dir@ — The direction in which the pane will exit to
--
-- ObjC selector: @- willExitPane:@
willExitPane :: IsInstallerPane installerPane => installerPane -> InstallerSectionDirection -> IO ()
willExitPane installerPane  dir =
  sendMsg installerPane (mkSelector "willExitPane:") retVoid [argCLong (coerce dir)]

-- | willExitPane:
--
-- Called immediatly after the InstallerPane has exited and has been removed from the screen.
--
-- @dir@ — The direction in which the pane exited
--
-- ObjC selector: @- didExitPane:@
didExitPane :: IsInstallerPane installerPane => installerPane -> InstallerSectionDirection -> IO ()
didExitPane installerPane  dir =
  sendMsg installerPane (mkSelector "didExitPane:") retVoid [argCLong (coerce dir)]

-- | @- setContentView:@
setContentView :: (IsInstallerPane installerPane, IsNSView value) => installerPane -> value -> IO ()
setContentView installerPane  value =
withObjCPtr value $ \raw_value ->
    sendMsg installerPane (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- setInitialKeyView:@
setInitialKeyView :: (IsInstallerPane installerPane, IsNSView value) => installerPane -> value -> IO ()
setInitialKeyView installerPane  value =
withObjCPtr value $ \raw_value ->
    sendMsg installerPane (mkSelector "setInitialKeyView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- setFirstKeyView:@
setFirstKeyView :: (IsInstallerPane installerPane, IsNSView value) => installerPane -> value -> IO ()
setFirstKeyView installerPane  value =
withObjCPtr value $ \raw_value ->
    sendMsg installerPane (mkSelector "setFirstKeyView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- setLastKeyView:@
setLastKeyView :: (IsInstallerPane installerPane, IsNSView value) => installerPane -> value -> IO ()
setLastKeyView installerPane  value =
withObjCPtr value $ \raw_value ->
    sendMsg installerPane (mkSelector "setLastKeyView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- setNextPane:@
setNextPane :: (IsInstallerPane installerPane, IsInstallerPane value) => installerPane -> value -> IO ()
setNextPane installerPane  value =
withObjCPtr value $ \raw_value ->
    sendMsg installerPane (mkSelector "setNextPane:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | title
--
-- Title text for the pane while being displayed.  The title is retrieved and displayed				every time a pane is entered.  The title must be vaild after the willEnter method is called.
--
-- You must override this method if you would like a title for a custom pane.
--
-- ObjC selector: @- title@
title :: IsInstallerPane installerPane => installerPane -> IO (Id NSString)
title installerPane  =
  sendMsg installerPane (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | section
--
-- The parent section for this pane.
--
-- ObjC selector: @- section@
section :: IsInstallerPane installerPane => installerPane -> IO (Id InstallerSection)
section installerPane  =
  sendMsg installerPane (mkSelector "section") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | nextEnabled
--
-- Specifies whether going to the next pane is enabled in the UI.
--
-- ObjC selector: @- nextEnabled@
nextEnabled :: IsInstallerPane installerPane => installerPane -> IO Bool
nextEnabled installerPane  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg installerPane (mkSelector "nextEnabled") retCULong []

-- | nextEnabled
--
-- Specifies whether going to the next pane is enabled in the UI.
--
-- ObjC selector: @- setNextEnabled:@
setNextEnabled :: IsInstallerPane installerPane => installerPane -> Bool -> IO ()
setNextEnabled installerPane  value =
  sendMsg installerPane (mkSelector "setNextEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | previousEnabled
--
-- Specifies whether going to the previous pane is enabled in the UI.
--
-- ObjC selector: @- previousEnabled@
previousEnabled :: IsInstallerPane installerPane => installerPane -> IO Bool
previousEnabled installerPane  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg installerPane (mkSelector "previousEnabled") retCULong []

-- | previousEnabled
--
-- Specifies whether going to the previous pane is enabled in the UI.
--
-- ObjC selector: @- setPreviousEnabled:@
setPreviousEnabled :: IsInstallerPane installerPane => installerPane -> Bool -> IO ()
setPreviousEnabled installerPane  value =
  sendMsg installerPane (mkSelector "setPreviousEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | gotoNextPane
--
-- Causes the current pane to exit and the following pane available to be loaded.  gotoNextPane causes this pane's				shouldExit: method to be skipped.
--
-- Returns: gotoNextPane will return NO if there is no nextPane (in any sections) or there is an error loading the nextPane.
--
-- ObjC selector: @- gotoNextPane@
gotoNextPane :: IsInstallerPane installerPane => installerPane -> IO Bool
gotoNextPane installerPane  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg installerPane (mkSelector "gotoNextPane") retCULong []

-- | gotoPreviousPane
--
-- Causes the current pane to exit and the previous pane in the Installer's Pane Stack to be displayed.  				gotoNextPane causes this pane's shouldExit: method to be skipped.
--
-- Returns: gotoPreviousPane will return NO if there is no previous (in any sections) or there is an error loading the previousPane.
--
-- ObjC selector: @- gotoPreviousPane@
gotoPreviousPane :: IsInstallerPane installerPane => installerPane -> IO Bool
gotoPreviousPane installerPane  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg installerPane (mkSelector "gotoPreviousPane") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSection:@
initWithSectionSelector :: Selector
initWithSectionSelector = mkSelector "initWithSection:"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @initialKeyView@
initialKeyViewSelector :: Selector
initialKeyViewSelector = mkSelector "initialKeyView"

-- | @Selector@ for @firstKeyView@
firstKeyViewSelector :: Selector
firstKeyViewSelector = mkSelector "firstKeyView"

-- | @Selector@ for @lastKeyView@
lastKeyViewSelector :: Selector
lastKeyViewSelector = mkSelector "lastKeyView"

-- | @Selector@ for @nextPane@
nextPaneSelector :: Selector
nextPaneSelector = mkSelector "nextPane"

-- | @Selector@ for @willEnterPane:@
willEnterPaneSelector :: Selector
willEnterPaneSelector = mkSelector "willEnterPane:"

-- | @Selector@ for @didEnterPane:@
didEnterPaneSelector :: Selector
didEnterPaneSelector = mkSelector "didEnterPane:"

-- | @Selector@ for @shouldExitPane:@
shouldExitPaneSelector :: Selector
shouldExitPaneSelector = mkSelector "shouldExitPane:"

-- | @Selector@ for @willExitPane:@
willExitPaneSelector :: Selector
willExitPaneSelector = mkSelector "willExitPane:"

-- | @Selector@ for @didExitPane:@
didExitPaneSelector :: Selector
didExitPaneSelector = mkSelector "didExitPane:"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @setInitialKeyView:@
setInitialKeyViewSelector :: Selector
setInitialKeyViewSelector = mkSelector "setInitialKeyView:"

-- | @Selector@ for @setFirstKeyView:@
setFirstKeyViewSelector :: Selector
setFirstKeyViewSelector = mkSelector "setFirstKeyView:"

-- | @Selector@ for @setLastKeyView:@
setLastKeyViewSelector :: Selector
setLastKeyViewSelector = mkSelector "setLastKeyView:"

-- | @Selector@ for @setNextPane:@
setNextPaneSelector :: Selector
setNextPaneSelector = mkSelector "setNextPane:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @section@
sectionSelector :: Selector
sectionSelector = mkSelector "section"

-- | @Selector@ for @nextEnabled@
nextEnabledSelector :: Selector
nextEnabledSelector = mkSelector "nextEnabled"

-- | @Selector@ for @setNextEnabled:@
setNextEnabledSelector :: Selector
setNextEnabledSelector = mkSelector "setNextEnabled:"

-- | @Selector@ for @previousEnabled@
previousEnabledSelector :: Selector
previousEnabledSelector = mkSelector "previousEnabled"

-- | @Selector@ for @setPreviousEnabled:@
setPreviousEnabledSelector :: Selector
setPreviousEnabledSelector = mkSelector "setPreviousEnabled:"

-- | @Selector@ for @gotoNextPane@
gotoNextPaneSelector :: Selector
gotoNextPaneSelector = mkSelector "gotoNextPane"

-- | @Selector@ for @gotoPreviousPane@
gotoPreviousPaneSelector :: Selector
gotoPreviousPaneSelector = mkSelector "gotoPreviousPane"

