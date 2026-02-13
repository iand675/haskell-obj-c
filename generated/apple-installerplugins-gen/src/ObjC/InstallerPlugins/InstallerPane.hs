{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , contentViewSelector
  , didEnterPaneSelector
  , didExitPaneSelector
  , firstKeyViewSelector
  , gotoNextPaneSelector
  , gotoPreviousPaneSelector
  , initWithSectionSelector
  , initialKeyViewSelector
  , lastKeyViewSelector
  , nextEnabledSelector
  , nextPaneSelector
  , previousEnabledSelector
  , sectionSelector
  , setContentViewSelector
  , setFirstKeyViewSelector
  , setInitialKeyViewSelector
  , setLastKeyViewSelector
  , setNextEnabledSelector
  , setNextPaneSelector
  , setPreviousEnabledSelector
  , shouldExitPaneSelector
  , titleSelector
  , willEnterPaneSelector
  , willExitPaneSelector

  -- * Enum types
  , InstallerSectionDirection(InstallerSectionDirection)
  , pattern InstallerDirectionForward
  , pattern InstallerDirectionBackward
  , pattern InstallerDirectionUndefined

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithSection installerPane parent =
  sendOwnedMessage installerPane initWithSectionSelector parent

-- | contentView
--
-- Returns the contentView outlet.  ContentView is used to determine what is to				be displayed on screen when this pane is active.  The contentView must be the 				same view when the pane is exited, as when the pane is first entered.
--
-- Subclasses can override this method to return dynamic views which are not loaded				from a nib.
--
-- ObjC selector: @- contentView@
contentView :: IsInstallerPane installerPane => installerPane -> IO (Id NSView)
contentView installerPane =
  sendMessage installerPane contentViewSelector

-- | initialKeyView
--
-- Returns the view that should have the keyboard focus when the pane is entered.				This method returns the initialKeyView outlet.  This outlet should be connected				in the nib containing the pane.
--
-- A subclass can override this method to return a dynamically defined initialKeyView.
--
-- ObjC selector: @- initialKeyView@
initialKeyView :: IsInstallerPane installerPane => installerPane -> IO (Id NSView)
initialKeyView installerPane =
  sendOwnedMessage installerPane initialKeyViewSelector

-- | firstKeyView
--
-- Returns the view that should first have keyboard focus when the content view of the pane				first becomes key.  This method returns the firstKeyView outlet.  This outlet should be connected				in the nib containing the pane.
--
-- A subclass can override this method to return a dynamically defined firstKeyView.
--
-- ObjC selector: @- firstKeyView@
firstKeyView :: IsInstallerPane installerPane => installerPane -> IO (Id NSView)
firstKeyView installerPane =
  sendMessage installerPane firstKeyViewSelector

-- | lastKeyView
--
-- Returns the lastKeyView which has focus before the contentView of the pane is no longer				the key view.
--
-- A subclass can override this method to return a dynamically defined lastKeyView.
--
-- ObjC selector: @- lastKeyView@
lastKeyView :: IsInstallerPane installerPane => installerPane -> IO (Id NSView)
lastKeyView installerPane =
  sendMessage installerPane lastKeyViewSelector

-- | nextPane
--
-- Returns the next InstallerPane to follow this one.  Set the nextPane outlet in a nib to define				a default nextPane.
--
-- A subclass may want to override nextPane if the pane determines it's nextPane dynamically.
--
-- ObjC selector: @- nextPane@
nextPane :: IsInstallerPane installerPane => installerPane -> IO (Id InstallerPane)
nextPane installerPane =
  sendMessage installerPane nextPaneSelector

-- | willEnterPane:
--
-- Called immediatly before the InstallerPane is displayed on the screen.
--
-- @dir@ — The direction in which the pane will be entered from
--
-- ObjC selector: @- willEnterPane:@
willEnterPane :: IsInstallerPane installerPane => installerPane -> InstallerSectionDirection -> IO ()
willEnterPane installerPane dir =
  sendMessage installerPane willEnterPaneSelector dir

-- | didEnterPane:
--
-- Called immediatly after the InstallerPane is displayed on the screen.
--
-- @dir@ — The direction in which the pane was entered
--
-- ObjC selector: @- didEnterPane:@
didEnterPane :: IsInstallerPane installerPane => installerPane -> InstallerSectionDirection -> IO ()
didEnterPane installerPane dir =
  sendMessage installerPane didEnterPaneSelector dir

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
shouldExitPane installerPane dir =
  sendMessage installerPane shouldExitPaneSelector dir

-- | willExitPane:
--
-- Called immediatly before the InstallerPane will exit and be removed from the screen.
--
-- @dir@ — The direction in which the pane will exit to
--
-- ObjC selector: @- willExitPane:@
willExitPane :: IsInstallerPane installerPane => installerPane -> InstallerSectionDirection -> IO ()
willExitPane installerPane dir =
  sendMessage installerPane willExitPaneSelector dir

-- | willExitPane:
--
-- Called immediatly after the InstallerPane has exited and has been removed from the screen.
--
-- @dir@ — The direction in which the pane exited
--
-- ObjC selector: @- didExitPane:@
didExitPane :: IsInstallerPane installerPane => installerPane -> InstallerSectionDirection -> IO ()
didExitPane installerPane dir =
  sendMessage installerPane didExitPaneSelector dir

-- | @- setContentView:@
setContentView :: (IsInstallerPane installerPane, IsNSView value) => installerPane -> value -> IO ()
setContentView installerPane value =
  sendMessage installerPane setContentViewSelector (toNSView value)

-- | @- setInitialKeyView:@
setInitialKeyView :: (IsInstallerPane installerPane, IsNSView value) => installerPane -> value -> IO ()
setInitialKeyView installerPane value =
  sendMessage installerPane setInitialKeyViewSelector (toNSView value)

-- | @- setFirstKeyView:@
setFirstKeyView :: (IsInstallerPane installerPane, IsNSView value) => installerPane -> value -> IO ()
setFirstKeyView installerPane value =
  sendMessage installerPane setFirstKeyViewSelector (toNSView value)

-- | @- setLastKeyView:@
setLastKeyView :: (IsInstallerPane installerPane, IsNSView value) => installerPane -> value -> IO ()
setLastKeyView installerPane value =
  sendMessage installerPane setLastKeyViewSelector (toNSView value)

-- | @- setNextPane:@
setNextPane :: (IsInstallerPane installerPane, IsInstallerPane value) => installerPane -> value -> IO ()
setNextPane installerPane value =
  sendMessage installerPane setNextPaneSelector (toInstallerPane value)

-- | title
--
-- Title text for the pane while being displayed.  The title is retrieved and displayed				every time a pane is entered.  The title must be vaild after the willEnter method is called.
--
-- You must override this method if you would like a title for a custom pane.
--
-- ObjC selector: @- title@
title :: IsInstallerPane installerPane => installerPane -> IO (Id NSString)
title installerPane =
  sendMessage installerPane titleSelector

-- | section
--
-- The parent section for this pane.
--
-- ObjC selector: @- section@
section :: IsInstallerPane installerPane => installerPane -> IO (Id InstallerSection)
section installerPane =
  sendMessage installerPane sectionSelector

-- | nextEnabled
--
-- Specifies whether going to the next pane is enabled in the UI.
--
-- ObjC selector: @- nextEnabled@
nextEnabled :: IsInstallerPane installerPane => installerPane -> IO Bool
nextEnabled installerPane =
  sendMessage installerPane nextEnabledSelector

-- | nextEnabled
--
-- Specifies whether going to the next pane is enabled in the UI.
--
-- ObjC selector: @- setNextEnabled:@
setNextEnabled :: IsInstallerPane installerPane => installerPane -> Bool -> IO ()
setNextEnabled installerPane value =
  sendMessage installerPane setNextEnabledSelector value

-- | previousEnabled
--
-- Specifies whether going to the previous pane is enabled in the UI.
--
-- ObjC selector: @- previousEnabled@
previousEnabled :: IsInstallerPane installerPane => installerPane -> IO Bool
previousEnabled installerPane =
  sendMessage installerPane previousEnabledSelector

-- | previousEnabled
--
-- Specifies whether going to the previous pane is enabled in the UI.
--
-- ObjC selector: @- setPreviousEnabled:@
setPreviousEnabled :: IsInstallerPane installerPane => installerPane -> Bool -> IO ()
setPreviousEnabled installerPane value =
  sendMessage installerPane setPreviousEnabledSelector value

-- | gotoNextPane
--
-- Causes the current pane to exit and the following pane available to be loaded.  gotoNextPane causes this pane's				shouldExit: method to be skipped.
--
-- Returns: gotoNextPane will return NO if there is no nextPane (in any sections) or there is an error loading the nextPane.
--
-- ObjC selector: @- gotoNextPane@
gotoNextPane :: IsInstallerPane installerPane => installerPane -> IO Bool
gotoNextPane installerPane =
  sendMessage installerPane gotoNextPaneSelector

-- | gotoPreviousPane
--
-- Causes the current pane to exit and the previous pane in the Installer's Pane Stack to be displayed.  				gotoNextPane causes this pane's shouldExit: method to be skipped.
--
-- Returns: gotoPreviousPane will return NO if there is no previous (in any sections) or there is an error loading the previousPane.
--
-- ObjC selector: @- gotoPreviousPane@
gotoPreviousPane :: IsInstallerPane installerPane => installerPane -> IO Bool
gotoPreviousPane installerPane =
  sendMessage installerPane gotoPreviousPaneSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSection:@
initWithSectionSelector :: Selector '[RawId] (Id InstallerPane)
initWithSectionSelector = mkSelector "initWithSection:"

-- | @Selector@ for @contentView@
contentViewSelector :: Selector '[] (Id NSView)
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @initialKeyView@
initialKeyViewSelector :: Selector '[] (Id NSView)
initialKeyViewSelector = mkSelector "initialKeyView"

-- | @Selector@ for @firstKeyView@
firstKeyViewSelector :: Selector '[] (Id NSView)
firstKeyViewSelector = mkSelector "firstKeyView"

-- | @Selector@ for @lastKeyView@
lastKeyViewSelector :: Selector '[] (Id NSView)
lastKeyViewSelector = mkSelector "lastKeyView"

-- | @Selector@ for @nextPane@
nextPaneSelector :: Selector '[] (Id InstallerPane)
nextPaneSelector = mkSelector "nextPane"

-- | @Selector@ for @willEnterPane:@
willEnterPaneSelector :: Selector '[InstallerSectionDirection] ()
willEnterPaneSelector = mkSelector "willEnterPane:"

-- | @Selector@ for @didEnterPane:@
didEnterPaneSelector :: Selector '[InstallerSectionDirection] ()
didEnterPaneSelector = mkSelector "didEnterPane:"

-- | @Selector@ for @shouldExitPane:@
shouldExitPaneSelector :: Selector '[InstallerSectionDirection] Bool
shouldExitPaneSelector = mkSelector "shouldExitPane:"

-- | @Selector@ for @willExitPane:@
willExitPaneSelector :: Selector '[InstallerSectionDirection] ()
willExitPaneSelector = mkSelector "willExitPane:"

-- | @Selector@ for @didExitPane:@
didExitPaneSelector :: Selector '[InstallerSectionDirection] ()
didExitPaneSelector = mkSelector "didExitPane:"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector '[Id NSView] ()
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @setInitialKeyView:@
setInitialKeyViewSelector :: Selector '[Id NSView] ()
setInitialKeyViewSelector = mkSelector "setInitialKeyView:"

-- | @Selector@ for @setFirstKeyView:@
setFirstKeyViewSelector :: Selector '[Id NSView] ()
setFirstKeyViewSelector = mkSelector "setFirstKeyView:"

-- | @Selector@ for @setLastKeyView:@
setLastKeyViewSelector :: Selector '[Id NSView] ()
setLastKeyViewSelector = mkSelector "setLastKeyView:"

-- | @Selector@ for @setNextPane:@
setNextPaneSelector :: Selector '[Id InstallerPane] ()
setNextPaneSelector = mkSelector "setNextPane:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @section@
sectionSelector :: Selector '[] (Id InstallerSection)
sectionSelector = mkSelector "section"

-- | @Selector@ for @nextEnabled@
nextEnabledSelector :: Selector '[] Bool
nextEnabledSelector = mkSelector "nextEnabled"

-- | @Selector@ for @setNextEnabled:@
setNextEnabledSelector :: Selector '[Bool] ()
setNextEnabledSelector = mkSelector "setNextEnabled:"

-- | @Selector@ for @previousEnabled@
previousEnabledSelector :: Selector '[] Bool
previousEnabledSelector = mkSelector "previousEnabled"

-- | @Selector@ for @setPreviousEnabled:@
setPreviousEnabledSelector :: Selector '[Bool] ()
setPreviousEnabledSelector = mkSelector "setPreviousEnabled:"

-- | @Selector@ for @gotoNextPane@
gotoNextPaneSelector :: Selector '[] Bool
gotoNextPaneSelector = mkSelector "gotoNextPane"

-- | @Selector@ for @gotoPreviousPane@
gotoPreviousPaneSelector :: Selector '[] Bool
gotoPreviousPaneSelector = mkSelector "gotoPreviousPane"

