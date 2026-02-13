{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DRBurnSetupPanel
--
-- Manages a panel that allows users to specify the parameters of an burn.
--
-- This class supports choosing the the device to use, whether or not				to verify the burned data and how to handle the burned disc when it completes.
--
-- Generated bindings for @DRBurnSetupPanel@.
module ObjC.DiscRecordingUI.DRBurnSetupPanel
  ( DRBurnSetupPanel
  , IsDRBurnSetupPanel(..)
  , setupPanel
  , setDefaultButtonTitle
  , setCanSelectTestBurn
  , setCanSelectAppendableMedia
  , burnObject
  , expand
  , burnSpeed
  , appendable
  , completionAction
  , testBurn
  , verifyBurn
  , appendableSelector
  , burnObjectSelector
  , burnSpeedSelector
  , completionActionSelector
  , expandSelector
  , setCanSelectAppendableMediaSelector
  , setCanSelectTestBurnSelector
  , setDefaultButtonTitleSelector
  , setupPanelSelector
  , testBurnSelector
  , verifyBurnSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DiscRecordingUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setupPanel
--
-- Creates and return an instance of a burn setup panel.
--
-- Returns: A pointer to the newly created DRBurnSetupPanel.
--
-- ObjC selector: @+ setupPanel@
setupPanel :: IO (Id DRBurnSetupPanel)
setupPanel  =
  do
    cls' <- getRequiredClass "DRBurnSetupPanel"
    sendClassMessage cls' setupPanelSelector

-- | setDefaultButtonTitle:
--
-- Sets the title for the receiver's default button to title.
--
-- Normally, the default button is “Burn”.
--
-- ObjC selector: @- setDefaultButtonTitle:@
setDefaultButtonTitle :: (IsDRBurnSetupPanel drBurnSetupPanel, IsNSString title) => drBurnSetupPanel -> title -> IO ()
setDefaultButtonTitle drBurnSetupPanel title =
  sendMessage drBurnSetupPanel setDefaultButtonTitleSelector (toNSString title)

-- | setCanSelectTestBurn:
--
-- Specifies whether the user can choose to make a test burn.
--
-- This method controls whether a checkbox should be added to the				receiver that allows the user to set the burn to be a test burn.				By default, the test burn button is not displayed.
--
-- This method must be called before the panel is displayed.
--
-- @flag@ — YES to show the test burn checkbox, NO to hide it.
--
-- ObjC selector: @- setCanSelectTestBurn:@
setCanSelectTestBurn :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> Bool -> IO ()
setCanSelectTestBurn drBurnSetupPanel flag =
  sendMessage drBurnSetupPanel setCanSelectTestBurnSelector flag

-- | setCanSelectAppendableMedia:
--
-- Specifies whether the user can choose to leave the disc appendable.
--
-- This method controls whether the appendable checkbox is enabled.
--
-- If the data being writen to disc does not lend itself to having more data				appended on to it, you can disable the ability of the user to leave the disc				open.
--
-- This method must be called before the panel is displayed.
--
-- @flag@ — YES to enable the appendable checkbox, NO to disable.
--
-- ObjC selector: @- setCanSelectAppendableMedia:@
setCanSelectAppendableMedia :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> Bool -> IO ()
setCanSelectAppendableMedia drBurnSetupPanel flag =
  sendMessage drBurnSetupPanel setCanSelectAppendableMediaSelector flag

-- | burnObject
--
-- Creates and returns a new DRBurn object that's configured to write				data to the currently selected device.
--
-- The new DRBurn object is configured based on the settings in the setup panel				when the user clicks the OK button.
--
-- Do not invoke this method within a modal session (
--
-- //apple_ref/occ/instm/DRSetupPanel/runSetupPanel runSetupPanel
--
-- or
--
-- //apple_ref/occ/instm/DRSetupPanel/beginSetupSheetForWindow:modalDelegate:didEndSelector:contextInfo: beginSetupSheetForWindow:modalDelegate:didEndSelector:contextInfo:
--
-- )				because the burn object information is only updated just before the				modal session ends.
--
-- Returns: A new DRBurn object.
--
-- ObjC selector: @- burnObject@
burnObject :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> IO (Id DRBurn)
burnObject drBurnSetupPanel =
  sendMessage drBurnSetupPanel burnObjectSelector

-- | expand:
--
-- Invoked when the user clicks the panel's expand button.
--
-- ObjC selector: @- expand:@
expand :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
expand drBurnSetupPanel sender =
  sendMessage drBurnSetupPanel expandSelector sender

-- | burnSpeed:
--
-- Invoked when the user clicks the panel's burn speed popup button.
--
-- ObjC selector: @- burnSpeed:@
burnSpeed :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
burnSpeed drBurnSetupPanel sender =
  sendMessage drBurnSetupPanel burnSpeedSelector sender

-- | appendable:
--
-- Invoked when the user clicks the panel's appendable checkbox.
--
-- ObjC selector: @- appendable:@
appendable :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
appendable drBurnSetupPanel sender =
  sendMessage drBurnSetupPanel appendableSelector sender

-- | completionAction:
--
-- Invoked when the user clicks one of the panel's completion action radio buttons.
--
-- ObjC selector: @- completionAction:@
completionAction :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
completionAction drBurnSetupPanel sender =
  sendMessage drBurnSetupPanel completionActionSelector sender

-- | testBurn:
--
-- Invoked when the user clicks the panel's test burn checkbox.
--
-- ObjC selector: @- testBurn:@
testBurn :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
testBurn drBurnSetupPanel sender =
  sendMessage drBurnSetupPanel testBurnSelector sender

-- | verifyBurn:
--
-- Invoked when the user clicks the panel's verify burn checkbox.
--
-- ObjC selector: @- verifyBurn:@
verifyBurn :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
verifyBurn drBurnSetupPanel sender =
  sendMessage drBurnSetupPanel verifyBurnSelector sender

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setupPanel@
setupPanelSelector :: Selector '[] (Id DRBurnSetupPanel)
setupPanelSelector = mkSelector "setupPanel"

-- | @Selector@ for @setDefaultButtonTitle:@
setDefaultButtonTitleSelector :: Selector '[Id NSString] ()
setDefaultButtonTitleSelector = mkSelector "setDefaultButtonTitle:"

-- | @Selector@ for @setCanSelectTestBurn:@
setCanSelectTestBurnSelector :: Selector '[Bool] ()
setCanSelectTestBurnSelector = mkSelector "setCanSelectTestBurn:"

-- | @Selector@ for @setCanSelectAppendableMedia:@
setCanSelectAppendableMediaSelector :: Selector '[Bool] ()
setCanSelectAppendableMediaSelector = mkSelector "setCanSelectAppendableMedia:"

-- | @Selector@ for @burnObject@
burnObjectSelector :: Selector '[] (Id DRBurn)
burnObjectSelector = mkSelector "burnObject"

-- | @Selector@ for @expand:@
expandSelector :: Selector '[RawId] ()
expandSelector = mkSelector "expand:"

-- | @Selector@ for @burnSpeed:@
burnSpeedSelector :: Selector '[RawId] ()
burnSpeedSelector = mkSelector "burnSpeed:"

-- | @Selector@ for @appendable:@
appendableSelector :: Selector '[RawId] ()
appendableSelector = mkSelector "appendable:"

-- | @Selector@ for @completionAction:@
completionActionSelector :: Selector '[RawId] ()
completionActionSelector = mkSelector "completionAction:"

-- | @Selector@ for @testBurn:@
testBurnSelector :: Selector '[RawId] ()
testBurnSelector = mkSelector "testBurn:"

-- | @Selector@ for @verifyBurn:@
verifyBurnSelector :: Selector '[RawId] ()
verifyBurnSelector = mkSelector "verifyBurn:"

