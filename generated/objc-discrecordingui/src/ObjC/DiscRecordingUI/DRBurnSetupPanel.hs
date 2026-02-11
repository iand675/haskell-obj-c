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
  , setupPanelSelector
  , setDefaultButtonTitleSelector
  , setCanSelectTestBurnSelector
  , setCanSelectAppendableMediaSelector
  , burnObjectSelector
  , expandSelector
  , burnSpeedSelector
  , appendableSelector
  , completionActionSelector
  , testBurnSelector
  , verifyBurnSelector


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
    sendClassMsg cls' (mkSelector "setupPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setDefaultButtonTitle:
--
-- Sets the title for the receiver's default button to title.
--
-- Normally, the default button is “Burn”.
--
-- ObjC selector: @- setDefaultButtonTitle:@
setDefaultButtonTitle :: (IsDRBurnSetupPanel drBurnSetupPanel, IsNSString title) => drBurnSetupPanel -> title -> IO ()
setDefaultButtonTitle drBurnSetupPanel  title =
withObjCPtr title $ \raw_title ->
    sendMsg drBurnSetupPanel (mkSelector "setDefaultButtonTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

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
setCanSelectTestBurn drBurnSetupPanel  flag =
  sendMsg drBurnSetupPanel (mkSelector "setCanSelectTestBurn:") retVoid [argCULong (if flag then 1 else 0)]

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
setCanSelectAppendableMedia drBurnSetupPanel  flag =
  sendMsg drBurnSetupPanel (mkSelector "setCanSelectAppendableMedia:") retVoid [argCULong (if flag then 1 else 0)]

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
burnObject drBurnSetupPanel  =
  sendMsg drBurnSetupPanel (mkSelector "burnObject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | expand:
--
-- Invoked when the user clicks the panel's expand button.
--
-- ObjC selector: @- expand:@
expand :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
expand drBurnSetupPanel  sender =
  sendMsg drBurnSetupPanel (mkSelector "expand:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | burnSpeed:
--
-- Invoked when the user clicks the panel's burn speed popup button.
--
-- ObjC selector: @- burnSpeed:@
burnSpeed :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
burnSpeed drBurnSetupPanel  sender =
  sendMsg drBurnSetupPanel (mkSelector "burnSpeed:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | appendable:
--
-- Invoked when the user clicks the panel's appendable checkbox.
--
-- ObjC selector: @- appendable:@
appendable :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
appendable drBurnSetupPanel  sender =
  sendMsg drBurnSetupPanel (mkSelector "appendable:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | completionAction:
--
-- Invoked when the user clicks one of the panel's completion action radio buttons.
--
-- ObjC selector: @- completionAction:@
completionAction :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
completionAction drBurnSetupPanel  sender =
  sendMsg drBurnSetupPanel (mkSelector "completionAction:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | testBurn:
--
-- Invoked when the user clicks the panel's test burn checkbox.
--
-- ObjC selector: @- testBurn:@
testBurn :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
testBurn drBurnSetupPanel  sender =
  sendMsg drBurnSetupPanel (mkSelector "testBurn:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | verifyBurn:
--
-- Invoked when the user clicks the panel's verify burn checkbox.
--
-- ObjC selector: @- verifyBurn:@
verifyBurn :: IsDRBurnSetupPanel drBurnSetupPanel => drBurnSetupPanel -> RawId -> IO ()
verifyBurn drBurnSetupPanel  sender =
  sendMsg drBurnSetupPanel (mkSelector "verifyBurn:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setupPanel@
setupPanelSelector :: Selector
setupPanelSelector = mkSelector "setupPanel"

-- | @Selector@ for @setDefaultButtonTitle:@
setDefaultButtonTitleSelector :: Selector
setDefaultButtonTitleSelector = mkSelector "setDefaultButtonTitle:"

-- | @Selector@ for @setCanSelectTestBurn:@
setCanSelectTestBurnSelector :: Selector
setCanSelectTestBurnSelector = mkSelector "setCanSelectTestBurn:"

-- | @Selector@ for @setCanSelectAppendableMedia:@
setCanSelectAppendableMediaSelector :: Selector
setCanSelectAppendableMediaSelector = mkSelector "setCanSelectAppendableMedia:"

-- | @Selector@ for @burnObject@
burnObjectSelector :: Selector
burnObjectSelector = mkSelector "burnObject"

-- | @Selector@ for @expand:@
expandSelector :: Selector
expandSelector = mkSelector "expand:"

-- | @Selector@ for @burnSpeed:@
burnSpeedSelector :: Selector
burnSpeedSelector = mkSelector "burnSpeed:"

-- | @Selector@ for @appendable:@
appendableSelector :: Selector
appendableSelector = mkSelector "appendable:"

-- | @Selector@ for @completionAction:@
completionActionSelector :: Selector
completionActionSelector = mkSelector "completionAction:"

-- | @Selector@ for @testBurn:@
testBurnSelector :: Selector
testBurnSelector = mkSelector "testBurn:"

-- | @Selector@ for @verifyBurn:@
verifyBurnSelector :: Selector
verifyBurnSelector = mkSelector "verifyBurn:"

