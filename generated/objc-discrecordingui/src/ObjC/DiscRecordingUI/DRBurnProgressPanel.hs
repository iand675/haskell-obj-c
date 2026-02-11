{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DRBurnProgressPanel
--
-- Manages a panel that displays progress while burning data to media.
--
-- A DRBurnProgressPanel object manages a panel that displays 					and updates burn progress. The burn panel is responsible 					for begining the burn.
--
-- The burn is begun and a progress panel is displayed on screen 					by calling
--
-- //apple_ref/occ/instm/DRBurnProgressPanel/beginProgressSheetForBurn:layout:modalForWindow: beginProgressSheetForBurn:layout:modalForWindow:
--
-- if a sheet interface is desired, or
--
-- //apple_ref/occ/instm/DRBurnProgressPanel/beginProgressPanelForBurn:layout: beginProgressPanelForBurn:layout:
--
-- for a non-modal panel.
--
-- A DRBurnProgressPanel sends a
--
-- //apple_ref/occ/instm/NSObject/burnProgressPanel:burnDidFinish: burnProgressPanel:burnDidFinish:
--
-- message to it's delegate 					when the burn completes. This method allows the delegate 					to take over end-of-burn handling from the burn progress 					panel to customize error dialogs or user notification.
--
-- Generated bindings for @DRBurnProgressPanel@.
module ObjC.DiscRecordingUI.DRBurnProgressPanel
  ( DRBurnProgressPanel
  , IsDRBurnProgressPanel(..)
  , progressPanel
  , beginProgressSheetForBurn_layout_modalForWindow
  , beginProgressPanelForBurn_layout
  , setDescription
  , description
  , setVerboseProgressStatus
  , verboseProgressStatus
  , stopBurn
  , progressPanelSelector
  , beginProgressSheetForBurn_layout_modalForWindowSelector
  , beginProgressPanelForBurn_layoutSelector
  , setDescriptionSelector
  , descriptionSelector
  , setVerboseProgressStatusSelector
  , verboseProgressStatusSelector
  , stopBurnSelector


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

-- | progressPanel
--
-- Creates and returns an instance of the burn progress panel.
--
-- Returns: A pointer to the newly created DRBurnProgressPanel.
--
-- ObjC selector: @+ progressPanel@
progressPanel :: IO (Id DRBurnProgressPanel)
progressPanel  =
  do
    cls' <- getRequiredClass "DRBurnProgressPanel"
    sendClassMsg cls' (mkSelector "progressPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | beginProgressSheetForBurn:layout:modalForWindow:
--
-- Presents the progress panel as a sheet and begins the burn process.
--
-- This method returns control to the caller after it has displayed the 					progress sheet and begun the burn. Once the method has returned the					caller can perform other operations while the burn continues.
--
-- @burn@ — The object performing the burn.
--
-- @layout@ — The data to be burned to disc. See the
--
-- //apple_ref/occ/cl/DRBurn DRBurn
--
-- documentation for information on								valid layouts.
--
-- @docWindow@ — The window the sheet will be attached to. If docWindow is 								not nil, the panel slides down as a sheet running as a 								document modal window. If owner is nil, this is an error.
--
-- ObjC selector: @- beginProgressSheetForBurn:layout:modalForWindow:@
beginProgressSheetForBurn_layout_modalForWindow :: (IsDRBurnProgressPanel drBurnProgressPanel, IsDRBurn burn, IsNSWindow docWindow) => drBurnProgressPanel -> burn -> RawId -> docWindow -> IO ()
beginProgressSheetForBurn_layout_modalForWindow drBurnProgressPanel  burn layout docWindow =
withObjCPtr burn $ \raw_burn ->
  withObjCPtr docWindow $ \raw_docWindow ->
      sendMsg drBurnProgressPanel (mkSelector "beginProgressSheetForBurn:layout:modalForWindow:") retVoid [argPtr (castPtr raw_burn :: Ptr ()), argPtr (castPtr (unRawId layout) :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ())]

-- | beginProgressPanelForBurn:layout:
--
-- Presents the progress panel on screen and begins the burn process.
--
-- This method returns control to the caller after it has displayed the 					progress sheet and begun the burn. Once the method has returned the					caller can perform other operations while the burn continues.
--
-- @burn@ — The object performing the burn.
--
-- @layout@ — The data to be burned to disc. See the
--
-- //apple_ref/occ/cl/DRBurn DRBurn
--
-- documentation for information on								valid layouts.
--
-- ObjC selector: @- beginProgressPanelForBurn:layout:@
beginProgressPanelForBurn_layout :: (IsDRBurnProgressPanel drBurnProgressPanel, IsDRBurn burn) => drBurnProgressPanel -> burn -> RawId -> IO ()
beginProgressPanelForBurn_layout drBurnProgressPanel  burn layout =
withObjCPtr burn $ \raw_burn ->
    sendMsg drBurnProgressPanel (mkSelector "beginProgressPanelForBurn:layout:") retVoid [argPtr (castPtr raw_burn :: Ptr ()), argPtr (castPtr (unRawId layout) :: Ptr ())]

-- | setDescription:
--
-- Sets the panel text displayed to the user.
--
-- The panel's description is typically a short text string that gives an 					indication to the user what operation is being performed. If no description					is explicitly set, the progress panel uses a standard text string suitable					to the burn.
--
-- @description@ — The text to display.
--
-- ObjC selector: @- setDescription:@
setDescription :: (IsDRBurnProgressPanel drBurnProgressPanel, IsNSString description) => drBurnProgressPanel -> description -> IO ()
setDescription drBurnProgressPanel  description =
withObjCPtr description $ \raw_description ->
    sendMsg drBurnProgressPanel (mkSelector "setDescription:") retVoid [argPtr (castPtr raw_description :: Ptr ())]

-- | description
--
-- Returns the description string displayed in the panel.
--
-- If no description is explicitly set, this method will return the standard					text string.
--
-- Returns: An NSString containing the text of the description.
--
-- ObjC selector: @- description@
description :: IsDRBurnProgressPanel drBurnProgressPanel => drBurnProgressPanel -> IO (Id NSString)
description drBurnProgressPanel  =
  sendMsg drBurnProgressPanel (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setVerboseProgressStatus:
--
-- Sets the vebosity of the progress feedback.
--
-- If verbose is YES, the panel will update status for every change. 					If verbose is NO, the panel will filter some status messages and 					only update for major changes. The default for the panel is filter the 					status messages.
--
-- @verbose@ — A BOOL value indicating how detailed the status panel feedback should be.
--
-- ObjC selector: @- setVerboseProgressStatus:@
setVerboseProgressStatus :: IsDRBurnProgressPanel drBurnProgressPanel => drBurnProgressPanel -> Bool -> IO ()
setVerboseProgressStatus drBurnProgressPanel  verbose =
  sendMsg drBurnProgressPanel (mkSelector "setVerboseProgressStatus:") retVoid [argCULong (if verbose then 1 else 0)]

-- | verboseProgressStatus
--
-- Returns the vebosity of the panel.
--
-- This method will return YES if the panel will update status 					for every change and NO if the panel will filter some status 					messages and only update for major changes.
--
-- Returns: A BOOL value indicating how detailed the status panel feedback is.
--
-- ObjC selector: @- verboseProgressStatus@
verboseProgressStatus :: IsDRBurnProgressPanel drBurnProgressPanel => drBurnProgressPanel -> IO Bool
verboseProgressStatus drBurnProgressPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drBurnProgressPanel (mkSelector "verboseProgressStatus") retCULong []

-- | stopBurn:
--
-- Invoked when the user clicks the panel's stop button.
--
-- ObjC selector: @- stopBurn:@
stopBurn :: IsDRBurnProgressPanel drBurnProgressPanel => drBurnProgressPanel -> RawId -> IO ()
stopBurn drBurnProgressPanel  sender =
  sendMsg drBurnProgressPanel (mkSelector "stopBurn:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @progressPanel@
progressPanelSelector :: Selector
progressPanelSelector = mkSelector "progressPanel"

-- | @Selector@ for @beginProgressSheetForBurn:layout:modalForWindow:@
beginProgressSheetForBurn_layout_modalForWindowSelector :: Selector
beginProgressSheetForBurn_layout_modalForWindowSelector = mkSelector "beginProgressSheetForBurn:layout:modalForWindow:"

-- | @Selector@ for @beginProgressPanelForBurn:layout:@
beginProgressPanelForBurn_layoutSelector :: Selector
beginProgressPanelForBurn_layoutSelector = mkSelector "beginProgressPanelForBurn:layout:"

-- | @Selector@ for @setDescription:@
setDescriptionSelector :: Selector
setDescriptionSelector = mkSelector "setDescription:"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

-- | @Selector@ for @setVerboseProgressStatus:@
setVerboseProgressStatusSelector :: Selector
setVerboseProgressStatusSelector = mkSelector "setVerboseProgressStatus:"

-- | @Selector@ for @verboseProgressStatus@
verboseProgressStatusSelector :: Selector
verboseProgressStatusSelector = mkSelector "verboseProgressStatus"

-- | @Selector@ for @stopBurn:@
stopBurnSelector :: Selector
stopBurnSelector = mkSelector "stopBurn:"

