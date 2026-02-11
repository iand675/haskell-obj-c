{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DREraseProgressPanel
--
-- Manages a panel that displays progress while erasing media.
--
-- A DREraseProgressPanel object manages a panel that displays 					and updates erase progress. The erase panel is responsible 					for begining the erase.
--
-- The erase is begun and a progress panel is displayed on screen 					by calling
--
-- //apple_ref/occ/instm/DREraseProgressPanel/beginProgressSheetForErase:modalForWindow: beginProgressSheetForErase:modalForWindow:
--
-- if a sheet interface is desired, or
--
-- //apple_ref/occ/instm/DREraseProgressPanel/beginProgressPanelForErase: beginProgressPanelForErase:
--
-- for a non-modal panel.
--
-- A DREraseProgressPanel sends a
--
-- //apple_ref/occ/instm/NSObject/eraseProgressPanel:eraseDidFinish: eraseProgressPanel:eraseDidFinish:
--
-- message to it's delegate 					when the erase completes. This method allows the delegate 					to take over end-of-erase handling from the erase progress 					panel to customize error dialogs or user notification.
--
-- Generated bindings for @DREraseProgressPanel@.
module ObjC.DiscRecordingUI.DREraseProgressPanel
  ( DREraseProgressPanel
  , IsDREraseProgressPanel(..)
  , progressPanel
  , beginProgressSheetForErase_modalForWindow
  , beginProgressPanelForErase
  , setDescription
  , description
  , progressPanelSelector
  , beginProgressSheetForErase_modalForWindowSelector
  , beginProgressPanelForEraseSelector
  , setDescriptionSelector
  , descriptionSelector


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
-- Creates and returns an instance of the erase progress panel.
--
-- Returns: A pointer to the newly created DREraseProgressPanel.
--
-- ObjC selector: @+ progressPanel@
progressPanel :: IO (Id DREraseProgressPanel)
progressPanel  =
  do
    cls' <- getRequiredClass "DREraseProgressPanel"
    sendClassMsg cls' (mkSelector "progressPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | beginProgressSheetForErase:modalForWindow:
--
-- Presents the progress panel as a sheet and begins the erase process.
--
-- This method returns control to the caller after it has displayed the 					progress sheet and begun the erase. Once the method has returned the					caller can perform other operations while the erase continues.
--
-- @erase@ — The object performing the erase.
--
-- @docWindow@ — The window the sheet will be attached to. If docWindow is 								not nil, the panel slides down as a sheet running as a 								document modal window. If owner is nil, this is an error.
--
-- ObjC selector: @- beginProgressSheetForErase:modalForWindow:@
beginProgressSheetForErase_modalForWindow :: (IsDREraseProgressPanel drEraseProgressPanel, IsDRErase erase, IsNSWindow docWindow) => drEraseProgressPanel -> erase -> docWindow -> IO ()
beginProgressSheetForErase_modalForWindow drEraseProgressPanel  erase docWindow =
withObjCPtr erase $ \raw_erase ->
  withObjCPtr docWindow $ \raw_docWindow ->
      sendMsg drEraseProgressPanel (mkSelector "beginProgressSheetForErase:modalForWindow:") retVoid [argPtr (castPtr raw_erase :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ())]

-- | beginProgressPanelForErase:
--
-- Presents the progress panel on screen and begins the erase process.
--
-- This method returns control to the caller after it has displayed the 					progress sheet and begun the erase. Once the method has returned the					caller can perform other operations while the erase continues.
--
-- @erase@ — The object performing the erase.
--
-- ObjC selector: @- beginProgressPanelForErase:@
beginProgressPanelForErase :: (IsDREraseProgressPanel drEraseProgressPanel, IsDRErase erase) => drEraseProgressPanel -> erase -> IO ()
beginProgressPanelForErase drEraseProgressPanel  erase =
withObjCPtr erase $ \raw_erase ->
    sendMsg drEraseProgressPanel (mkSelector "beginProgressPanelForErase:") retVoid [argPtr (castPtr raw_erase :: Ptr ())]

-- | setDescription:
--
-- Sets the panel text displayed to the user.
--
-- The panel's description is typically a short text string that gives an 					indication to the user what operation is being performed. If no description					is explicitly set, the progress panel uses a standard text string suitable					to the erase.
--
-- @description@ — The text to display.
--
-- ObjC selector: @- setDescription:@
setDescription :: (IsDREraseProgressPanel drEraseProgressPanel, IsNSString description) => drEraseProgressPanel -> description -> IO ()
setDescription drEraseProgressPanel  description =
withObjCPtr description $ \raw_description ->
    sendMsg drEraseProgressPanel (mkSelector "setDescription:") retVoid [argPtr (castPtr raw_description :: Ptr ())]

-- | description
--
-- Returns the description string displayed in the panel.
--
-- If no description is explicitly set, this method will return the standard					text string.
--
-- Returns: An NSString containing the text of the description.
--
-- ObjC selector: @- description@
description :: IsDREraseProgressPanel drEraseProgressPanel => drEraseProgressPanel -> IO (Id NSString)
description drEraseProgressPanel  =
  sendMsg drEraseProgressPanel (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @progressPanel@
progressPanelSelector :: Selector
progressPanelSelector = mkSelector "progressPanel"

-- | @Selector@ for @beginProgressSheetForErase:modalForWindow:@
beginProgressSheetForErase_modalForWindowSelector :: Selector
beginProgressSheetForErase_modalForWindowSelector = mkSelector "beginProgressSheetForErase:modalForWindow:"

-- | @Selector@ for @beginProgressPanelForErase:@
beginProgressPanelForEraseSelector :: Selector
beginProgressPanelForEraseSelector = mkSelector "beginProgressPanelForErase:"

-- | @Selector@ for @setDescription:@
setDescriptionSelector :: Selector
setDescriptionSelector = mkSelector "setDescription:"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

