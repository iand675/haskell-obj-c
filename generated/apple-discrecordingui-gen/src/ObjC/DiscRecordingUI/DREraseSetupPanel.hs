{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DREraseSetupPanel
--
-- Manages a panel that allows users to specify the					parameters of an erase.
--
-- This class supports choosing the 					device to use and what sort of erase to perform.
--
-- When the panel is closed by the user choosing to					erase the media in the device, the device is					exclusively held by the application for its own use					to prevent possible bad or corrupt media from					causing problem for the rest of the system. This					means that if the erase object obtained from the					panel is not used to do an erase, the device will					remain unavailable to other applications until the					exclusive access is released.
--
-- Generated bindings for @DREraseSetupPanel@.
module ObjC.DiscRecordingUI.DREraseSetupPanel
  ( DREraseSetupPanel
  , IsDREraseSetupPanel(..)
  , setupPanel
  , eraseObject
  , eraseType
  , eraseObjectSelector
  , eraseTypeSelector
  , setupPanelSelector


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
-- Returns an instance of a erase setup panel.
--
-- Returns: An erase setup panel.
--
-- ObjC selector: @+ setupPanel@
setupPanel :: IO (Id DREraseSetupPanel)
setupPanel  =
  do
    cls' <- getRequiredClass "DREraseSetupPanel"
    sendClassMessage cls' setupPanelSelector

-- | eraseObject
--
-- Creates and returns a new DRErase object that's configured to erase the disc in the currently selected device.
--
-- The new DRErase object is configured based on the settings in the setup panel				when the user clicks the OK button.
--
-- Do not invoke this method within a modal session (
--
-- //apple_ref/occ/instm/DRSetupPanel/runSetupPanel runSetupPanel
--
-- or
--
-- //apple_ref/occ/instm/DRSetupPanel/beginSetupSheetForWindow:modalDelegate:didEndSelector:contextInfo: beginSetupSheetForWindow:modalDelegate:didEndSelector:contextInfo:
--
-- )				because the erase object information is only updated just before the				modal session ends.
--
-- Returns: A new DRErase object.
--
-- ObjC selector: @- eraseObject@
eraseObject :: IsDREraseSetupPanel drEraseSetupPanel => drEraseSetupPanel -> IO (Id DRErase)
eraseObject drEraseSetupPanel =
  sendMessage drEraseSetupPanel eraseObjectSelector

-- | eraseType:
--
-- Invoked when the user clicks one of the panel's    					erase type radio buttons.
--
-- @sender@ — The object that invoked this method.
--
-- ObjC selector: @- eraseType:@
eraseType :: IsDREraseSetupPanel drEraseSetupPanel => drEraseSetupPanel -> RawId -> IO ()
eraseType drEraseSetupPanel sender =
  sendMessage drEraseSetupPanel eraseTypeSelector sender

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setupPanel@
setupPanelSelector :: Selector '[] (Id DREraseSetupPanel)
setupPanelSelector = mkSelector "setupPanel"

-- | @Selector@ for @eraseObject@
eraseObjectSelector :: Selector '[] (Id DRErase)
eraseObjectSelector = mkSelector "eraseObject"

-- | @Selector@ for @eraseType:@
eraseTypeSelector :: Selector '[RawId] ()
eraseTypeSelector = mkSelector "eraseType:"

