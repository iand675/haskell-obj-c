{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DRSetupPanel
--
-- This class is the base class for setup panels in the DiscRecordingUI				framework. It provides a base framework for handling device				selection, media ejection and confirming or cancelling the panel.
--
-- Generated bindings for @DRSetupPanel@.
module ObjC.DiscRecordingUI.DRSetupPanel
  ( DRSetupPanel
  , IsDRSetupPanel(..)
  , initWithNibName
  , runSetupPanel
  , beginSetupSheetForWindow_modalDelegate_didEndSelector_contextInfo
  , ok
  , cancel
  , eject
  , open
  , close
  , deviceSelectionChanged
  , mediaStateChanged
  , setupForDisplay
  , initWithNibNameSelector
  , runSetupPanelSelector
  , beginSetupSheetForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , okSelector
  , cancelSelector
  , ejectSelector
  , openSelector
  , closeSelector
  , deviceSelectionChangedSelector
  , mediaStateChangedSelector
  , setupForDisplaySelector


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

-- | initWithNibName:
--
-- Initializes the receiver to use the panel from the nibName nib file.
--
-- @nibName@ — Nib filename.
--
-- Returns: The receiver.
--
-- ObjC selector: @- initWithNibName:@
initWithNibName :: (IsDRSetupPanel drSetupPanel, IsNSString nibName) => drSetupPanel -> nibName -> IO RawId
initWithNibName drSetupPanel  nibName =
withObjCPtr nibName $ \raw_nibName ->
    fmap (RawId . castPtr) $ sendMsg drSetupPanel (mkSelector "initWithNibName:") (retPtr retVoid) [argPtr (castPtr raw_nibName :: Ptr ())]

-- | runSetupPanel
--
-- Displays the receiver and begins its event loop.
--
-- Invokes NSApplication's
--
-- //apple_ref/occ/instm/NSApplication/runModalForWindow: runModalForWindow:
--
-- method with self as the argument.
--
-- Returns: Returns
--
-- //apple_ref/c/econst/NSOKButton NSOKButton
--
-- (if the user clicks the default button) or
--
-- //apple_ref/c/econst/NSCancelButton NSCancelButton
--
-- (if the user clicks the Cancel button).
--
-- ObjC selector: @- runSetupPanel@
runSetupPanel :: IsDRSetupPanel drSetupPanel => drSetupPanel -> IO CLong
runSetupPanel drSetupPanel  =
  sendMsg drSetupPanel (mkSelector "runSetupPanel") retCLong []

-- | beginSetupSheetForWindow:modalDelegate:didEndSelector:contextInfo:
--
-- Presents a setup panel as a sheet.
--
-- @owner@ — The window the sheet will be attached to. If owner is not nil, the setup									panel slides down as a sheet running as a document modal									window. If owner is nil, this is an error.
--
-- @modalDelegate@ — The modal delegate. The object that implements the didEndSelector.
--
-- @didEndSelector@ — Selector to invoke when the sheet ends. This selector is optional.									If implemented by the modal delegate, this method is invoked after 									the modal session has ended, but before dismissing the same panel. 									didEndSelector may dismiss the save panel itself; otherwise it will 									be dismissed on return from the method. didEndSelector should have 									the following signature:
--
-- - (void)setupPanelDidEnd:(DRSetupPanel*)panel returnCode:(int)returnCode contextInfo:(void*)contextInfo;
--
-- @contextInfo@ — Context information to be passed when the selector named by didEndSelector									is invoked.
--
-- ObjC selector: @- beginSetupSheetForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSetupSheetForWindow_modalDelegate_didEndSelector_contextInfo :: (IsDRSetupPanel drSetupPanel, IsNSWindow owner) => drSetupPanel -> owner -> RawId -> Selector -> Ptr () -> IO ()
beginSetupSheetForWindow_modalDelegate_didEndSelector_contextInfo drSetupPanel  owner modalDelegate didEndSelector contextInfo =
withObjCPtr owner $ \raw_owner ->
    sendMsg drSetupPanel (mkSelector "beginSetupSheetForWindow:modalDelegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_owner :: Ptr ()), argPtr (castPtr (unRawId modalDelegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | ok:
--
-- Invoked when the user clicks the panel's default button.
--
-- @sender@ — The object that invoked this method.
--
-- ObjC selector: @- ok:@
ok :: IsDRSetupPanel drSetupPanel => drSetupPanel -> RawId -> IO ()
ok drSetupPanel  sender =
  sendMsg drSetupPanel (mkSelector "ok:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | cancel:
--
-- Invoked when the user clicks the panel's cancel button.
--
-- @sender@ — The object that invoked this method.
--
-- ObjC selector: @- cancel:@
cancel :: IsDRSetupPanel drSetupPanel => drSetupPanel -> RawId -> IO ()
cancel drSetupPanel  sender =
  sendMsg drSetupPanel (mkSelector "cancel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | eject:
--
-- Invoked when the user clicks the panel's eject button.
--
-- @sender@ — The object that invoked this method.
--
-- ObjC selector: @- eject:@
eject :: IsDRSetupPanel drSetupPanel => drSetupPanel -> RawId -> IO ()
eject drSetupPanel  sender =
  sendMsg drSetupPanel (mkSelector "eject:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | open:
--
-- Invoked when the user clicks the panel's open button.
--
-- @sender@ — The object that invoked this method.
--
-- ObjC selector: @- open:@
open :: IsDRSetupPanel drSetupPanel => drSetupPanel -> RawId -> IO ()
open drSetupPanel  sender =
  sendMsg drSetupPanel (mkSelector "open:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | close:
--
-- Invoked when the user clicks the panel's close button.
--
-- @sender@ — The object that invoked this method.
--
-- ObjC selector: @- close:@
close :: IsDRSetupPanel drSetupPanel => drSetupPanel -> RawId -> IO ()
close drSetupPanel  sender =
  sendMsg drSetupPanel (mkSelector "close:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | deviceSelectionChanged:
--
-- Invoked when the user changes the device selected in the device popup.
--
-- If the device currently selected is disconnected from the machine, the device 				popup will remove the device from itself and select a new device. This will act 				as if the user changed the device selected. Because of this, device may be nil				if no eligible devices are currently connected to the machine.
--
-- @device@ — The newly selected device, or nil.
--
-- ObjC selector: @- deviceSelectionChanged:@
deviceSelectionChanged :: (IsDRSetupPanel drSetupPanel, IsDRDevice device) => drSetupPanel -> device -> IO ()
deviceSelectionChanged drSetupPanel  device =
withObjCPtr device $ \raw_device ->
    sendMsg drSetupPanel (mkSelector "deviceSelectionChanged:") retVoid [argPtr (castPtr raw_device :: Ptr ())]

-- | mediaStateChanged:
--
-- Invoked when the media state of the currently selected device changes. 				This can include media being ejected, inserted, being used by another				application, etc.
--
-- @status@ — The new device status dictionary.
--
-- Returns: YES if the inserted media is valid for use, NO otherwise.
--
-- ObjC selector: @- mediaStateChanged:@
mediaStateChanged :: (IsDRSetupPanel drSetupPanel, IsNSDictionary status) => drSetupPanel -> status -> IO Bool
mediaStateChanged drSetupPanel  status =
withObjCPtr status $ \raw_status ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg drSetupPanel (mkSelector "mediaStateChanged:") retCULong [argPtr (castPtr raw_status :: Ptr ())]

-- | setupForDisplay
--
-- This method is called immediately before panel is displayed on				the screen. Any setup to be done in preparation for display should be				done here.
--
-- ObjC selector: @- setupForDisplay@
setupForDisplay :: IsDRSetupPanel drSetupPanel => drSetupPanel -> IO ()
setupForDisplay drSetupPanel  =
  sendMsg drSetupPanel (mkSelector "setupForDisplay") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNibName:@
initWithNibNameSelector :: Selector
initWithNibNameSelector = mkSelector "initWithNibName:"

-- | @Selector@ for @runSetupPanel@
runSetupPanelSelector :: Selector
runSetupPanelSelector = mkSelector "runSetupPanel"

-- | @Selector@ for @beginSetupSheetForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSetupSheetForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector
beginSetupSheetForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSetupSheetForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @ok:@
okSelector :: Selector
okSelector = mkSelector "ok:"

-- | @Selector@ for @cancel:@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel:"

-- | @Selector@ for @eject:@
ejectSelector :: Selector
ejectSelector = mkSelector "eject:"

-- | @Selector@ for @open:@
openSelector :: Selector
openSelector = mkSelector "open:"

-- | @Selector@ for @close:@
closeSelector :: Selector
closeSelector = mkSelector "close:"

-- | @Selector@ for @deviceSelectionChanged:@
deviceSelectionChangedSelector :: Selector
deviceSelectionChangedSelector = mkSelector "deviceSelectionChanged:"

-- | @Selector@ for @mediaStateChanged:@
mediaStateChangedSelector :: Selector
mediaStateChangedSelector = mkSelector "mediaStateChanged:"

-- | @Selector@ for @setupForDisplay@
setupForDisplaySelector :: Selector
setupForDisplaySelector = mkSelector "setupForDisplay"

