{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.DiscRecordingUI.NSObject
  ( NSObject
  , IsNSObject(..)
  , eraseProgressPanelWillBegin
  , eraseProgressPanelDidFinish
  , eraseProgressPanel_eraseDidFinish
  , burnProgressPanelWillBegin
  , burnProgressPanelDidFinish
  , burnProgressPanel_burnDidFinish
  , setupPanel_deviceCouldBeTarget
  , setupPanel_determineBestDeviceOfA_orB
  , setupPanelDeviceSelectionChanged
  , setupPanelShouldHandleMediaReservations
  , setupPanel_deviceContainsSuitableMedia_promptString
  , eraseProgressPanelWillBeginSelector
  , eraseProgressPanelDidFinishSelector
  , eraseProgressPanel_eraseDidFinishSelector
  , burnProgressPanelWillBeginSelector
  , burnProgressPanelDidFinishSelector
  , burnProgressPanel_burnDidFinishSelector
  , setupPanel_deviceCouldBeTargetSelector
  , setupPanel_determineBestDeviceOfA_orBSelector
  , setupPanelDeviceSelectionChangedSelector
  , setupPanelShouldHandleMediaReservationsSelector
  , setupPanel_deviceContainsSuitableMedia_promptStringSelector


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
import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | eraseProgressPanelWillBegin:
--
-- Notification sent by the panel before display.
--
-- If the delegate implements this method 					it will receive the message immediately before the panel is displayed.
--
-- @aNotification@ — Always
--
-- //apple_ref/occ/data/DREraseProgressPanelWillBeginNotification DREraseProgressPanelWillBeginNotification
--
-- . 					You can retrieve the DREraseProgressPanel object in question by sending
--
-- //apple_ref/occ/instm/NSNotification/object object
--
-- message to aNotification.
--
-- ObjC selector: @- eraseProgressPanelWillBegin:@
eraseProgressPanelWillBegin :: (IsNSObject nsObject, IsNSNotification aNotification) => nsObject -> aNotification -> IO ()
eraseProgressPanelWillBegin nsObject  aNotification =
withObjCPtr aNotification $ \raw_aNotification ->
    sendMsg nsObject (mkSelector "eraseProgressPanelWillBegin:") retVoid [argPtr (castPtr raw_aNotification :: Ptr ())]

-- | eraseProgressPanelDidFinish:
--
-- Notification sent by the panel after ordering out.
--
-- If the delegate implements this method 					it will receive the message after the panel is removed from display.
--
-- @aNotification@ — Always
--
-- //apple_ref/occ/data/DREraseProgressPanelDidFinishNotification DREraseProgressPanelDidFinishNotification
--
-- . 					You can retrieve the DREraseProgressPanel object in question by sending
--
-- //apple_ref/occ/instm/NSNotification/object object
--
-- message to aNotification.
--
-- ObjC selector: @- eraseProgressPanelDidFinish:@
eraseProgressPanelDidFinish :: (IsNSObject nsObject, IsNSNotification aNotification) => nsObject -> aNotification -> IO ()
eraseProgressPanelDidFinish nsObject  aNotification =
withObjCPtr aNotification $ \raw_aNotification ->
    sendMsg nsObject (mkSelector "eraseProgressPanelDidFinish:") retVoid [argPtr (castPtr raw_aNotification :: Ptr ())]

-- | eraseProgressPanel:eraseDidFinish:
--
-- Allows the delegate to handle the end-of-erase feedback.
--
-- This method allows the delegate to handle or modify the end-of-erase					feedback performed by the progress panel. Return YES to indicate the 					delegate handled the erase completion and the standard feedback should 					be supressed. If this method returns NO, the normal end-of-erase 					handling is performed (displaying an error if appropriate, playing an 					"I'm done" sound, etc).					The delegate is messaged before the progress panel is ordered out so 					a sheet may be displayed on a progress panel displayed as a window.
--
-- @theErasePanel@ — The progress panel
--
-- @erase@ — The object that performed the erase.
--
-- Returns: A BOOL indicating whether the normal end-of-erase feedback should occur.
--
-- ObjC selector: @- eraseProgressPanel:eraseDidFinish:@
eraseProgressPanel_eraseDidFinish :: (IsNSObject nsObject, IsDREraseProgressPanel theErasePanel, IsDRErase erase) => nsObject -> theErasePanel -> erase -> IO Bool
eraseProgressPanel_eraseDidFinish nsObject  theErasePanel erase =
withObjCPtr theErasePanel $ \raw_theErasePanel ->
  withObjCPtr erase $ \raw_erase ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "eraseProgressPanel:eraseDidFinish:") retCULong [argPtr (castPtr raw_theErasePanel :: Ptr ()), argPtr (castPtr raw_erase :: Ptr ())]

-- | burnProgressPanelWillBegin:
--
-- Notification sent by the panel before display.
--
-- If the delegate implements this method 					it will receive the message immediately before the panel is displayed.
--
-- @aNotification@ — Always
--
-- //apple_ref/occ/data/DRBurnProgressPanelWillBeginNotification DRBurnProgressPanelWillBeginNotification
--
-- . 					You can retrieve the DRBurnProgressPanel object in question by sending
--
-- //apple_ref/occ/instm/NSNotification/object object
--
-- message to aNotification.
--
-- ObjC selector: @- burnProgressPanelWillBegin:@
burnProgressPanelWillBegin :: (IsNSObject nsObject, IsNSNotification aNotification) => nsObject -> aNotification -> IO ()
burnProgressPanelWillBegin nsObject  aNotification =
withObjCPtr aNotification $ \raw_aNotification ->
    sendMsg nsObject (mkSelector "burnProgressPanelWillBegin:") retVoid [argPtr (castPtr raw_aNotification :: Ptr ())]

-- | burnProgressPanelDidFinish:
--
-- Notification sent by the panel after ordering out.
--
-- If the delegate implements this method 					it will receive the message after the panel is removed from display.
--
-- @aNotification@ — Always
--
-- //apple_ref/occ/data/DRBurnProgressPanelDidFinishNotification DRBurnProgressPanelDidFinishNotification
--
-- . 					You can retrieve the DRBurnProgressPanel object in question by sending
--
-- //apple_ref/occ/instm/NSNotification/object object
--
-- message to aNotification.
--
-- ObjC selector: @- burnProgressPanelDidFinish:@
burnProgressPanelDidFinish :: (IsNSObject nsObject, IsNSNotification aNotification) => nsObject -> aNotification -> IO ()
burnProgressPanelDidFinish nsObject  aNotification =
withObjCPtr aNotification $ \raw_aNotification ->
    sendMsg nsObject (mkSelector "burnProgressPanelDidFinish:") retVoid [argPtr (castPtr raw_aNotification :: Ptr ())]

-- | burnProgressPanel:burnDidFinish:
--
-- Allows the delegate to handle the end-of-burn feedback.
--
-- This method allows the delegate to handle or modify the end-of-burn					feedback performed by the progress panel. Return YES to indicate the 					delegate handled the burn completion and the standard feedback should 					be supressed. If this method returns NO, the normal end-of-burn 					handling is performed (displaying an error if appropriate, playing an 					"I'm done" sound, etc).					The delegate is messaged before the progress panel is ordered out so 					a sheet may be displayed on a progress panel displayed as a window.
--
-- @theBurnPanel@ — The progress panel
--
-- @burn@ — The object that performed the burn.
--
-- Returns: A BOOL indicating whether the normal end-of-burn feedback should occur.
--
-- ObjC selector: @- burnProgressPanel:burnDidFinish:@
burnProgressPanel_burnDidFinish :: (IsNSObject nsObject, IsDRBurnProgressPanel theBurnPanel, IsDRBurn burn) => nsObject -> theBurnPanel -> burn -> IO Bool
burnProgressPanel_burnDidFinish nsObject  theBurnPanel burn =
withObjCPtr theBurnPanel $ \raw_theBurnPanel ->
  withObjCPtr burn $ \raw_burn ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "burnProgressPanel:burnDidFinish:") retCULong [argPtr (castPtr raw_theBurnPanel :: Ptr ()), argPtr (castPtr raw_burn :: Ptr ())]

-- | setupPanel:deviceCouldBeTarget:
--
-- Allows the delegate to determine if device can be used as a target.
--
-- This method is used to limit the menu to only those devices that you want				to appear.  For example, a DVD burning application might use this				to limit the menu to only devices that are capable of writing DVD-Rs.
--
-- @aPanel@ — The panel.
--
-- @device@ — The candidate device.
--
-- Returns: YES if the device is acceptable, NO if not.
--
-- ObjC selector: @- setupPanel:deviceCouldBeTarget:@
setupPanel_deviceCouldBeTarget :: (IsNSObject nsObject, IsDRSetupPanel aPanel, IsDRDevice device) => nsObject -> aPanel -> device -> IO Bool
setupPanel_deviceCouldBeTarget nsObject  aPanel device =
withObjCPtr aPanel $ \raw_aPanel ->
  withObjCPtr device $ \raw_device ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "setupPanel:deviceCouldBeTarget:") retCULong [argPtr (castPtr raw_aPanel :: Ptr ()), argPtr (castPtr raw_device :: Ptr ())]

-- | setupPanel:determineBestDeviceOfA:orB:
--
-- Allows the delegate to specify which device is its preferred.
--
-- When the setup panel is first displayed and again, 				each time a new device appears, the setup panel will ask the delegate 				to compare two devices to determine which is most suitable for their 				content to burn.
--
-- @aPanel@ — The panel.
--
-- @deviceA@ — A candidate device. May be nil.
--
-- @deviceA@ — A candidate device. May be nil.
--
-- Returns: One of the two device objects passed in.
--
-- ObjC selector: @- setupPanel:determineBestDeviceOfA:orB:@
setupPanel_determineBestDeviceOfA_orB :: (IsNSObject nsObject, IsDRSetupPanel aPanel, IsDRDevice deviceA, IsDRDevice device) => nsObject -> aPanel -> deviceA -> device -> IO (Id DRDevice)
setupPanel_determineBestDeviceOfA_orB nsObject  aPanel deviceA device =
withObjCPtr aPanel $ \raw_aPanel ->
  withObjCPtr deviceA $ \raw_deviceA ->
    withObjCPtr device $ \raw_device ->
        sendMsg nsObject (mkSelector "setupPanel:determineBestDeviceOfA:orB:") (retPtr retVoid) [argPtr (castPtr raw_aPanel :: Ptr ()), argPtr (castPtr raw_deviceA :: Ptr ()), argPtr (castPtr raw_device :: Ptr ())] >>= retainedObject . castPtr

-- | setupPanelDeviceSelectionChanged:
--
-- Sent by the default notification center when the device selection in the				panel has changed.
--
-- @aNotification@ — Notification object. This is always
--
-- DRSetupPanelDeviceSelectionChangedNotification DRSetupPanelDeviceSelectionChangedNotification
--
-- . You can 								retrieve the DRSetupPanel object in question by sending
--
-- //apple_ref/occ/instm/NSNotification/object object
--
-- to aNotification. 								The userInfo dictionary contains the single key DRSetupPanelSelectedDeviceKey whose								value is the
--
-- //apple_ref/occ/cl/DRDevice DRDevice
--
-- object that is currently selected.
--
-- ObjC selector: @- setupPanelDeviceSelectionChanged:@
setupPanelDeviceSelectionChanged :: (IsNSObject nsObject, IsNSNotification aNotification) => nsObject -> aNotification -> IO ()
setupPanelDeviceSelectionChanged nsObject  aNotification =
withObjCPtr aNotification $ \raw_aNotification ->
    sendMsg nsObject (mkSelector "setupPanelDeviceSelectionChanged:") retVoid [argPtr (castPtr raw_aNotification :: Ptr ())]

-- | setupPanelShouldHandleMediaReservations:
--
-- This delegate method allows the delegate to control how media reservations are handled.
--
-- @aPanel@ — The setup panel sending the message.
--
-- Returns: Return NO to indicate the delegate will handle media reservations. Return YES to				indicate the setupPanel should handle media reservations itself.
--
-- ObjC selector: @- setupPanelShouldHandleMediaReservations:@
setupPanelShouldHandleMediaReservations :: (IsNSObject nsObject, IsDRSetupPanel aPanel) => nsObject -> aPanel -> IO Bool
setupPanelShouldHandleMediaReservations nsObject  aPanel =
withObjCPtr aPanel $ \raw_aPanel ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "setupPanelShouldHandleMediaReservations:") retCULong [argPtr (castPtr raw_aPanel :: Ptr ())]

-- | setupPanel:deviceContainsSuitableMedia:promptString:
--
-- This delegate method allows the delegate to determine if the media inserted in the 				device is suitable for whatever operation is to be performed.
--
-- @aPanel@ — The setup panel sending the message.
--
-- @device@ — The device that contains the media being asked about.
--
-- @prompt@ — A pointer to storage for an NSString. Pass back an NSString object describing 						the media state.
--
-- Returns: Return NO to disable the default button.
--
-- ObjC selector: @- setupPanel:deviceContainsSuitableMedia:promptString:@
setupPanel_deviceContainsSuitableMedia_promptString :: (IsNSObject nsObject, IsDRSetupPanel aPanel, IsDRDevice device, IsNSString prompt) => nsObject -> aPanel -> device -> prompt -> IO Bool
setupPanel_deviceContainsSuitableMedia_promptString nsObject  aPanel device prompt =
withObjCPtr aPanel $ \raw_aPanel ->
  withObjCPtr device $ \raw_device ->
    withObjCPtr prompt $ \raw_prompt ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "setupPanel:deviceContainsSuitableMedia:promptString:") retCULong [argPtr (castPtr raw_aPanel :: Ptr ()), argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_prompt :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @eraseProgressPanelWillBegin:@
eraseProgressPanelWillBeginSelector :: Selector
eraseProgressPanelWillBeginSelector = mkSelector "eraseProgressPanelWillBegin:"

-- | @Selector@ for @eraseProgressPanelDidFinish:@
eraseProgressPanelDidFinishSelector :: Selector
eraseProgressPanelDidFinishSelector = mkSelector "eraseProgressPanelDidFinish:"

-- | @Selector@ for @eraseProgressPanel:eraseDidFinish:@
eraseProgressPanel_eraseDidFinishSelector :: Selector
eraseProgressPanel_eraseDidFinishSelector = mkSelector "eraseProgressPanel:eraseDidFinish:"

-- | @Selector@ for @burnProgressPanelWillBegin:@
burnProgressPanelWillBeginSelector :: Selector
burnProgressPanelWillBeginSelector = mkSelector "burnProgressPanelWillBegin:"

-- | @Selector@ for @burnProgressPanelDidFinish:@
burnProgressPanelDidFinishSelector :: Selector
burnProgressPanelDidFinishSelector = mkSelector "burnProgressPanelDidFinish:"

-- | @Selector@ for @burnProgressPanel:burnDidFinish:@
burnProgressPanel_burnDidFinishSelector :: Selector
burnProgressPanel_burnDidFinishSelector = mkSelector "burnProgressPanel:burnDidFinish:"

-- | @Selector@ for @setupPanel:deviceCouldBeTarget:@
setupPanel_deviceCouldBeTargetSelector :: Selector
setupPanel_deviceCouldBeTargetSelector = mkSelector "setupPanel:deviceCouldBeTarget:"

-- | @Selector@ for @setupPanel:determineBestDeviceOfA:orB:@
setupPanel_determineBestDeviceOfA_orBSelector :: Selector
setupPanel_determineBestDeviceOfA_orBSelector = mkSelector "setupPanel:determineBestDeviceOfA:orB:"

-- | @Selector@ for @setupPanelDeviceSelectionChanged:@
setupPanelDeviceSelectionChangedSelector :: Selector
setupPanelDeviceSelectionChangedSelector = mkSelector "setupPanelDeviceSelectionChanged:"

-- | @Selector@ for @setupPanelShouldHandleMediaReservations:@
setupPanelShouldHandleMediaReservationsSelector :: Selector
setupPanelShouldHandleMediaReservationsSelector = mkSelector "setupPanelShouldHandleMediaReservations:"

-- | @Selector@ for @setupPanel:deviceContainsSuitableMedia:promptString:@
setupPanel_deviceContainsSuitableMedia_promptStringSelector :: Selector
setupPanel_deviceContainsSuitableMedia_promptStringSelector = mkSelector "setupPanel:deviceContainsSuitableMedia:promptString:"

