{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKFilterBrowserPanel
--
-- The IKFilterBrowserPanel provides the shared IKFilterBrowser with its runtime model.
--
-- See information in the introduction.
--
-- Generated bindings for @IKFilterBrowserPanel@.
module ObjC.Quartz.IKFilterBrowserPanel
  ( IKFilterBrowserPanel
  , IsIKFilterBrowserPanel(..)
  , filterBrowserPanelWithStyleMask
  , filterName
  , beginWithOptions_modelessDelegate_didEndSelector_contextInfo
  , beginSheetWithOptions_modalForWindow_modalDelegate_didEndSelector_contextInfo
  , runModalWithOptions
  , filterBrowserViewWithOptions
  , finish
  , beginSheetWithOptions_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , beginWithOptions_modelessDelegate_didEndSelector_contextInfoSelector
  , filterBrowserPanelWithStyleMaskSelector
  , filterBrowserViewWithOptionsSelector
  , filterNameSelector
  , finishSelector
  , runModalWithOptionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | filterBrowserPanelWithStyleMask:
--
-- Create a shared instance of the IKFilterBrowser
--
-- Use this method to create a shared instance of the IKFilterBrowser with a specific NSWindow style. Right now it only supports selecting of deselecting the NSTexturedBackgroundWindowMask style bit.
--
-- ObjC selector: @+ filterBrowserPanelWithStyleMask:@
filterBrowserPanelWithStyleMask :: CUInt -> IO RawId
filterBrowserPanelWithStyleMask styleMask =
  do
    cls' <- getRequiredClass "IKFilterBrowserPanel"
    sendClassMessage cls' filterBrowserPanelWithStyleMaskSelector styleMask

-- | filterName
--
-- Returns the name of the currently selected filter.
--
-- Use this method in response to a IKFilterBrowserFilterSelectedNotification or IKFilterBrowserFilterDoubleClickNotification or afer returning from a modal session.
--
-- ObjC selector: @- filterName@
filterName :: IsIKFilterBrowserPanel ikFilterBrowserPanel => ikFilterBrowserPanel -> IO (Id NSString)
filterName ikFilterBrowserPanel =
  sendMessage ikFilterBrowserPanel filterNameSelector

-- | beginWithOptions:modelessDelegate:didEndSelector:contextInfo:
--
-- Displays the FilterBrowser in a new window unless it is already open.
--
-- Use this method to open the IKFilterBrowser in a seperate window, much like a panel. When the panel operation is ended, didEndSelector is invoked on the modelessDelegate, passing contextInfo as an argument.			didEndSelector should have the following signature:
--
-- - (void)openPanelDidEnd:(NSOpenPanel *)panel returnCode:(int)returnCode  contextInfo:(void  *)contextInfo
--
-- The value passed as returnCode will be either NSCancelButton or NSOKButton.
--
-- @inOptions@ — A dictionary describing the desired UI configuration for the IKFilterBrowser
--
-- @modelessDelegate@ — See discussion below
--
-- @didEndSelector@ — See discussion below
--
-- @contextInfo@ — See discussion below
--
-- ObjC selector: @- beginWithOptions:modelessDelegate:didEndSelector:contextInfo:@
beginWithOptions_modelessDelegate_didEndSelector_contextInfo :: (IsIKFilterBrowserPanel ikFilterBrowserPanel, IsNSDictionary inOptions) => ikFilterBrowserPanel -> inOptions -> RawId -> Sel -> Ptr () -> IO ()
beginWithOptions_modelessDelegate_didEndSelector_contextInfo ikFilterBrowserPanel inOptions modelessDelegate didEndSelector contextInfo =
  sendMessage ikFilterBrowserPanel beginWithOptions_modelessDelegate_didEndSelector_contextInfoSelector (toNSDictionary inOptions) modelessDelegate didEndSelector contextInfo

-- | beginSheetWithOptions:modalForWindow:modalDelegate:didEndSelector:contextInfo:
--
-- Displays the FilterBrowser in a sheet attached to the passed in window.
--
-- Use this method to open the IKFilterBrowser in a sheet attached to a window. When the sheet operation is ended, didEndSelector is invoked on the modalDelegate, passing contextInfo as an argument. 			didEndSelector should have the following signature:
--
-- - (void)openPanelDidEnd:(NSOpenPanel *)panel returnCode:(int)returnCode  contextInfo:(void  *)contextInfo
--
-- The value passed as returnCode will be either NSCancelButton or NSOKButton.
--
-- @inOptions@ — A dictionary describing the desired UI configuration for the IKFilterBrowser
--
-- @modalForWindow@ — The window to which the sheet should be attached to.
--
-- @modalDelegate@ — See discussion below
--
-- @didEndSelector@ — See discussion below
--
-- @contextInfo@ — See discussion below
--
-- ObjC selector: @- beginSheetWithOptions:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetWithOptions_modalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsIKFilterBrowserPanel ikFilterBrowserPanel, IsNSDictionary inOptions, IsNSWindow docWindow) => ikFilterBrowserPanel -> inOptions -> docWindow -> RawId -> Sel -> Ptr () -> IO ()
beginSheetWithOptions_modalForWindow_modalDelegate_didEndSelector_contextInfo ikFilterBrowserPanel inOptions docWindow modalDelegate didEndSelector contextInfo =
  sendMessage ikFilterBrowserPanel beginSheetWithOptions_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector (toNSDictionary inOptions) (toNSWindow docWindow) modalDelegate didEndSelector contextInfo

-- | runModalWithOptions
--
-- Displays the FilterBrowser in a modal dialog.
--
-- Use this method to run the IKFilterBrowser in a modal dialog. The value passed as returnCode will be either NSCancelButton or NSOKButton.
--
-- @inOptions@ — A dictionary describing the desired UI configuration for the IKFilterBrowser
--
-- ObjC selector: @- runModalWithOptions:@
runModalWithOptions :: (IsIKFilterBrowserPanel ikFilterBrowserPanel, IsNSDictionary inOptions) => ikFilterBrowserPanel -> inOptions -> IO CInt
runModalWithOptions ikFilterBrowserPanel inOptions =
  sendMessage ikFilterBrowserPanel runModalWithOptionsSelector (toNSDictionary inOptions)

-- | filterBrowserViewWithOptions
--
-- Returns a view containing the FilterBrowser.
--
-- Use this method to run the IKFilterBrowser in your own UI. To dismiss it, invoke the finish action as described below.
--
-- @inOptions@ — A dictionary describing the desired UI configuration for the IKFilterBrowser
--
-- ObjC selector: @- filterBrowserViewWithOptions:@
filterBrowserViewWithOptions :: (IsIKFilterBrowserPanel ikFilterBrowserPanel, IsNSDictionary inOptions) => ikFilterBrowserPanel -> inOptions -> IO (Id IKFilterBrowserView)
filterBrowserViewWithOptions ikFilterBrowserPanel inOptions =
  sendMessage ikFilterBrowserPanel filterBrowserViewWithOptionsSelector (toNSDictionary inOptions)

-- | finish
--
-- Closes the IKFilterBrowser.
--
-- Invoke this action for instance from your OK or Cancel button when you are running the IKFilterBrowserView modal in your own UI.
--
-- ObjC selector: @- finish:@
finish :: IsIKFilterBrowserPanel ikFilterBrowserPanel => ikFilterBrowserPanel -> RawId -> IO ()
finish ikFilterBrowserPanel sender =
  sendMessage ikFilterBrowserPanel finishSelector sender

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterBrowserPanelWithStyleMask:@
filterBrowserPanelWithStyleMaskSelector :: Selector '[CUInt] RawId
filterBrowserPanelWithStyleMaskSelector = mkSelector "filterBrowserPanelWithStyleMask:"

-- | @Selector@ for @filterName@
filterNameSelector :: Selector '[] (Id NSString)
filterNameSelector = mkSelector "filterName"

-- | @Selector@ for @beginWithOptions:modelessDelegate:didEndSelector:contextInfo:@
beginWithOptions_modelessDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSDictionary, RawId, Sel, Ptr ()] ()
beginWithOptions_modelessDelegate_didEndSelector_contextInfoSelector = mkSelector "beginWithOptions:modelessDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @beginSheetWithOptions:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetWithOptions_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSDictionary, Id NSWindow, RawId, Sel, Ptr ()] ()
beginSheetWithOptions_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetWithOptions:modalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @runModalWithOptions:@
runModalWithOptionsSelector :: Selector '[Id NSDictionary] CInt
runModalWithOptionsSelector = mkSelector "runModalWithOptions:"

-- | @Selector@ for @filterBrowserViewWithOptions:@
filterBrowserViewWithOptionsSelector :: Selector '[Id NSDictionary] (Id IKFilterBrowserView)
filterBrowserViewWithOptionsSelector = mkSelector "filterBrowserViewWithOptions:"

-- | @Selector@ for @finish:@
finishSelector :: Selector '[RawId] ()
finishSelector = mkSelector "finish:"

